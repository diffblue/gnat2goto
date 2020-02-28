with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Aspects;                 use Aspects;
with Elists;                  use Elists;
with Nlists;                  use Nlists;
with Stringt;                 use Stringt;
with Sinput;                  use Sinput;
with Namet;                   use Namet;
with Tree_Walk;               use Tree_Walk;
with Einfo;                   use Einfo;
with Sem_Prag;                use Sem_Prag;
with Symbol_Table_Info;       use Symbol_Table_Info;
with GOTO_Utils;              use GOTO_Utils;
with Range_Check;             use Range_Check;
with Follow;                  use Follow;
with Binary_To_Hex;           use Binary_To_Hex;
with Uintp;                   use Uintp;
with Ada.Text_IO;             use Ada.Text_IO;
with Treepr;                  use Treepr;
--  with System;
package body ASVAT_Modelling is

   Print_Message : constant Boolean := True;

   function Do_Nondet_Var (Var_Name, Var_Type : String;
                           E : Entity_Id) return Irep;
   --  Nondets the given variable.

   function Do_Var_In_Type (Var_Name, Var_Type  : String;
                            Var_Irep, Type_Irep : Irep;
                            E : Entity_Id) return Irep;
   --  Marks as in type the given discrete variable.

   function Do_Parameterless_Function_Call
     (Fun_Name : String; E : Entity_Id) return Irep;

   function Find_Model (Model : String) return Model_Sorts;

   function Get_Actual_Obj_Name (Obj : Entity_Id;
                                 Replace_Object : Boolean) return String;

   function Get_Actual_Type (Obj : Entity_Id;
                             Replace_Object : Boolean) return String;

   procedure Make_Memcpy_Procedure (E : Entity_Id);

   procedure Make_Selector_Names (Root : String;
                                  Root_Irep : Irep;
                                  Block : Irep;
                                  Root_Type : Node_Id;
                                  E : Entity_Id;
                                  Loc : Irep);
   --  A provisional subprogram which recurses any non-discriminated record
   --  and marks its discrete components in type.
   --  The procedure will be replaced with a more general one which
   --  handles discriminated records and arrays.

   procedure Print_Modelling_Message (Mess : String; Loc : Source_Ptr);

   function Replace_Dots (S : String) return String;

   function Replace_Local_With_Non_Visible
     (Is_Type : Boolean; E : Entity_Id) return String
   with Pre => Ekind (E) in E_Variable | E_Constant and then
     Get_Model_Sort (E) = Represents;

   -------------------
   -- Do_Nondet_Var --
   -------------------

   function Do_Nondet_Var (Var_Name, Var_Type : String;
                           E : Entity_Id) return Irep is
      Source_Location : constant Irep := Get_Source_Location (E);
      Var_Symbol_Id : constant Symbol_Id := Intern (Var_Name);
      pragma Assert (Global_Symbol_Table.Contains (Var_Symbol_Id),
                     "Do_Nondet_Var: Variable name is not in symbol table");
      Var_Symbol : constant Symbol := Global_Symbol_Table (Var_Symbol_Id);
      Fun_Name : constant String := "nondet___" & Var_Type;
      Fun_Symbol_Id : constant Symbol_Id := Intern (Fun_Name);
   begin
      --  First a nondet function is required to assign to the variable.
      --  One for the type may already exist.
      if not Global_Symbol_Table.Contains (Fun_Symbol_Id) then
         Make_Nondet_Function (Fun_Name    => Fun_Name,
                               Result_Type => Var_Type,
                               Statements  => Ireps.Empty,
                               E           => E);
      end if;

      --  Now the nondet function is declared, the LHS and RHS of the
      --  assignment can be declared.
      declare
         LHS : constant Irep :=
           Make_Symbol_Expr
             (Source_Location => Source_Location,
              Identifier      => Var_Name,
              I_Type          => Var_Symbol.SymType);

         RHS : constant Irep :=
           Do_Parameterless_Function_Call
             (Fun_Name => Fun_Name,
              E        => E);
      begin
         Print_Modelling_Message ("Assign " &
                                    Var_Name & " := " & Fun_Name,
                                  Sloc (E));
         return
           Make_Code_Assign
             (Lhs => LHS,
              Rhs => RHS,
              Source_Location => Source_Location);
      end;
   end Do_Nondet_Var;

   --------------------
   -- Do_Var_In_Type --
   --------------------

   function Do_Var_In_Type (Var_Name, Var_Type  : String;
                            Var_Irep, Type_Irep : Irep;
                            E                   : Entity_Id) return Irep is
      Source_Location : constant Irep := Get_Source_Location (E);
      Followed_Type : constant Irep :=
        Follow_Symbol_Type (Type_Irep, Global_Symbol_Table);
      Resolved_Var : constant Irep :=
        Cast_Enum (Var_Irep, Global_Symbol_Table);
   begin
      if Kind (Followed_Type) in
        I_Bounded_Unsignedbv_Type | I_Bounded_Signedbv_Type
          | I_Bounded_Floatbv_Type | I_Unsignedbv_Type | I_Signedbv_Type
            | I_Floatbv_Type | I_C_Enum_Type
      then
         declare
            Resolved_Type : constant Irep :=
              (if Kind (Followed_Type) = I_C_Enum_Type then
                    Get_Subtype (Followed_Type)
               else
                  Followed_Type);

            Var_First : constant Irep :=
              Cast_Enum (Get_Bound (E, Resolved_Type, Bound_Low),
                         Global_Symbol_Table);
            Var_Last  : constant Irep :=
              Cast_Enum (Get_Bound (E, Resolved_Type, Bound_High),
                        Global_Symbol_Table);

            Geq_Var_First : constant Irep :=
              Make_Op_Geq
                (Rhs =>
                   Typecast_If_Necessary
                     (Var_First,
                      Get_Type (Resolved_Var),
                      Global_Symbol_Table),
                 Lhs             => Resolved_Var,
                 Source_Location => Source_Location,
                 Overflow_Check  => False,
                 I_Type          => Make_Bool_Type,
                 Range_Check     => False);
            Leq_Var_Last : constant Irep :=
              Make_Op_Leq
                (Rhs             =>
                   Typecast_If_Necessary
                     (Var_Last,
                      Get_Type (Resolved_Var),
                      Global_Symbol_Table),
                 Lhs             => Resolved_Var,
                 Source_Location => Source_Location,
                 Overflow_Check  => False,
                 I_Type          => Make_Bool_Type,
                 Range_Check     => False);
            Var_In_Type_Cond : constant Irep :=
              Make_Op_And
                (Source_Location => Source_Location,
                 I_Type          => CProver_Bool_T,
                 Range_Check     => False);
         begin
            --  Make the and condition:
            --  Var_Irep >= Var_First and Var_Irep <= Var_Last
            Append_Op (Var_In_Type_Cond, Geq_Var_First);
            Append_Op (Var_In_Type_Cond, Leq_Var_Last);

            Print_Modelling_Message
              ("Assume " &
                 Var_Name & " >= " & Var_Type & "'First and " &
                 Var_Name & " <= " & Var_Type & "'Last",
               Sloc (E));
            return
              Make_Assume_Call (Assumption     => Var_In_Type_Cond,
                                Source_Loc     => Source_Location,
                                A_Symbol_Table => Global_Symbol_Table);
         end;
      else
         return Report_Unhandled_Node_Irep
           (E,
            "Do_Var_In_Type",
            "Enumeration object not supported");
      end if;

   end Do_Var_In_Type;

   ------------------------------------
   -- Do_Parameterless_Function_Call --
   ------------------------------------

   function Do_Parameterless_Function_Call
     (Fun_Name : String; E : Entity_Id) return Irep
   is
      Fun_Id     : constant Symbol_Id := Intern (Fun_Name);
      pragma Assert (Global_Symbol_Table.Contains (Fun_Id),
                     "gnat2goto fatal error: Nondet_Function_Call " &
                    Fun_Name & " not in symbol table.");
      Fun_Symbol : constant Symbol    := Global_Symbol_Table (Fun_Id);
   begin
      return Make_Side_Effect_Expr_Function_Call
        (Source_Location => Get_Source_Location (E),
         I_Function      => Symbol_Expr (Fun_Symbol),
         Arguments       => Make_Argument_List, --  Parameterless function.
         I_Type          => Get_Return_Type (Fun_Symbol.SymType));
   end Do_Parameterless_Function_Call;

   ----------------
   -- Find_Model --
   ----------------

   function Find_Model (Model : String) return Model_Sorts is
      Result : Model_Sorts := Not_A_Model;
      Upper_Model : constant String := To_Upper (Model);
   begin
      for A_Model in Valid_Model loop
         if Upper_Model = Model_Sorts'Image (A_Model) then
            Result := A_Model;
            exit;
         end if;
      end loop;
      return Result;
   end Find_Model;

   ----------------------------
   -- Get_Actual_Object_Name --
   ----------------------------

   function Get_Actual_Obj_Name (Obj : Entity_Id;
                                 Replace_Object : Boolean) return String is
      Loc_Obj_Unique_Name : constant String :=
        Unique_Name (Obj);

      Obj_Name_String : constant String :=
        (if Replace_Object then
            Replace_Local_With_Non_Visible
           (Is_Type => False,
            E       => Obj)
         else
            Loc_Obj_Unique_Name);
   begin
      return Obj_Name_String;
   end Get_Actual_Obj_Name;

   --------------------------
   -- Get_Actual_Type_Name --
   --------------------------

   function Get_Actual_Type (Obj : Entity_Id;
                            Replace_Object : Boolean) return String is
      Given_Type  : constant Node_Id := Etype (Obj);

      Optional_Type_Name : constant String :=
        (if Replace_Object then
            Replace_Local_With_Non_Visible
           (Is_Type => True,
            E       => Obj)
         else
            "");

      Replace_Type : constant Boolean :=
        Replace_Object and Optional_Type_Name /= "";

      Type_Name_String : constant String :=
        (if Replace_Type then
            Optional_Type_Name
         else
            Unique_Name (Given_Type));
   begin
      return Type_Name_String;
   end Get_Actual_Type;

   -------------------------
   -- Get_Annotation_Name --
   -------------------------

   function Get_Annotation_Name (N : Node_Id) return String is
     (Get_Name_String
        (Chars (Expression
                (First (Pragma_Argument_Associations (N))))));

   ---------------------------
   -- Get_Import_Convention --
   ---------------------------

   function Get_Import_Convention (N : Node_Id) return String is
      --  The gnat front end insists thet the parameters for
      --  pragma Import are given in the specified order even
      --  if named association is used:
      --  1. Convention,
      --  2. Enity,
      --  3. Optional External_Name,
      --  4. Optional Link_Name.
      --  The first 2 parameters are mandatory and
      --  for ASVAT models the External_Name is required.
      --
      --  The Convention parameter will always be present as
      --  the first parameter.
      Conv_Assoc : constant Node_Id :=
        First (Pragma_Argument_Associations (N));
      Conv_Name  : constant Name_Id := Chars (Conv_Assoc);
      Convention : constant String  := Get_Name_String
        (Chars (Expression (Conv_Assoc)));
   begin
      --  Double check the named parameter if named association is used.
      pragma Assert (Conv_Name = No_Name or else
                     Get_Name_String (Conv_Name) = "convention");
      return Convention;
   end Get_Import_Convention;

   ------------------------------
   -- Get_Import_External_Name --
   ------------------------------

   function Get_Import_External_Name (N : Node_Id) return String is
      --  The gnat front end insists thet the parameters for
      --  pragma Import are given in the specified order even
      --  if named association is used:
      --  1. Convention,
      --  2. Enity,
      --  3. Optional External_Name,
      --  4. Optional Link_Name.
      --  The first 2 parameters are mandatory and
      --  for ASVAT models the External_Name is required.
      --
      --  The External_Name parameter, if present, will be
      --  the third parameter.
      External_Assoc : constant Node_Id := Next
        (Next
           (First (Pragma_Argument_Associations (N))));
   begin
      if Present (External_Assoc) then
         declare
            External_Name : constant Name_Id := Chars (External_Assoc);
            External_Name_Id : constant String_Id :=
              Strval (Expression (External_Assoc));
            External_Name_Id_Length : constant Natural :=
              Natural (String_Length (External_Name_Id));
         begin
            --  Double check the named parameter if named association is used.
            pragma Assert (External_Name = No_Name or else
                             Get_Name_String
                               (External_Name) = "external_name");
            String_To_Name_Buffer (External_Name_Id);
            return To_Lower (Name_Buffer (1 .. External_Name_Id_Length));
         end;
      else
         return "";
      end if;
   end Get_Import_External_Name;

   --------------------------
   -- Get_Import_Link_Name --
   --------------------------

   function Get_Import_Link_Name (N : Node_Id) return String is
      --  The gnat front end insists thet the parameters for
      --  pragma Import are given in the specified order even
      --  if named association is used:
      --  1. Convention,
      --  2. Enity,
      --  3. Optional External_Name,
      --  4. Optional Link_Name.
      --  The first 2 parameters are mandatory and
      --  for ASVAT models the External_Name is required
      --  and for imported non-visible objects, Link_Name is required.
      --  The Link_Name parameter, if present, will be
      --  the Fourth parameter.
      External_Assoc : constant Node_Id := Next
        (Next
           (First (Pragma_Argument_Associations (N))));
      Link_Assoc     : constant Node_Id :=
        (if Present (External_Assoc) then Next (External_Assoc)
         else External_Assoc);
   begin
      if Present (Link_Assoc) then
         declare
            Link_Name    : constant Name_Id := Chars (Link_Assoc);
            Link_Name_Id : constant String_Id :=
              Strval (Expression (Link_Assoc));
            Link_Name_Id_Length : constant Natural :=
              Natural (String_Length (Link_Name_Id));
         begin
            --  Double check the named parameter if named association is used.
            pragma Assert (Link_Name = No_Name or else
                             Get_Name_String
                               (Link_Name) = "link_name");
            String_To_Name_Buffer (Link_Name_Id);
            return To_Lower (Name_Buffer (1 .. Link_Name_Id_Length));
         end;
      else
         return "";
      end if;
   end Get_Import_Link_Name;

   -------------------------
   -- Get_Model_From_Anno --
   -------------------------

   function Get_Model_From_Anno (N : Node_Id) return Model_Sorts is
      Anno_Value : constant Node_Id := Expression (N);
      Is_Aggregate : constant Boolean :=
        Nkind (Anno_Value) = N_Aggregate;
   begin
      --  The value of an ASVAT annotation must be a non empty aggregate
      if Is_Aggregate and then Present (Expressions (Anno_Value))
      then
         declare
            First_Arg  : constant Node_Id :=
              First (Expressions (Anno_Value));
            Second_Arg : constant Node_Id :=
              (if Present (First_Arg) then
                    Next (First_Arg)
               else First_Arg);
            Is_ASVAT : constant Boolean :=
              Nkind (First_Arg) = N_Identifier and then
              Get_Name_String (Chars (First_Arg)) = "asvat";
            Has_Model : constant Boolean :=
              (Is_ASVAT and Present (Second_Arg)) and then
              Nkind (Second_Arg) = N_Identifier;
         begin
            if Is_ASVAT then
               if Present (Second_Arg) and then
                 Nkind (Second_Arg) /= N_Identifier
               then
                  Report_Unhandled_Node_Empty
                    (N,
                     "Is_ASVAT_Annotation",
                     "ASVAT model must be an identifier");
               elsif not Present (Second_Arg) then
                  Report_Unhandled_Node_Empty
                    (N,
                     "Is_ASVAT_Annotation",
                     "ASVAT anotation must supply a model");
               end if;
            end if;
            return (if Is_ASVAT and Has_Model then
                       Find_Model (Get_Name_String (Chars (Second_Arg)))
                    else
                       Not_A_Model);
         end;
      else
         if Nkind (Anno_Value) = N_Identifier then
            if Get_Name_String (Chars (Anno_Value)) = "asvat" then
               Report_Unhandled_Node_Empty
                 (N,
                  "Is_ASVAT_Annotation",
                  "ASVAT annotation must have a model");
            end if;
         end if;
         return Not_A_Model;
      end if;
   end Get_Model_From_Anno;

   ---------------------------
   -- Get_Model_From_Import --
   ---------------------------

   function Get_Model_From_Import (N : Node_Id) return Model_Sorts is
      Convention : constant String :=
        ASVAT_Modelling.Get_Import_Convention (N);

      Is_Ada : constant Boolean := Convention = "ada";

      Model_String  : constant String :=
        (if Is_Ada then
            Get_Import_External_Name (N)
         else
            "");
      Model : constant Model_Sorts := Find_Model (Model_String);
   begin
      if Is_Ada then
         if Model_String = "" then
            Report_Unhandled_Node_Empty
              (N, "Get_Model_From_Import",
               "Import convention Ada must have a model sort");
         elsif Model = Not_A_Model then
            Report_Unhandled_Node_Empty
              (N, "Get_Model_From_Import",
               "Import convention Ada but '" &
                 Model_String &
                 "' is not an ASVAT model sort");
         end if;
      end if;
      return Model;
   end Get_Model_From_Import;

   --------------------
   -- Get_Model_Sort --
   --------------------

   function Get_Model_Sort (E : Entity_Id) return Model_Sorts is
      Obj_Import    : constant Node_Id := Get_Pragma (E, Pragma_Import);
      Subprog_Import : constant Node_Id :=
        (if Ekind (E) in E_Procedure | E_Function then
              Import_Pragma (E)
         else
            Obj_Import);

      Anno           : constant Node_Id := Find_Aspect (E, Aspect_Annotate);

      Anno_Model     : constant Model_Sorts :=
          (if Present (Anno) then
              Get_Model_From_Anno (Anno)
         else
            Not_A_Model);

      --  The ASVAT anotation is used even if there is a pragma Import
      --  specifying a, possibly different, model.
      Import_Model   : constant Model_Sorts :=
        (if Present (Obj_Import) then
              (if Anno_Model = Not_A_Model then
                   Get_Model_From_Import (Obj_Import)
              else
                 Anno_Model)
         elsif Present (Subprog_Import) then
              (if Anno_Model = Not_A_Model then
                   Get_Model_From_Import (Subprog_Import)
              else
                 Anno_Model)
         else
            Not_A_Model);

   begin
      return (if Anno_Model /= Not_A_Model then
                 Anno_Model
              else
                 Import_Model);
   end Get_Model_Sort;

--  procedure Mem_Copy (E : Entity_Id; S, D : System.Address; Size : Natural);
--  procedure Mem_Copy (E : Entity_Id; S, D : System.Address; Size : Natural)
--     is
--        S_Irep : constant Irep :=
--          Make_Symbol_Expr (Source_Location => Get_Source_Location (E),
--                            I_Type          => Make_Pointer_Type
--                              (I_Subtype => Ireps.Empty,
--                               Width     => 64),
--                            Range_Check     => False,
--                            Identifier      =>
--                              Unique_Name (Defining_Identifier (S)));
--        D_Irep : constant Irep :=
--          Make_Symbol_Expr (Source_Location => Get_Source_Location (E),
--                            I_Type          => Make_Pointer_Type
--                              (I_Subtype => Ireps.Empty,
--                               Width     => 64),
--                            Range_Check     => False,
--                            Identifier      =>
--                              Unique_Name (Defining_Identifier (D)));
--        Size_Irep : constant Irep :=
--          Make_Symbol_Expr (Source_Location => Get_Source_Location (E),
--                         I_Type          => Do_Type_Reference (Etype (Size)),
--                            Range_Check     => False,
--                            Identifier      =>
--                              Unique_Name (Defining_Identifier (Size)));
--   begin
--        Print_Irep (S_Irep);
--        Print_Irep (D);
--        Print_Irep (Size);
--      null;
--   end Mem_Copy;

   procedure Make_Memcpy_Procedure (E : Entity_Id) is
      Source_Location : constant Irep := Get_Source_Location (E);
      First_Param : constant Node_Id :=
        First (Parameter_Specifications (Declaration_Node (E)));
      Second_Param : constant Node_Id :=
        (if Present (First_Param) then Next (First_Param) else Types.Empty);
      Third_Param : constant Node_Id :=
        (if Present (Second_Param) then Next (Second_Param) else Types.Empty);
      Only_3_Param : constant Boolean  :=
        Present (Third_Param) and not Present (Next (Third_Param));

      Destination_Loc_Name : constant String :=
        (if Present (First_Param) then
              Get_Name_String (Chars (Defining_Identifier (First_Param)))
         else
            "");
      Destination : constant String :=
        (if Present (First_Param) then
              Unique_Name (Defining_Identifier (First_Param))
         else
            "");
      Source_Loc_Name : constant String :=
        (if Present (Second_Param) then
              Get_Name_String (Chars (Defining_Identifier (Second_Param)))
         else
            "");
      Source : constant String :=
        (if Present (Second_Param) then
              Unique_Name (Defining_Identifier (Second_Param))
         else
            "");

      Count_Loc_Name : constant String :=
        (if Present (Third_Param) then
              Get_Name_String (Chars (Defining_Identifier (Third_Param)))
         else
            "");
      Count : constant String :=
        (if Present (Third_Param) then
              Unique_Name (Defining_Identifier (Third_Param))
         else
            "");
   begin
      if Only_3_Param and then
        Destination_Loc_Name = "destination" and then
        Source_Loc_Name = "source" and then
        Count_Loc_Name = "no_of_bits"
      then
         declare
            Subprog_Id  : constant Symbol_Id := Intern (Unique_Name (E));
            Subprog_Sym : constant Symbol    :=
                Global_Symbol_Table (Subprog_Id);

            Dest_Sym_Id : constant Symbol_Id := Intern (Destination);
            Src_Sym_Id  : constant Symbol_Id := Intern (Source);
            Cnt_Sym_Id  : constant Symbol_Id := Intern (Count);

            Dest_Sym : constant Symbol :=
              Global_Symbol_Table (Dest_Sym_Id);
            Src_Sym  : constant Symbol :=
              Global_Symbol_Table (Src_Sym_Id);
            Cnt_Sym  : constant Symbol :=
              Global_Symbol_Table (Cnt_Sym_Id);

            Dest_Irep : constant Irep :=
              Typecast_If_Necessary
                (Symbol_Expr (Sym => Dest_Sym),
                 New_Type => Make_Pointer_Type (Make_Void_Type),
                 A_Symbol_Table => Global_Symbol_Table);

            Src_Irep : constant Irep :=
              Typecast_If_Necessary
                (Symbol_Expr (Sym => Src_Sym),
                 New_Type => Make_Pointer_Type (Make_Void_Type),
                 A_Symbol_Table => Global_Symbol_Table);

            Cnt_Irep : constant Irep :=
--              Typecast_If_Necessary
                Symbol_Expr (Sym => Cnt_Sym);
--                 New_Type => Make_Signedbv_Type (64),
--                 A_Symbol_Table => Global_Symbol_Table);

            Bits_To_Bytes : constant Irep :=
              Make_Constant_Expr (Source_Location => Source_Location,
                                  I_Type          => CProver_Size_T,
                                  Range_Check     => False,
                                  Value           =>
                                  --  bits to bytes (div by 8)
                                    Convert_Uint_To_Hex
                                      (Value     => UI_From_Int (8),
                                       Bit_Width => 32));

            Byte_Cnt_Irep : constant Irep :=
              Make_Op_Div (Rhs               => Bits_To_Bytes,
                           Lhs               => Cnt_Irep,
                           Div_By_Zero_Check => False,
                           Source_Location   => Source_Location,
                           Overflow_Check    => False,
                           I_Type            => CProver_Size_T,
                           Range_Check       => False);

            Memcpy_Args  : constant Irep := Make_Argument_List;
            Memcpy_Name  : constant String := "memcpy";
            Memcpy_Sym   : constant Symbol :=
              Global_Symbol_Table (Intern (Memcpy_Name));
            Memcpy_Func  : constant Irep :=
              Symbol_Expr (Sym => Memcpy_Sym);

            Memcpy_Proc_Call : constant Irep :=
             Make_Code_Function_Call
              (Arguments       => Memcpy_Args,
               I_Function      => Memcpy_Func,
               Lhs             => CProver_Nil,
               Source_Location => Source_Location,
               I_Type          => CProver_Void_T,
               Range_Check     => False);

            Subprog_Body : constant Irep :=
              Make_Code_Block (Source_Location => Source_Location,
                               I_Type          => CProver_Nil_T);

            Updated_Subprog_Sym : Symbol;

         begin
            Put_Line (Unintern (Dest_Sym.Name));
            Print_Irep (Dest_Sym.SymType);
            Put_Line (Unintern (Src_Sym.Name));
            Print_Irep (Src_Sym.SymType);
            Put_Line (Unintern (Cnt_Sym.Name));
            Print_Irep (Cnt_Sym.SymType);

            Print_Irep (Memcpy_Func);

            Print_Irep (Dest_Irep);
            Print_Irep (Src_Irep);
            Print_Irep (Cnt_Irep);
            Print_Irep (Bits_To_Bytes);
            Print_Irep (Byte_Cnt_Irep);

            Append_Argument (I     => Memcpy_Args,
                             Value => Dest_Irep);
            Append_Argument (I     => Memcpy_Args,
                             Value => Src_Irep);
            Append_Argument (I     => Memcpy_Args,
                             Value => Byte_Cnt_Irep);

            Print_Irep (Memcpy_Args);

            Print_Irep (Memcpy_Proc_Call);

            Append_Op (Subprog_Body, Memcpy_Proc_Call);

            Print_Irep (Subprog_Body);

            Put_Line (Unintern (Subprog_Id));
            Put_Line (Unintern (Subprog_Sym.Name));

            Updated_Subprog_Sym := Subprog_Sym;
            Updated_Subprog_Sym.Value := Subprog_Body;
            Global_Symbol_Table.Replace (Subprog_Id, Updated_Subprog_Sym);
         end;
      else
         Report_Unhandled_Node_Empty (E,
                                      "Make_Memcpy_Procedure",
                                      "Invalid parameters");
      end if;
   end Make_Memcpy_Procedure;

      ----------------
   -- Make_Model --
   ----------------

   procedure Make_Model (E : Entity_Id; Model : Model_Sorts) is
      Source_Location : constant Irep := Get_Source_Location (E);
      Subprog_Id      : constant Symbol_Id := Intern (Unique_Name (E));
      Subprog_Body    : constant Irep := Make_Code_Block (Source_Location);

      --  Get lists of all the inputs and outputs of the model
      --  subprogram including all those listed in a pragma Global.
      --  Presently the list of inputs is not used.

      Model_Inputs  : Elist_Id := No_Elist;
      Model_Outputs : Elist_Id := No_Elist;
      Global_Seen   : Boolean;
      Iter          : Elmt_Id;
   begin
      if Model = Memcpy then
         Print_Node_Briefly (E);
         Print_Node_Briefly (Declaration_Node (E));
         Make_Memcpy_Procedure (E);
         return;
      end if;

      Collect_Subprogram_Inputs_Outputs
        (Subp_Id      => E,
         Synthesize   => False,
         Subp_Inputs  => Model_Inputs,
         Subp_Outputs => Model_Outputs,
         Global_Seen  => Global_Seen);
      if not Global_Seen then
         Put_Line
           (Standard_Error,
            "Global aspect or pragma expected for ASVAT model.");
         Put_Line
           (Standard_Error,
            "Specify 'Global => null' if the model has no " &
              "global variables");
      end if;

      Print_Modelling_Message
        ("Adding a " & To_Lower (Model_Sorts'Image (Model)) &
           " body for modelling subprogram " &
           Unique_Name (E), Sloc (E));

      --  Process all of the potential output parameters and globals.
      --  They will be set to nondet.
      Iter := (if Model_Outputs /= No_Elist then
                  First_Elmt (Model_Outputs)
               else
                  No_Elmt);
      while Present (Iter) loop
         if Nkind (Node (Iter)) in
           N_Identifier | N_Expanded_Name | N_Defining_Identifier
         then
            declare
               Curr_Entity : constant Node_Id :=
                 (if Nkind (Node (Iter)) = N_Defining_Identifier then
                     Node (Iter)
                  else
                     Entity (Node (Iter)));

               --  Determine whether the local object declaration represents
               --  a non-visible object and possibly (sub)type declaration.
               Replace_Object : constant Boolean :=
                 Get_Model_Sort (Curr_Entity) = Represents;

               --  The local object may be replaced by a non-visible variable
               --  if the "Represents" ASVAT model is applied to the
               --  local object declaration.
               Unique_Object_Name : constant String :=
                 Get_Actual_Obj_Name (Curr_Entity, Replace_Object);

               --  The local object's type  may be replaced by a non-visible
               --  type if the "Represents" ASVAT model is applied to the
               --  local object declaration.
               Unique_Type_Name : constant String :=
                 Get_Actual_Type (Curr_Entity, Replace_Object);

               Object_Id : constant Symbol_Id := Intern (Unique_Object_Name);
               Type_Id   : constant Symbol_Id := Intern (Unique_Type_Name);

            begin
               if Replace_Object and then Unique_Object_Name = "" then
                  --  Object replacement requested but no replacement
                  --  object specified.
                  Report_Unhandled_Node_Empty
                    (Curr_Entity, "Make_Model",
                     "ASVAT_Modelling: replacement object missing after " &
                       "Represents in model definition.");
               elsif Ekind (Curr_Entity) /= E_Abstract_State then

                  if Replace_Object then
                     Print_Modelling_Message
                       ("Replace local object '" &
                          Unique_Name (Curr_Entity) &
                          "' with '" &
                          Unique_Object_Name &
                          " : " &
                          Unique_Type_Name &
                          "'", Sloc (Curr_Entity));

                     if not Global_Symbol_Table.Contains (Type_Id)
                     then
                        --  The non-visible type declaration has not been
                        --  processed yet.
                        --  A premature declaration which exactly matches
                        --  the actual declaration has to be inserted into
                        --  the global symbol table.
                        --  The local type definition has to match the
                        --  actual declaration so the local type definition
                        --  can be used.
                        declare
                           Local_Type_Id : constant Symbol_Id :=
                             Intern (Unique_Name (Etype (Curr_Entity)));
                           Local_Type_Sym : constant Symbol :=
                             Global_Symbol_Table (Local_Type_Id);
                           Actual_Type : constant Symbol :=
                             Make_Type_Symbol
                               (Type_Id, Local_Type_Sym.SymType);
                        begin
                           Global_Symbol_Table.Insert
                             (Type_Id, Actual_Type);
                        end;
                     end if;

                     pragma Assert (Global_Symbol_Table.Contains
                                    (Type_Id), "Type not in Table");

                     if not Global_Symbol_Table.Contains (Object_Id)
                     then
                        declare
                           --  Similarly to the non-visible object
                           --  declaration has not been processed yet.
                           --  The local object declaration can be used to
                           --  enter a premature declaration of the actual
                           --  object.
                           --  The symbol table must contain the local
                           --  object as it has just been declared
                           Local_Object_Id     : constant Symbol_Id :=
                             Intern (Unique_Name (Curr_Entity));
                           Local_Object_Symbol : constant Symbol :=
                             Global_Symbol_Table (Local_Object_Id);
                        begin
                           New_Object_Symbol_Entry
                             (Object_Name       => Object_Id,
                              Object_Type       => Local_Object_Symbol.SymType,
                              Object_Init_Value => Local_Object_Symbol.Value,
                              A_Symbol_Table    => Global_Symbol_Table);
                        end;
                     end if;
                  end if;

                  --  The symbol table will have the declaration of the
                  --  object to be made nondet.

                  declare
                     Var_Sym_Id : constant Symbol_Id :=
                       Intern (Unique_Object_Name);
                     Var_Symbol : constant Symbol :=
                       Global_Symbol_Table (Var_Sym_Id);
                  begin
                     --  Nondet the variable.
                     Append_Op (Subprog_Body,
                                Do_Nondet_Var (Var_Name => Unique_Object_Name,
                                               Var_Type => Unique_Type_Name,
                                               E        => E));

                     --  If the subprogram ASVAT model is "Nondet_In_Type",
                     --  an assume statment is appended to the subprogram body
                     if Model =  Nondet_In_Type then
                        --  If the object declaration is not visible
                        --  The local declaration will be used as the
                        --  modelling rules state that it must be
                        --  identical to the hidden declaration.
                        declare
                           Var_Irep   : constant Irep := Make_Symbol_Expr
                             (Source_Location => Source_Location,
                              I_Type          => Var_Symbol.SymType,
                              Range_Check     => False,
                              Identifier      => Unique_Object_Name);
                        begin
                           Make_Selector_Names
                             (Unique_Object_Name,
                              Var_Irep,
                              Subprog_Body,
                              Etype (Curr_Entity),
                              E,
                              Get_Source_Location (E));
                        end;
                     end if;
                  end;
               else
                  Report_Unhandled_Node_Empty
                    (Curr_Entity, "Make_Model",
                     "Abstract_State as a global output is unsupported");
               end if;
            end;
         else
            Report_Unhandled_Node_Empty
              (Node (Iter), "Make_Model",
               "Unsupported Global output");
         end if;

         Next_Elmt (Iter);
      end loop;

      --  If the subprogram is a function, the result must be made nondet too.
      if Ekind (E) = E_Function then
         declare
            --  Create a variable to contain the nondet result.
            Result_Var    : constant String :=  "result___" & Unique_Name (E);
            Result_Var_Id : constant Symbol_Id := Intern (Result_Var);

            Result_Type     : constant String := Unique_Name (Etype (E));
            Result_Type_Id  : constant Symbol_Id := Intern (Result_Type);
            pragma Assert (Global_Symbol_Table.Contains (Result_Type_Id),
                           "Make_Model: Symbol table does not contain" &
                             "function " & Unique_Name (E) & " result type.");
            Result_Type_Sym : constant Symbol :=
              Global_Symbol_Table (Result_Type_Id);

            Result_Var_Irep   : constant Irep := Make_Symbol_Expr
              (Source_Location => Source_Location,
               Identifier      => Result_Var,
               I_Type          => Result_Type_Sym.SymType);

            Var_Decl          : constant Irep := Make_Code_Decl
              (Symbol          => Result_Var_Irep,
               Source_Location => Source_Location);

            Return_Statement : constant Irep := Make_Code_Return
              (Return_Value    => Result_Var_Irep,
               Source_Location => Source_Location);

         begin
            Put_Line ("The unique namme of the result type is " &
                        Unique_Name (Etype (E)));
            --  Insert the Result_Var into the symbol table.
            --  It should not already exist.
            pragma Assert (not Global_Symbol_Table.Contains (Result_Var_Id),
                           "Symbol table already contains " & Result_Var);
            New_Object_Symbol_Entry
              (Object_Name       => Result_Var_Id,
               Object_Type       => Result_Type_Sym.SymType,
               Object_Init_Value => Ireps.Empty,
               A_Symbol_Table    => Global_Symbol_Table);

            --  Add a new block to the function for the Result_Var declaration
            Append_Op (Subprog_Body, Make_Code_Block (Source_Location));
            --  Add the declaration of the result varible to the new block
            Append_Op (Subprog_Body, Var_Decl);

            --  Set the result variable to nondet.
            Append_Op (Subprog_Body,
                       Do_Nondet_Var
                         (Var_Name => Result_Var,
                          Var_Type => Result_Type,
                          E        => E));
            --  if ASVAT model is "Nondet_In_Type", do an in type assumption.
            if Model = Nondet_In_Type then
               Make_Selector_Names
                 (Result_Var,
                  Result_Var_Irep,
                  Subprog_Body,
                  Etype (E),
                  E,
                  Get_Source_Location (E));
            end if;
            --  The function needs a return statement.
            Append_Op (Subprog_Body, Return_Statement);
         end;
      end if;

      pragma Assert (Global_Symbol_Table.Contains (Subprog_Id),
                     "Make_Model: Subprogram not in symbol table.");
      --  The model body is now made the body of the subprogram.
      declare
         Subprog_Sym : Symbol := Global_Symbol_Table (Subprog_Id);
      begin
         Subprog_Sym.Value := Subprog_Body;
         Global_Symbol_Table.Replace (Subprog_Id, Subprog_Sym);
      end;
   end Make_Model;

   ---------------------------
   -- Make_Nondet_Function --
   ---------------------------

   procedure Make_Nondet_Function (Fun_Name, Result_Type : String;
                                   Statements : Irep;
                                   E          : Entity_Id) is
      Fun_Symbol_Id : constant Symbol_Id := Intern (Fun_Name);
      Source_Loc    : constant Irep := Get_Source_Location (E);
   begin
      if not Global_Symbol_Table.Contains (Fun_Symbol_Id) then
         declare
            Type_Id : constant Symbol_Id := Intern (Result_Type);
            pragma Assert (Global_Symbol_Table.Contains (Type_Id),
                           Result_Type & " is not in symbol table");
            Result_Type_Irep : constant Irep :=
              Global_Symbol_Table (Type_Id).SymType;

            Param_List : constant Irep := Make_Parameter_List;
            --  For a nondet funcition the Param_List is always empty.
            Fun_Type : constant Irep := Make_Code_Type
              (Parameters  => Param_List,
               Ellipsis    => False,
               Return_Type => Result_Type_Irep,
               Inlined     => False,
               Knr         => False);

            Obj_Name  : constant String := "result___" & Fun_Name;
            Obj_Id    : constant Symbol_Id := Intern (Obj_Name);
            Obj_Sym   : constant Irep := Make_Symbol_Expr
              (Source_Location => Source_Loc,
               Identifier      => Obj_Name,
               I_Type          => Result_Type_Irep);

            Decl      : constant Irep := Make_Code_Decl
              (Symbol          => Obj_Sym,
               Source_Location => Source_Loc);

            Fun_Body : constant Irep := Make_Code_Block (Source_Loc);

            Return_Statement : constant Irep := Make_Code_Return
              (Return_Value    => Obj_Sym,
               Source_Location => Source_Loc);

            Fun_Symbol : Symbol;

         begin
            Print_Modelling_Message
              ("Making nondet function " & Fun_Name &
                 " : " & Result_Type, Sloc (E));

            New_Subprogram_Symbol_Entry
              (Subprog_Name   => Fun_Symbol_Id,
               Subprog_Type   => Fun_Type,
               A_Symbol_Table => Global_Symbol_Table);

            pragma Assert (Global_Symbol_Table.Contains (Fun_Symbol_Id));

            Append_Op (Fun_Body, Decl);

            pragma Assert (not Global_Symbol_Table.Contains (Obj_Id),
                           "Symbol table already contains " & Obj_Name);
            New_Object_Symbol_Entry (Object_Name       => Obj_Id,
                                     Object_Type       => Result_Type_Irep,
                                     Object_Init_Value => Ireps.Empty,
                                     A_Symbol_Table    => Global_Symbol_Table);

            if Statements /= Ireps.Empty then
               Report_Unhandled_Node_Empty
                 (Error,
                  "Make_Nondet_Function",
                  "Additional statements are currently unsupported");
               null;  --  Todo append the statements
            end if;

            Append_Op (Fun_Body, Return_Statement);

            Fun_Symbol := Global_Symbol_Table (Fun_Symbol_Id);
            Fun_Symbol.Value := Fun_Body;
            Global_Symbol_Table.Replace (Fun_Symbol_Id, Fun_Symbol);
         end;
      else
         null;  --  The function has already been created previously.
      end if;
   end Make_Nondet_Function;

   -------------------------
   -- Make_Selector_Names --
   -------------------------

   procedure Make_Selector_Names (Root : String;
                                  Root_Irep : Irep;
                                  Block : Irep;
                                  Root_Type : Node_Id;
                                  E : Entity_Id;
                                  Loc : Irep) is
      Type_Irep : constant Irep := Do_Type_Reference (Root_Type);
   begin
      if Is_Scalar_Type (Root_Type) then
         Append_Op (Block,
                    Do_Var_In_Type (Var_Name  => Root,
                                    Var_Type  => Unique_Name (Root_Type),
                                    Var_Irep  => Root_Irep,
                                    Type_Irep => Type_Irep,
                                    E         => E));
      elsif Is_Record_Type (Root_Type) then
         if not Has_Discriminants (E) then
            declare
               Comp : Node_Id :=
                 First_Component (Root_Type);
            begin
               while Present (Comp) loop
                  declare
                     Comp_Name : constant String :=
                       Root & "__" & Get_Name_String (Chars (Comp));
                     Comp_Unique : constant String := Unique_Name (Comp);
                     Comp_Type : constant Node_Id :=
                       Etype (Comp);
                  begin
                     Make_Selector_Names
                       (Comp_Name & " == " & Comp_Unique,
                        Make_Member_Expr (Compound         => Root_Irep,
                                          Source_Location  => Loc,
                                          I_Type           => Do_Type_Reference
                                            (Comp_Type),
                                          Component_Name   => Comp_Unique),
                        Block,
                        Comp_Type,
                        E,
                        Loc);
                  end;
                  Comp := Next_Component (Comp);
               end loop;
            end;
         else
            Report_Unhandled_Node_Empty
              (E,
               "Make_Selector_Names",
               "Discrimiminated records are currently unsupported");
         end if;
      else
         Report_Unhandled_Node_Empty
           (E,
            "Make_Selector_Names",
           "Only scalar and record subtypes are currently supported");
      end if;
   end Make_Selector_Names;

   -----------------------------
   -- Print_Modelling_Message --
   -----------------------------

   procedure Print_Modelling_Message (Mess : String; Loc : Source_Ptr) is
   begin
      if Print_Message then
         Put_Line (Build_Location_String (Loc) & " ASVAT modelling: ");
         Put_Line (Mess);
      end if;
   end Print_Modelling_Message;

   ------------------------------------
   -- Replace_Local_With_Non_Visible --
   ------------------------------------

   function Replace_Local_With_Non_Visible
     (Is_Type : Boolean; E : Entity_Id) return String
   is
      function Get_Object_From_Anno (N : Node_Id;
                                     Want_Type : Boolean) return String
      with Pre => Nkind (N) = N_Aspect_Specification;

      function Get_Object_From_Anno (N : Node_Id;
                                         Want_Type : Boolean) return String is
         --  From where this subprogram is called it has been determined that
         --  the ASVAT annotation specifies the model "'Represents'.
         --  The ASVAT Represents annotation must have at least one more
         --  argument which is the hidden object that is represented by the
         --  local object.
         --  The hidden type, if present is the fourth argument.
         Hidden_Obj_Name : constant Node_Id :=
         --  The expression of N is an aggregate, The first of the aggregate
         --  expressions is "ASVAT", the second is "Represents" and the third
         --  the hidden object name and the fourth
         --  (if present) the hidden type.
           Next (Next (First (Expressions (Expression (N)))));
         Hidden_Name : constant Node_Id :=
           (if Want_Type and Present (Hidden_Obj_Name) then
                 Next (Hidden_Obj_Name)
            else
               Hidden_Obj_Name);
      begin
         if Present (Hidden_Name) then
            if Nkind (Hidden_Name) = N_String_Literal then
               declare
                  Hidden_Name_Str : constant String_Id :=
                    Strval (Hidden_Name);
                  Hidden_Name_Length : constant Natural :=
                    Natural (String_Length (Hidden_Name_Str));
               begin
                  String_To_Name_Buffer (Hidden_Name_Str);
                  return To_Lower (Name_Buffer (1 .. Hidden_Name_Length));
               end;
            else
               if Want_Type then
                  Report_Unhandled_Node_Empty
                    (Hidden_Name,
                     "Replace_Local_With_Import",
                     "Hidden subtype name must be a string literal");
               else
                  Report_Unhandled_Node_Empty
                    (Hidden_Name,
                     "Replace_Local_With_Import",
                     "Hidden object name must be a string literal");
               end if;
            end if;
         elsif not Want_Type then
            Report_Unhandled_Node_Empty
              (Hidden_Name,
               "Replace_Local_With_Import",
               "Hidden object name must specified");
         end if;
         return "";
      end Get_Object_From_Anno;

      Obj_ASVAT_Anno : constant Node_Id :=
        Find_Aspect (E, Aspect_Annotate);
      Obj_Import_Pragma  : constant Node_Id := Get_Pragma (E, Pragma_Import);

      Import_Object_Desc : constant String :=
        Replace_Dots
          (Trim ((if Present (Obj_ASVAT_Anno) then
              Get_Object_From_Anno (N => Obj_ASVAT_Anno,
                                    Want_Type => False)
           elsif Present (Obj_Import_Pragma) then
              Get_Import_Link_Name (Obj_Import_Pragma)
           else
              ""), Ada.Strings.Both));

      Has_Type_Specified : constant Natural := Index (Import_Object_Desc, ":");

      Obj_Name_End       : constant Natural :=
        (if Has_Type_Specified /= 0 then
            Has_Type_Specified - 1
         else
            Import_Object_Desc'Last);

      Replacement_Obj_Name : constant String :=
        Trim
          (Import_Object_Desc
             (Import_Object_Desc'First .. Obj_Name_End),
                 Ada.Strings.Both);

      Replacement_Type_Name : constant String :=
        Trim ((if Present (Obj_ASVAT_Anno) then
                  Replace_Dots
                 (Get_Object_From_Anno
                    (N => Obj_ASVAT_Anno,
                     Want_Type => True))
               elsif Has_Type_Specified /= 0 then
                  Import_Object_Desc
                 (Has_Type_Specified + 1 .. Import_Object_Desc'Last)
               else
                  ""), Ada.Strings.Both);
   begin
      return (if Is_Type then
                 Replacement_Type_Name
              else
                 Replacement_Obj_Name);
   end Replace_Local_With_Non_Visible;

   function Replace_Dots (S : String) return String is
      function Replace_Dots_Rec (So_Far : String;
                                 Pos : Natural) return String;
      function Replace_Dots_Rec (So_Far : String;
                                 Pos : Natural) return String is
      begin
         if Pos in S'Range then
            if S (Pos) = '.' then
               return Replace_Dots_Rec (So_Far & "__", Pos + 1);
            else
               return Replace_Dots_Rec (So_Far & S (Pos), Pos + 1);
            end if;
         else
            return So_Far; --  (if So_Far /= "" then So_Far & "__" else "");
         end if;
      end Replace_Dots_Rec;

   begin
      return Replace_Dots_Rec ("", S'First);
   end Replace_Dots;

end ASVAT_Modelling;
