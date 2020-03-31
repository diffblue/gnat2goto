with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Aspects;                 use Aspects;
with Nlists;                  use Nlists;
with Stringt;                 use Stringt;
with Sinput;                  use Sinput;
with Namet;                   use Namet;
with Tree_Walk;               use Tree_Walk;
with Einfo;                   use Einfo;
with Symbol_Table_Info;       use Symbol_Table_Info;
with Follow;                  use Follow;
with GOTO_Utils;              use GOTO_Utils;
with Range_Check;             use Range_Check;
with Ada.Text_IO;             use Ada.Text_IO;
--  with Treepr;                  use Treepr;
--  with System;
package body ASVAT_Modelling is

   function Find_Model (Model : String) return Model_Sorts;

   function Make_In_Type_Function (E : Entity_Id) return Irep;
   --  The In_Type_Function model must have a single svcalar parameter
   --  of mode in, the object to be tested, and have a Boolean return type

   function Make_Nondet_Function (E : Entity_Id) return Irep;
   --  The Nondet_Function model must be a parameterless function with
   --  a scalar result subtype.

   function Replace_Dots (S : String) return String;

   function Replace_Local_With_Non_Visible
     (Is_Type : Boolean; E : Entity_Id) return String
   with Pre => Ekind (E) in E_Variable | E_Constant and then
     Get_Model_Sort (E) = Represents;

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

   --------------------------
   -- Make_In_Type_Function --
   --------------------------

   function Make_In_Type_Function (E : Entity_Id) return Irep is
      Source_Location : constant Irep := Get_Source_Location (E);
      Function_Name   : constant String := Unique_Name (E);
      Function_Id     : constant Symbol_Id := Intern (Function_Name);
      Parameter       : constant Node_Id :=
        First (Parameter_Specifications (Declaration_Node (E)));
      Param_Entity    : constant Node_Id := Defining_Identifier (Parameter);

      pragma Assert
        ((Ekind (E) = E_Function and
           Present (Parameter)) and then
           (not (Out_Present (Parameter) or Present (Next (Parameter))) and
                Is_Scalar_Type (Etype (Param_Entity)) and
              Is_Boolean_Type (Etype (E))),
         "In_Type_Function model must have a single scalar " &
           "mode in parameter and a Boolean return subtype");

      --  The subprogram is declared from the source text.
      pragma Assert (Global_Symbol_Table.Contains (Function_Id));

      --  Make a function body which is just a return statement with
      --  an and expression which is the in type condition
      Function_Body     : constant Irep :=
        Make_Code_Block (Source_Location);
      Param_Name : constant String :=
        Unique_Name (Defining_Identifier (Parameter));
      Param_Type        : constant Node_Id := Etype (Param_Entity);
      Followed_Type     : constant Irep :=
        Follow_Symbol_Type (Do_Type_Reference (Param_Type),
                            Global_Symbol_Table);
      Param_Irep        : constant Irep :=
        Make_Symbol_Expr
          (Source_Location => Source_Location,
           I_Type          => Followed_Type,
           Range_Check     => False,
           Identifier      => Param_Name);
      Resolved_Var : constant Irep :=
        Cast_Enum (Param_Irep, Global_Symbol_Table);
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

            Low_Bound_Irep : constant Irep :=
              Cast_Enum (Get_Bound (E, Resolved_Type, Bound_Low),
                         Global_Symbol_Table);

            High_Bound_Irep : constant Irep :=
              Cast_Enum (Get_Bound (E, Resolved_Type, Bound_High),
                         Global_Symbol_Table);

            Low_Bound_Condition : constant Irep :=
              Make_Op_Geq
                (Rhs =>
                   Typecast_If_Necessary
                     (Low_Bound_Irep,
                      Get_Type (Resolved_Var),
                      Global_Symbol_Table),
                 Lhs             => Resolved_Var,
                 Source_Location => Source_Location,
                 Overflow_Check  => False,
                 I_Type          => Make_Bool_Type,
                 Range_Check     => False);

            High_Bound_Condition : constant Irep :=
              Make_Op_Leq
                (Rhs             =>
                   Typecast_If_Necessary
                     (High_Bound_Irep,
                      Get_Type (Resolved_Var),
                      Global_Symbol_Table),
                 Lhs             => Resolved_Var,
                 Source_Location => Source_Location,
                 Overflow_Check  => False,
                 I_Type          => Make_Bool_Type,
                 Range_Check     => False);

            And_Conditions : constant Irep :=
              Make_Op_And
                (Source_Location => Source_Location,
                 I_Type          => Make_Bool_Type,
                 Range_Check     => False);

            Return_Statement : constant Irep :=
              Make_Code_Return
                (Return_Value    => And_Conditions,
                 Source_Location => Source_Location,
                 I_Type          => Make_Bool_Type,
                 Range_Check     => False);
         begin
            Print_Modelling_Message
              ("Making in type function " & Function_Name &
                 " (" & Get_Name_String
                 (Chars (Defining_Identifier (Parameter))) &
                 " : " & Unique_Name (Param_Type) &
                 ") return Boolean)", Sloc (E));
            Print_Modelling_Message
              ("Check (" &
                 Param_Name & " >= " & Unique_Name (Param_Type)
               & "'First and " &
                 Param_Name & " <= " & Unique_Name (Param_Type) & "'Last);",
               Sloc (E));

            Append_Op (And_Conditions, Low_Bound_Condition);
            Append_Op (And_Conditions, High_Bound_Condition);
            Append_Op (Function_Body, Return_Statement);
            return Function_Body;
         end;
      else
         return Report_Unhandled_Node_Irep
           (E,
            "Do_Var_In_Type",
            Irep_Kind'Image (Kind (Followed_Type)) &
              " objects not supported");
      end if;
   end Make_In_Type_Function;

   ----------------
   -- Make_Model --
   ----------------

   procedure Make_Model (E : Entity_Id; Model : Model_Sorts) is
      Subprog_Id      : constant Symbol_Id := Intern (Unique_Name (E));

      --  The subprogram must be in the symbol table as it has been declared
      --  in the subprogram text.
      --  Make an appropriate body for the model subprogram.
      Subprog_Body : constant Irep :=
      (case Model is
         when Nondet_Function  => Make_Nondet_Function (E),
         when In_Type_Function => Make_In_Type_Function (E),
         when Memcpy =>
--            Subprog_Sym.Value := Make_Memcpy_Procedure (E);
            Report_Unhandled_Node_Irep
              (N        => E,
               Fun_Name => "Make_Model",
               Message  => "ASVAT model Memcpy" &
                 " is currently unsupported"),
         when others =>
            Report_Unhandled_Node_Irep
              (N        => E,
               Fun_Name => "Make_Model",
               Message  => "ASVAT model " & Model_Sorts'Image (Model) &
                 " is currently unsupported"));

      Subprog_Sym : Symbol := Global_Symbol_Table (Subprog_Id);
   begin
      Subprog_Sym.Value := Subprog_Body;
      --  The model body is now made the body of the subprogram.
      Global_Symbol_Table.Replace (Subprog_Id, Subprog_Sym);
   end Make_Model;

   --------------------------
   -- Make_Nondet_Function --
   --------------------------

   function Make_Nondet_Function (E : Entity_Id) return Irep is
      Source_Location : constant Irep := Get_Source_Location (E);
      Function_Name   : constant String := Unique_Name (E);
      Function_Id      : constant Symbol_Id := Intern (Function_Name);

      pragma Assert (Ekind (E) = E_Function and
                       (not Present
                         (First
                            (Parameter_Specifications
                               (Declaration_Node (E))))
                     and then Is_Scalar_Type (Etype (E))),
                     "Nondet_Function model must be a parameterless " &
                       "function with a scalar return subtype");
      --  The subprogram is declared from the source text.
      pragma Assert (Global_Symbol_Table.Contains (Function_Id));

      --  Make a function body which declares an unitialised variable
      --  (and therefore nondet) of the function return type.
      Function_Symbol   : constant Symbol := Global_Symbol_Table (Function_Id);
      Function_Body     : constant Irep := Make_Code_Block (Source_Location);
      Return_Type_Irep  : constant Irep :=
        Get_Return_Type (Function_Symbol.SymType);
      Uninitialised_Var : constant String := "result___" & Function_Name;
      Var_Id            : constant Symbol_Id := Intern (Uninitialised_Var);
      Var_Expr          : constant Irep := Make_Symbol_Expr
        (Source_Location => Source_Location,
         Identifier      => Uninitialised_Var,
         I_Type          => Return_Type_Irep);
      Declaration       : constant Irep := Make_Code_Decl
        (Symbol          => Var_Expr,
         Source_Location => Source_Location);
      Return_Statement  : constant Irep := Make_Code_Return
        (Return_Value    => Var_Expr,
         Source_Location => Source_Location);
   begin
      Print_Modelling_Message
        ("Making nondet function " & Function_Name &
           " : " & Unique_Name (Etype (E)), Sloc (E));

      Append_Op (Function_Body, Declaration);

      --  Enter the uninitialised variable into the symbol table.
      pragma Assert (not Global_Symbol_Table.Contains (Var_Id),
                     "Symbol table already contains " & Uninitialised_Var);
      New_Object_Symbol_Entry (Object_Name       => Var_Id,
                               Object_Type       => Return_Type_Irep,
                               Object_Init_Value => Ireps.Empty,
                               A_Symbol_Table    => Global_Symbol_Table);

      --  Make the function body
      Append_Op (Function_Body, Return_Statement);

      return Function_Body;

   end Make_Nondet_Function;

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
