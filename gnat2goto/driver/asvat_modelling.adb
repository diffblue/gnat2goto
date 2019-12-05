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
--  with Symbol_Table_Info;       use Symbol_Table_Info;
with Ada.Text_IO;             use Ada.Text_IO;
package body ASVAT_Modelling is

   Print_Message : constant Boolean := False;

   function Find_Model (Model : String) return Model_Sorts;

   procedure Print_Modelling_Message (Mess : String; Loc : Source_Ptr);

   function Replace_Dots (S : String) return String;

   function Replace_Local_With_Import
     (Is_Type : Boolean; E : Entity_Id) return String
   with Pre => Ekind (E) in E_Variable | E_Constant and then
               Get_Model_Sort (E) = Represents;

   -------------------------
   -- Do_Nondet_Attribute --
   -------------------------

   function Do_Nondet_Attribute
     (N : Node_Id; Type_Name : String) return Irep is
      Fun_Name : constant String := To_Lower
        (Get_Name_String (Attribute_Name (N))) & "___" &
           Unique_Name (Entity (Prefix (N)));
      Loc      : constant Source_Ptr := Sloc (N);
   begin
      --  Create the nondet attribute function.
      --  It is not recreated by Make_Nondet_Function if it already exists.
      Make_Nondet_Function
        (Fun_Name    => Fun_Name,
         Result_Type => Type_Name,
         Statements  => Ireps.Empty,
         Loc         => Loc);
      --  Return the Irep of the nondet attribute function
      return Do_Nondet_Function_Call (Fun_Name, Loc);
   end Do_Nondet_Attribute;

   -------------------------------
   -- Do_Nondet_Function_Call --
   -------------------------------

   function Do_Nondet_Function_Call
     (Fun_Name : String; Loc : Source_Ptr) return Irep
   is
      Fun_Id     : constant Symbol_Id := Intern (Fun_Name);
      pragma Assert (Global_Symbol_Table.Contains (Fun_Id),
                     "gnat2goto fatal error: Nondet_Attribute_Function " &
                    Fun_Name & " not in symbol table.");
      Fun_Symbol : constant Symbol    := Global_Symbol_Table (Fun_Id);
      The_Function : constant Irep := New_Irep (I_Symbol_Expr);
   begin
      Set_Identifier (The_Function, Fun_Name);
      Set_Type (The_Function, Fun_Symbol.SymType);

      return R : constant Irep :=
        New_Irep (I_Side_Effect_Expr_Function_Call)
      do
         Set_Source_Location (R, Loc);
         Set_Function        (R, The_Function);
         Set_Arguments       (R, New_Irep (I_Argument_List));
         Set_Type (R, Get_Return_Type (Fun_Symbol.SymType));
      end return;
   end Do_Nondet_Function_Call;

   function Do_Nondet_Valid (N : Node_Id) return Irep is
      function Has_Prefix (N : Node_Id) return Boolean is
        (Nkind (N) in
             N_Attribute_Reference |
             --         N_Expanded_Name |
         N_Explicit_Dereference |
         N_Indexed_Component |
         N_Reference |
         N_Selected_Component |
         N_Slice);

      function Unique_Prefix_Name (N : Node_Id; So_Far : String) return String;

      function Unique_Prefix_Name
        (N : Node_Id; So_Far : String) return String is
         Name_Node : Node_Id := N;
      begin
         if Nkind (Parent (Name_Node)) /= N_Attribute_Reference then
            Name_Node := Parent (Name_Node);
            return (if Nkind (Name_Node) = N_Selected_Component then
                       Unique_Prefix_Name (Name_Node, So_Far & "__" &
                        Get_Name_String
                        (Chars (Selector_Name (Name_Node))))
                    elsif Nkind (Name_Node) = N_Indexed_Component then
                       Unique_Prefix_Name (Name_Node, So_Far & "__indexed")
                    else
                       So_Far);
         else
            return So_Far;
         end if;
      end Unique_Prefix_Name;

      Loc       : constant Source_Ptr := Sloc (N);
      Bool      : constant String := "standard__boolean";

      Name_Node : Node_Id := N;
      Result    : Irep;
   begin
      while Has_Prefix (Name_Node) loop
         Name_Node := Prefix (Name_Node);
      end loop;

      if Present (Entity_Of (Name_Node)) then
         declare
            Valid_Unique : constant String := "valid___" &
              Unique_Prefix_Name
              (Name_Node, Unique_Name (Entity_Of (Name_Node)));
         begin
            Make_Nondet_Function
              (Fun_Name    => Valid_Unique,
               Result_Type => Bool,
               Statements  => Ireps.Empty,
               Loc         => Loc);
            Result := Do_Nondet_Function_Call (Valid_Unique, Loc);
         end;
      else
         declare
            Valid_Default : constant String := "valid___default";
         begin
            Make_Nondet_Function (Fun_Name    => Valid_Default,
                                  Result_Type => Bool,
                                  Statements  => Ireps.Empty,
                                  Loc         => Loc);
            Result := Do_Nondet_Function_Call (Valid_Default, Loc);
         end;
      end if;

      return Result;

   end Do_Nondet_Valid;

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
      if Anno_Model /= Not_A_Model then
         if Import_Model /= Not_A_Model then
            Report_Unhandled_Node_Empty
              (E,
               "Get_Model_Sort",
               "Using model from Annotation, not Import pragma");
         end if;
         return Anno_Model;
      else
         return Import_Model;
      end if;
   end Get_Model_Sort;

   ----------------
   -- Make_Model --
   ----------------

   procedure Make_Model (E : Entity_Id; Model : Model_Sorts) is
      Loc : constant Source_Ptr := Sloc (E);
      Subprog_Id : constant Symbol_Id := Intern (Unique_Name (E));
      Block : constant Irep := New_Irep (I_Code_Block);

      procedure Make_Model_Section (Model : Model_Sorts;
                                    Outputs : Elist_Id);

      procedure Make_Model_Section (Model : Model_Sorts;
                                    Outputs : Elist_Id) is
         Iter      : Elmt_Id  := (if Present (Outputs)
                                  then First_Elmt (Outputs)
                                  else No_Elmt);
         Type_List : constant Elist_Id := New_Elmt_List;
         Print_Model : constant Boolean := False;
      begin
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

                  Given_Type  : constant Node_Id := Etype (Curr_Entity);
                  Loc_Type_Id : constant Symbol_Id :=
                    Intern (Unique_Name (Given_Type));

                  Loc_Obj_Unique_Name : constant String :=
                    Unique_Name (Curr_Entity);

                  Loc_Obj_Id : constant Symbol_Id :=
                    Intern (Loc_Obj_Unique_Name);

                  Replace_Object : constant Boolean :=
                    Get_Model_Sort (Curr_Entity) = Represents;

                  Obj_Name_String : constant String :=
                    (if Replace_Object then
                        Replace_Local_With_Import
                       (Is_Type => False,
                        E       => Curr_Entity)
                     else
                        Loc_Obj_Unique_Name);

                  Obj_Name_Id : constant Symbol_Id :=
                    Intern (Obj_Name_String);

                  Optional_Type_Name : constant String :=
                    (if Replace_Object then
                        Replace_Local_With_Import
                       (Is_Type => True,
                        E       => Curr_Entity)
                     else
                        "");

                  Replace_Type : constant Boolean :=
                    Replace_Object and Optional_Type_Name /= "";

                  Type_Name_String : constant String :=
                    (if Replace_Type then
                        Optional_Type_Name
                     else
                        Unique_Name (Given_Type));

                  Type_Name_Id : constant Symbol_Id :=
                    Intern (Type_Name_String);

                  Fun_Name : constant String := "Nondet___" &
                    Type_Name_String;

                  Assignment : constant Irep := New_Irep (I_Code_Assign);

                  Obj_Sym : constant Irep := New_Irep (I_Symbol_Expr);

               begin
                  if Replace_Object and then Obj_Name_String = "" then
                     --  Object replacement requested but no replacement
                     --  object specified.
                     Report_Unhandled_Node_Empty
                       (Curr_Entity, "Make_Model",
                        "ASVAT_Modelling: replacement object missing from " &
                        "Replace_With pragma Import.");
                  elsif Ekind (Curr_Entity) /= E_Abstract_State then

                     if Replace_Object then
                        Print_Modelling_Message
                          ("Replace local object '" &
                                    Unique_Name (Curr_Entity) &
                                    "' with '" &
                                    Obj_Name_String &
                                    " : " &
                                    Type_Name_String &
                                    "'", Sloc (Curr_Entity));

                        if Replace_Type and then not
                          Global_Symbol_Table.Contains (Type_Name_Id)
                        then
                           --  The type declaration has not been processed yet.
                           --  A premature declaration which exactly matches
                           --  the actual declaration has to be inserted into
                           --  the global symbol table.
                           --  The local type definition has to match the
                           --  actual declaration so the local type definition
                           --  can be used.
                           declare
                              Local_Type_Sym : constant Symbol :=
                                Global_Symbol_Table (Loc_Type_Id);
                              Actual_Type : constant Symbol :=
                                Make_Type_Symbol
                                  (Type_Name_Id, Local_Type_Sym.SymType);
                           begin
                              Global_Symbol_Table.Insert
                                (Type_Name_Id, Actual_Type);
                           end;
                        end if;

                        pragma Assert (Global_Symbol_Table.Contains
                                       (Type_Name_Id), "Type not in Table");

                        if not Global_Symbol_Table.Contains (Obj_Name_Id)
                        then
                           declare
                              --  The symbol table must contain the local
                              --  object as it has just been declared
                              Loc_Obj_Symbol : constant Symbol :=
                                Global_Symbol_Table (Loc_Obj_Id);
                           begin
                              New_Object_Symbol_Entry
                                (Object_Name       => Obj_Name_Id,
                                 Object_Type       => Loc_Obj_Symbol.SymType,
                                 Object_Init_Value => Loc_Obj_Symbol.Value,
                                 A_Symbol_Table    => Global_Symbol_Table);
                           end;
                        end if;
                     end if;

                     if not Contains (Type_List, Given_Type) then
                        Append_Elmt (Given_Type, Type_List);
                        Make_Nondet_Function (Fun_Name    => Fun_Name,
                                              Result_Type => Type_Name_String,
                                              Statements  => Ireps.Empty,
                                              Loc         => Sloc (E));
                     end if;

                     Set_Source_Location (Obj_Sym, Loc);
                     Set_Identifier      (Obj_Sym, Obj_Name_String);
                     Set_Type            (Obj_Sym,
                                          Global_Symbol_Table
                                            (Obj_Name_Id).SymType);

                     Set_Source_Location (Assignment, Loc);
                     Set_Lhs             (Assignment, Obj_Sym);
                     Set_Rhs             (Assignment,
                                          Do_Nondet_Function_Call
                                            (Fun_Name, Loc));
                     Append_Op (Block, Assignment);

                     if Print_Model then
                        Put_Line ("Assign " &
                                 Obj_Name_String & " := " & Fun_Name);
                     end if;

                     if Model =  Nondet_In_Type and then
                       Is_Scalar_Type (Given_Type)
                     then
                        if Print_Model then
                           Put_Line ("pragma Assume (" & Obj_Name_String &
                                       " in " & Type_Name_String &
                                       "'Range)");
                        end if;
                     end if;
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

         pragma Assert (Global_Symbol_Table.Contains (Subprog_Id),
                        "Make_Model_Section: Subprogram not in symbol table.");
         declare
            Subprog_Sym : Symbol := Global_Symbol_Table (Subprog_Id);
         begin
            Subprog_Sym.Value := Block;
            Global_Symbol_Table.Replace (Subprog_Id, Subprog_Sym);
         end;
      end Make_Model_Section;

      --  Get lists of all the inputs and outputs of the model
      --  subprogram including all those listed in a pragma Global.
      --  Presently the list of inputs is not used.

      Model_Inputs  : Elist_Id := No_Elist;
      Model_Outputs : Elist_Id := No_Elist;
      Global_Seen   : Boolean;
   begin
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

      Make_Model_Section (Model, Model_Outputs);
   end Make_Model;

   ---------------------------
   -- Make_Nondet_Function --
   ---------------------------

   procedure Make_Nondet_Function (Fun_Name, Result_Type : String;
                                   Statements : Irep;
                                   Loc : Source_Ptr) is
      Fun_Symbol_Id : constant Symbol_Id := Intern (Fun_Name);
   begin
      if not Global_Symbol_Table.Contains (Fun_Symbol_Id) then
         declare
            Ret : constant Irep := New_Irep (I_Code_Type);
            Type_Irep  : constant Irep :=
              Make_Symbol_Type (Identifier => Result_Type);

            Param_List : constant Irep := New_Irep (I_Parameter_List);
            --  For a nondet funcition the Param_List is always empty.

            Obj_Name  : constant String := "Res___" & Fun_Name;
            Obj_Id    : constant Symbol_Id := Intern (Obj_Name);
            Init_Expr : constant Irep := Ireps.Empty;

            Decl : constant Irep := New_Irep (I_Code_Decl);

            Block  : constant Irep   := New_Irep (I_Code_Block);

            Obj_Sym : constant Irep := New_Irep (I_Symbol_Expr);

            Return_Statement : constant Irep := New_Irep (I_Code_Return);
            Return_Value     : constant Irep := New_Irep (I_Symbol_Expr);

            Fun_Symbol : Symbol;

         begin
            Print_Modelling_Message
              ("Making nondet function " & Fun_Name &
                 " : " & Result_Type, Loc);

            Set_Return_Type (Ret, Type_Irep);
            Set_Parameters (Ret, Param_List);

            New_Subprogram_Symbol_Entry
              (Subprog_Name   => Fun_Symbol_Id,
               Subprog_Type   => Ret,
               A_Symbol_Table => Global_Symbol_Table);

            pragma Assert (Global_Symbol_Table.Contains (Fun_Symbol_Id));

            Set_Source_Location (Obj_Sym, Loc);
            Set_Identifier      (Obj_Sym, Obj_Name);
            Set_Type            (Obj_Sym, Type_Irep);

            New_Object_Symbol_Entry (Object_Name       => Obj_Id,
                                     Object_Type       => Type_Irep,
                                     Object_Init_Value => Init_Expr,
                                     A_Symbol_Table    => Global_Symbol_Table);

            Set_Source_Location (Decl, Loc);
            Set_Symbol (Decl, Obj_Sym);
            Append_Op (Block, Decl);

            Set_Source_Location (Return_Value, Loc);
            Set_Identifier (Return_Value, Obj_Name);
            Set_Type (Return_Value, Type_Irep);

            if Statements /= Ireps.Empty then
               Put_Line ("At the moment statements are not supported");
               null;  --  Todo append the statements
            end if;

            Set_Source_Location (Return_Statement, Loc);
            Set_Return_Value (Return_Statement, Return_Value);
            Append_Op (Block, Return_Statement);

            Fun_Symbol := Global_Symbol_Table (Fun_Symbol_Id);
            Fun_Symbol.Value := Block;
            --  Mark that this is a nondet function.
            Fun_Symbol.IsAuxiliary := True;
            Global_Symbol_Table.Replace (Fun_Symbol_Id, Fun_Symbol);
         end;
      else
         null;  --  The function has already been created previously.
      end if;
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

   -------------------------------
   -- Replace_Local_With_Import --
   -------------------------------

   function Replace_Local_With_Import
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
   end Replace_Local_With_Import;

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
