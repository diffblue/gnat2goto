with Elists;                  use Elists;
with Einfo;                   use Einfo;
with Sem_Prag;                use Sem_Prag;
with GOTO_Utils;              use GOTO_Utils;
with Symbol_Table_Info;       use Symbol_Table_Info;
with Tree_Walk;               use Tree_Walk;
with Range_Check;             use Range_Check;
with ASVAT_Modelling.Nondet;  use ASVAT_Modelling.Nondet;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Treepr;                  use Treepr;
with Text_IO;                 use Text_IO;

package body ASVAT_Modelling.Model_Variables is

   --------------------------
   -- Make_Model_Variables --
   --------------------------

   procedure Make_Model_Variables (E : Entity_Id; Model : Model_Sorts) is
      Source_Location : constant Irep := Get_Source_Location (E);
      Subprog_Body    : constant Irep := Make_Code_Block (Source_Location);

      --  Get lists of all the inputs and outputs of the model
      --  subprogram including all those listed in a pragma Global.
      --  Presently the list of inputs is not used.

      Model_Inputs  : Elist_Id := No_Elist;
      Model_Outputs : Elist_Id := No_Elist;
      Global_Seen   : Boolean;
      Iter          : Elmt_Id;
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
               Put_Line ("Curr_Entity " & Unique_Object_Name);
               Print_Node_Briefly (Curr_Entity);
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
                     Append_Op
                       (Subprog_Body,
                        Do_Nondet_Var (Var_Name => Unique_Object_Name,
                                       Var_Type => Unique_Type_Name,
                                       E        => E));

                     --  If the subprogram ASVAT model is "Nondet_In_Type",
                     --  an assume statment is appended to the subprogram body
                     if Model =  Nondet_In_Type_Vars then
                        --  If the object declaration is not visible
                        --  The local declaration will be used as the
                        --  modelling rules state that it must be
                        --  identical to the hidden declaration.
                        declare
                           --  If the object is an in out or out parameter
                           --  it must be derefereced.
                           Given_Type : constant Irep := Var_Symbol.SymType;

                           Sym : constant Irep :=
                             Make_Symbol_Expr
                               (Source_Location => Source_Location,
                                Identifier      => Unique_Object_Name,
                                I_Type          => Given_Type);

                           Var_Irep : constant Irep :=
                             (if Kind (Given_Type) = I_Pointer_Type then
                                   Make_Dereference_Expr
                                (Object          => Sym,
                                 Source_Location => Source_Location,
                                 I_Type          => Get_Subtype (Given_Type),
                                 Range_Check     => False)
                              else
                                 Sym);

                        begin
                           Put_Line ("Doing in type");
                           Print_Irep (Var_Irep);
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

      Put_Line ("The parameters are done");
      Print_Irep (Subprog_Body);
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

            --  Add a new block to the function for the Result_Var declaration
            Return_Block : constant Irep := Make_Code_Block (Source_Location);

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

            El_List : List_Cursor;

         begin
            Put_Line ("The unique namme of the result type is " &
                        Unique_Name (Etype (E)));
            Print_Irep (Result_Var_Irep);
            --  Insert the Result_Var into the symbol table.
            --  It should not already exist.
            pragma Assert (not Global_Symbol_Table.Contains (Result_Var_Id),
                           "Symbol table already contains " & Result_Var);
            New_Object_Symbol_Entry
              (Object_Name       => Result_Var_Id,
               Object_Type       => Result_Type_Sym.SymType,
               Object_Init_Value => Ireps.Empty,
               A_Symbol_Table    => Global_Symbol_Table);

            --  Add the declaration of the result varible to the new block
            Append_Op (Return_Block, Var_Decl);

            --  Set the result variable to nondet.
            Append_Op
              (Return_Block,
               Do_Nondet_Var (Var_Name => Result_Var,
                              Var_Type => Result_Type,
                              E        => E));
            --  if ASVAT model is "Nondet_In_Type", do an in type assumption.
            if Model = Nondet_In_Type_Vars then
               Make_Selector_Names
                 (Result_Var,
                  Result_Var_Irep,
                  Return_Block,
                  Etype (E),
                  E,
                  Get_Source_Location (E));
            end if;
            --  The function needs a return statement.
            Append_Op (Return_Block, Return_Statement);
            Print_Irep (Return_Block);
            Append_Op (Subprog_Body, Return_Block);
            El_List := List_First (Get_Op (Subprog_Body));

            Print_Irep (Subprog_Body);
            while List_Has_Element (Get_Op (Subprog_Body), El_List) loop
               Ireps.Print_Irep
                 (List_Element (Get_Op (Subprog_Body), El_List));
               El_List := List_Next (Get_Op (Subprog_Body), El_List);
            end loop;

         end;
      end if;

   end Make_Model_Variables;

   procedure Make_Nondet_Function_Body (Fun_Name, Result_Type : String;
                                        Statements : Irep;
                                        E          : Entity_Id);
   -------------------------------
   -- Make_Nondet_Function_Body --
   -------------------------------

   procedure Make_Nondet_Function_Body (Fun_Name, Result_Type : String;
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
   end Make_Nondet_Function_Body;

end ASVAT_Modelling.Model_Variables;
