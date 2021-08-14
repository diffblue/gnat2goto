with Uintp;                 use Uintp;
with Nlists;                use Nlists;
with Namet;                 use Namet;
with Einfo;                 use Einfo;
with Sem_Util;              use Sem_Util;
with Sem_Aux;               use Sem_Aux;
with GOTO_Utils;            use GOTO_Utils;
with Symbol_Table_Info;     use Symbol_Table_Info;
with Gnat2goto_Itypes;      use Gnat2goto_Itypes;
with Tree_Walk;             use Tree_Walk;
with Arrays;                use Arrays;
with ASVAT.Size_Model;      use ASVAT.Size_Model;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package body Records is

   --   Used for error recovery when a component is an unsupported array type.
   Dummy_Array_Size : constant Pos := 16;

   procedure Add_Entity_Substitution (E : Entity_Id; Subst : Irep);

   function Find_Record_Variant (Variant_Part : Node_Id;
                                 Actual_Disc : Node_Id) return Node_Id
   with Pre  => Nkind (Variant_Part) = N_Variant_Part,
        Post => Nkind (Find_Record_Variant'Result) = N_Variant;

   function Get_Fresh_Type_Name (Actual_Type : Irep; Associated_Node : Node_Id)
                                return Irep;

   function Get_Variant_Union_Member_Name (N : Node_Id) return String;

   --  Given an N_Variant_Part and then desired N_Variant,
   --  creates a union selector expression like
   --  .some_particular_value = {}
   --  where Union_Expr points to .some_particular_value
   --  and Struct_Expr points to {} (and empty struct expression
   --  of the right union component type)
   procedure Make_Variant_Literal (Variants : Node_Id;
                                   Chosen_Var : Node_Id;
                                   Union_Expr : out Irep;
                                   Struct_Expr : out Irep)
   with Pre  => Nkind (Variants) = N_Variant_Part and then
                Nkind (Chosen_Var) = N_Variant,
        Post => Kind (Union_Expr) = I_Union_Expr and then
                Kind (Struct_Expr) = I_Struct_Expr;

   procedure Remove_Entity_Substitution (E : Entity_Id);

   -----------------------------
   -- Add_Entity_Substitution --
   -----------------------------

   procedure Add_Entity_Substitution (E : Entity_Id; Subst : Irep) is
   begin
      Identifier_Substitution_Map.Insert (E, Subst);
   end Add_Entity_Substitution;

   ---------------------------------
   -- Do_Aggregate_Literal_Record --
   ---------------------------------

   function Do_Aggregate_Literal_Record (N : Node_Id) return Irep is
      N_Type : constant Entity_Id := Etype (N);
      N_Underlying_Type : constant Node_Id := Underlying_Type (N_Type);
   begin
      --  It appears GNAT sorts the aggregate members for us into the order
      --  discriminant (if any), common members, variant members.
      --  However, let's check.
      declare
         Components   : constant Node_Id :=
           Component_List (Type_Definition
                           (Parent
                              (Underlying_Type
                                   (Base_Type (N_Underlying_Type)))));
         Variant_Node : constant Node_Id := Variant_Part (Components);

         Component_Iter : Node_Id :=
           First_Component_Or_Discriminant (N_Underlying_Type);
         Actual_Iter    : Node_Id := First (Component_Associations (N));
         Struct_Expr : constant Irep := Make_Struct_Expr
           (Source_Location => Get_Source_Location (N),
            I_Type => Do_Type_Reference (N_Underlying_Type));
         Non_Discriminant_Components_Seen : Int := 0;
         pragma Assert (Present (Components));
         Non_Discriminant_Components_Expected : constant Int :=
           List_Length (Component_Items (Components));
         Variant_Disc_Value : Node_Id := Types.Empty;

         function Components_Match (L : Node_Id; R : Node_Id) return Boolean;
         function Components_Match (L : Node_Id; R : Node_Id) return Boolean
         is
            Lrec : constant Node_Id := Original_Record_Component (L);
            Rrec : constant Node_Id := Original_Record_Component (R);
         begin
            return Lrec = Rrec;
         end Components_Match;

         function Expect_More_Components return Boolean;
         function Expect_More_Components return Boolean is
         begin
            if Present (Component_Iter) and then
              Ekind (Component_Iter) = E_Discriminant
            then
               return True;
            end if;
            return Non_Discriminant_Components_Seen <
              Non_Discriminant_Components_Expected;
         end Expect_More_Components;

      begin

         --  Expect discriminants and components in declared order:
         while Expect_More_Components loop
            if not Present (Actual_Iter) then
               Report_Unhandled_Node_Empty (N,
                                            "Do_Aggregate_Literal_Record",
                                            "Actual iter not present");
               return Struct_Expr;
            end if;
            if not Present (Component_Iter) then
               Report_Unhandled_Node_Empty (N,
                                            "Do_Aggregate_Literal_Record",
                                            "Component iter not present");
               return Struct_Expr;
            end if;
            if not Components_Match (Component_Iter,
                                     Entity (First (Choices (Actual_Iter))))
            then
               Report_Unhandled_Node_Empty (N,
                                            "Do_Aggregate_Literal_Record",
                                            "Component actual iter mismatch");
               return Struct_Expr;
            end if;

            Append_Struct_Member (Struct_Expr,
                                  Do_Expression (Expression (Actual_Iter)));
            if Ekind (Component_Iter) = E_Component then
               Non_Discriminant_Components_Seen :=
                 Non_Discriminant_Components_Seen + 1;
            elsif Ekind (Component_Iter) = E_Discriminant then
               if Present (Variant_Node) and then
                 Original_Record_Component (Component_Iter) =
                 Entity (Name (Variant_Node))
               then
                  Variant_Disc_Value := Expression (Actual_Iter);
               end if;
            end if;
            Next_Component_Or_Discriminant (Component_Iter);
            Next (Actual_Iter);
         end loop;

         --  Extract variant members
         if Present (Variant_Node) then
            declare
               Variant_Found : constant Node_Id :=
                 Find_Record_Variant (Variant_Node, Variant_Disc_Value);
               Union_Literal : Irep;
               Variant_Substruct : Irep;
               pragma Assert (Present (Component_List (Variant_Found)));
               Substruct_Component_List : Node_Id :=
                 First (Component_Items (Component_List (Variant_Found)));
            begin
               if not Present (Variant_Found) then
                  Report_Unhandled_Node_Empty (N,
                                               "Do_Aggregate_Literal_Record",
                                               "Variant not present");
                  return Struct_Expr;
               end if;
               --  Initialises last two parameters:
               Make_Variant_Literal (Variant_Node, Variant_Found,
                                     Union_Literal, Variant_Substruct);
               --  Try to parse remaining aggregate parts according to that
               --  subrecord.
               while Present (Substruct_Component_List) loop
                  if not Present (Actual_Iter) then
                     Report_Unhandled_Node_Empty (N,
                                                 "Do_Aggregate_Literal_Record",
                                                  "Actual iter not present");
                     return Struct_Expr;
                  end if;

                  pragma Assert
                    (Nkind (Defining_Identifier (Substruct_Component_List)) in
                       N_Has_Chars);
                  pragma Assert
                    (Nkind (Entity (First (Choices (Actual_Iter)))) in
                       N_Has_Chars);
                  if Get_Name_String
                    (Chars (Defining_Identifier (Substruct_Component_List))) /=
                    Get_Name_String
                      (Chars (Entity (First (Choices (Actual_Iter)))))
                  then
                     Report_Unhandled_Node_Empty (N,
                                                 "Do_Aggregate_Literal_Record",
                                                  "Wrong defining identifier");
                     return Struct_Expr;
                  end if;
                  Append_Struct_Member (
                    Variant_Substruct,
                    Do_Expression (Expression (Actual_Iter)));
                  Next (Substruct_Component_List);
                  Next (Actual_Iter);
               end loop;
               --  Add union literal to the outer struct:
               Append_Struct_Member (Struct_Expr, Union_Literal);
            end;

         end if;
         return Struct_Expr;
      end;

   end Do_Aggregate_Literal_Record;

   ----------------------------------
   -- Do_Record_Object_Declaration --
   ----------------------------------

   procedure Do_Record_Object_Declaration
     (Block       : Irep;
      Dec_Node    : Node_Id;
      Target_Type : Entity_Id;
      Record_Name : String;
      Init_Expr   : Node_Id)
   is
      function Has_Defaulted_Components (E : Entity_Id) return Boolean;
      function Needs_Default_Initialisation (E : Entity_Id) return Boolean;
      function Disc_Expr (N : Node_Id) return Node_Id;
      function Make_Record_Default_Initialiser (E : Entity_Id;
                                                DCs : Node_Id) return Irep;
      function Make_Default_Initialiser (E : Entity_Id;
                                         DCs : Node_Id) return Irep;

      function Has_Defaulted_Components (E : Entity_Id) return Boolean is
         Record_E : Entity_Id := E;
         Record_Def : Node_Id;
         Component_Iter : Node_Id;
      begin
         while Ekind (Record_E) = E_Record_Subtype loop
            Record_E := Etype (Record_E);
         end loop;
         if Ekind (Record_E) /= E_Record_Type then
            return False;
         end if;
         Record_Def := Type_Definition (Parent (Record_E));
         if Nkind (Record_Def) /= N_Record_Definition and then
           Nkind (Record_Def) /= N_Variant
         then
            Report_Unhandled_Node_Empty (Dec_Node,
                                         "Do_Object_Declaration",
                                         "Record definition of wrong nkind");
            return False;
         end if;
         Component_Iter :=
           (if Present (Component_List (Record_Def)) then
               First (Component_Items (Component_List (Record_Def)))
            else
               Types.Empty);
         while Present (Component_Iter) loop
            if Present (Expression (Component_Iter)) then
               return True;
            end if;
            Next (Component_Iter);
         end loop;
         return False;
      end Has_Defaulted_Components;

      function Needs_Default_Initialisation (E : Entity_Id) return Boolean is
      begin
--           --  The record object will require default initialisation if:
--           --  1. It has defaulted discriminants,
--           --  2. It has defaulted components, or,
--         --  3. It is discriminated and it has no initialisation expression.
--           --     In this case it must be a constrained subtype of a
--           --     discriminated record.
         return Has_Defaulted_Discriminants (E)
           or else Has_Defaulted_Components (E);
--             or else (Has_Discriminants (E) and not Present (Init_Expr));
      end Needs_Default_Initialisation;

      function Disc_Expr (N : Node_Id) return Node_Id is
         (if Nkind (N) = N_Discriminant_Association
            then Expression (N)
            else N);

      function Make_Record_Default_Initialiser (E : Entity_Id;
                                                DCs : Node_Id) return Irep is

         procedure Add_Components (Components : Node_Id; Result : Irep);
         procedure Add_Components (Components : Node_Id; Result : Irep)
         is
            pragma Assert (Present (Components));
            Component_Iter : Node_Id :=
              First (Component_Items (Components));
            New_Expr : Irep;
         begin
            while Present (Component_Iter) loop
               if Nkind (Component_Iter) /= N_Allocator
                 and then Nkind (Component_Iter) /= N_Aspect_Specification
                 and then Nkind (Component_Iter) /= N_Assignment_Statement
                 and then Nkind (Component_Iter) /= N_At_Clause
                 and then
                 Nkind (Component_Iter) /= N_Attribute_Definition_Clause
                 and then Nkind (Component_Iter) /= N_Case_Expression
                 and then
                 Nkind (Component_Iter) /= N_Case_Expression_Alternative
                 and then Nkind (Component_Iter) /= N_Case_Statement
                 and then Nkind (Component_Iter) /= N_Code_Statement
                 and then Nkind (Component_Iter) /= N_Component_Association
                 and then Nkind (Component_Iter) /= N_Component_Declaration
                 and then Nkind (Component_Iter) /= N_Delay_Relative_Statement
                 and then Nkind (Component_Iter) /= N_Delay_Until_Statement
                 and then Nkind (Component_Iter) /= N_Delta_Aggregate
                 and then Nkind (Component_Iter) /= N_Discriminant_Association
                 and then
                 Nkind (Component_Iter) /= N_Discriminant_Specification
                 and then Nkind (Component_Iter) /= N_Exception_Declaration
                 and then Nkind (Component_Iter) /= N_Expression_Function
                 and then Nkind (Component_Iter) /= N_Expression_With_Actions
                 and then Nkind (Component_Iter) /= N_Free_Statement
                 and then
                 Nkind (Component_Iter) /= N_Iterated_Component_Association
                 and then Nkind (Component_Iter) /= N_Mod_Clause
                 and then Nkind (Component_Iter) /= N_Modular_Type_Definition
                 and then Nkind (Component_Iter) /= N_Number_Declaration
                 and then Nkind (Component_Iter) /= N_Object_Declaration
                 and then Nkind (Component_Iter) /= N_Parameter_Specification
                 and then
                 Nkind (Component_Iter) /= N_Pragma_Argument_Association
                 and then Nkind (Component_Iter) /= N_Qualified_Expression
                 and then Nkind (Component_Iter) /= N_Raise_Expression
                 and then Nkind (Component_Iter) /= N_Raise_Statement
                 and then Nkind (Component_Iter) /= N_Simple_Return_Statement
                 and then Nkind (Component_Iter) /= N_Type_Conversion
                 and then Nkind (Component_Iter) /= N_Unchecked_Expression
                 and then Nkind (Component_Iter) /= N_Unchecked_Type_Conversion
               then
                  Report_Unhandled_Node_Empty (Component_Iter,
                                             "Make_Record_Default_Initialiser",
                                               "Wrong component iter nkind");
                  return;
               end if;
               if Present (Expression (Component_Iter)) then
                  New_Expr := Do_Expression (Expression (Component_Iter));
               else
                  declare
                     Component_Type  : constant Entity_Id :=
                       Etype (Defining_Identifier (Component_Iter));
                     Is_Non_Static_Array : constant Boolean :=
                       Is_Array_Type (Component_Type) and then
                       not All_Dimensions_Static (Component_Type);
                     Comp_I_Type : constant Irep :=
                       (if Is_Non_Static_Array then
                           Make_Static_Array
                          (Dummy_Array_Size, Component_Type)
                        else
                           Do_Type_Reference (Component_Type));
                  begin
                     if Is_Record_Type (Component_Type) then
                        New_Expr :=
                          Make_Default_Initialiser (Component_Type,
                                                    Types.Empty);
                     else
                        New_Expr := Make_Side_Effect_Expr_Nondet
                          (I_Type => Comp_I_Type,
                           Source_Location => Get_Source_Location (E));
                     end if;
                  end;
               end if;
               Append_Struct_Member (Result, New_Expr);
               Next (Component_Iter);
            end loop;
         end Add_Components;

         Record_E : constant Entity_Id := Root_Type (E);
         Record_Def : constant Node_Id :=
           Type_Definition (Parent (Record_E));
         Record_Comps : constant Node_Id :=
           Component_List (Record_Def);
         Variant_Disc_Value : Node_Id;
         Ret : constant Irep := Make_Struct_Expr
           (Source_Location => Get_Source_Location (Dec_Node),
            I_Type => Do_Type_Reference (E));

      --  begin processing for Make_Record_Default_Initialiser

      begin
         if Has_Discriminants (E) then
            declare
               Iter : Entity_Id := First_Discriminant (E);
               Disc_Constraint_Iter : Node_Id := Types.Empty;
               Disc_Actual : Node_Id;
               New_Expr : Irep;
            begin
               if Present (DCs) then
                  Disc_Constraint_Iter := First (Constraints (DCs));
               end if;
               while Present (Iter) loop
                  if Present (DCs) then
                     if not Present (Disc_Constraint_Iter) then
                        Report_Unhandled_Node_Empty (Dec_Node,
                                             "Make_Record_Default_Initialiser",
                                           "Disc constraint iter not present");
                        return Ret;
                     end if;
                     Disc_Actual := Disc_Expr (Disc_Constraint_Iter);
                     Next (Disc_Constraint_Iter);
                  else
                     Disc_Actual := Discriminant_Default_Value (Iter);
                  end if;

                  --  If this assignment picks a variant, save the actual
                  --  value for later:
                  if Present (Variant_Part (Record_Comps)) and then
                    Entity (Name (Variant_Part (Record_Comps))) =
                      Original_Record_Component (Iter)
                  then
                     Variant_Disc_Value := Disc_Actual;
                  end if;

                  if Present (Disc_Actual) then
                     New_Expr := Do_Expression (Disc_Actual);
                  else
                     --  Default initialize to 0
                     New_Expr := Make_Constant_Expr
                       (Source_Location => Get_Source_Location (E),
                        I_Type          => Do_Type_Reference (Etype (Iter)),
                        Range_Check     => False,
                        Value           => "0");
                  end if;
                  Append_Struct_Member (Ret, New_Expr);
                  --  Substitute uses of the discriminant in the record
                  --  initialiser for its actual value:
                  Add_Entity_Substitution (Original_Record_Component (Iter),
                                           New_Expr);
                  Next_Discriminant (Iter);
               end loop;
            end;
         end if;

         --  Next defaulted components:
         Add_Components (Record_Comps, Ret);

         --  Now the variant part:
         if Present (Variant_Part (Record_Comps)) then
            --  Should have found the variant discriminant's
            --  actual value earlier:
            if not Present (Variant_Disc_Value) then
               Report_Unhandled_Node_Empty (Dec_Node,
                                            "Make_Record_Default_Initialiser",
                                        "Variant disc value iter not present");
               return Ret;
            end if;
            declare
               Var_Part : constant Node_Id := Variant_Part (Record_Comps);
               Variant : constant Node_Id :=
                 Find_Record_Variant (Var_Part, Variant_Disc_Value);
               Union_Expr : Irep;
               Substruct_Expr : Irep;
            begin
               if not Anonymous_Type_Map.Contains (Variant) then
                  Report_Unhandled_Node_Empty (Variant,
                                             "Make_Record_Default_Initialiser",
                                               "Variant not in type map");
                  return Ret;
               end if;
               --  Initialises the last two arguments:
               Make_Variant_Literal (Var_Part, Variant,
                                     Union_Expr, Substruct_Expr);
               --  Populate substructure:
               Add_Components (Component_List (Variant), Substruct_Expr);
               --  Add union initialiser to outer struct:
               Append_Struct_Member (Ret, Substruct_Expr);
            end;
         end if;

         --  Remove discriminant substitutions:
         if Has_Discriminants (E) then
            declare
               Iter : Entity_Id := First_Discriminant (E);
            begin
               while Present (Iter) loop
                  Remove_Entity_Substitution
                    (Original_Record_Component (Iter));
                  Next_Discriminant (Iter);
               end loop;
            end;
         end if;

         return Ret;

      end Make_Record_Default_Initialiser;

      function Make_Default_Initialiser (E : Entity_Id;
                                         DCs : Node_Id) return Irep
      is
      begin
         if Ekind (E) in Array_Kind then
            return Ireps.Empty; --  Make_Array_Default_Initialiser (E);
         elsif Ekind (E) in Record_Kind then
            return Make_Record_Default_Initialiser (E, DCs);
         elsif Ekind (E) in Private_Kind and then Present (Full_View (E))
           and then Ekind (Full_View (E)) in Array_Kind
         then
            return Ireps.Empty;
            --  Make_Array_Default_Initialiser (Full_View (E));
         else
            return Report_Unhandled_Node_Irep (E, "Make_Default_Initialiser",
                                                 "Unknown Ekind");
         end if;
      end Make_Default_Initialiser;

      Defn : constant Node_Id := Object_Definition (Dec_Node);

      Discriminant_Constraint : constant Node_Id :=
        (if Nkind (Defn) = N_Subtype_Indication then
              Constraint (Defn)
         else Types.Empty);
      --  Check for if initialization is required.
      Init_Expr_Irep : constant Irep :=
        (if Present (Init_Expr) then
            Do_Expression (Init_Expr)
         elsif Needs_Default_Initialisation (Target_Type) or
             (Present (Object_Definition (Dec_Node)) and then
              Nkind (Object_Definition (Dec_Node)) = N_Subtype_Indication)
         then
            Make_Default_Initialiser
           (Target_Type,
            Discriminant_Constraint)
         else
            Ireps.Empty);

      --  Do_Defining-Identifier cannot be called here because
      --  the object entity has not been entered into the symbol
      --  table yet.
      Id   : constant Irep :=
        Make_Symbol_Expr
          (Source_Location => Get_Source_Location (Dec_Node),
           I_Type          => Do_Type_Reference (Target_Type),
           Range_Check     => False,
           Identifier      => Record_Name);

   begin
      Do_Plain_Object_Declaration
        (Block       => Block,
         Object_Sym  => Id,
         Object_Name => Record_Name,
         Object_Def  => Defining_Identifier (Dec_Node),
         Init_Expr_Irep => Init_Expr_Irep);

      --  Assign the initialization, if any.
      if Init_Expr_Irep /= Ireps.Empty then
         Append_Op (Block, Make_Code_Assign
                    (Lhs => Id,
                     Rhs =>
                       Typecast_If_Necessary
                         (Init_Expr_Irep, Get_Type (Id),
                          Global_Symbol_Table),
                     Source_Location => Get_Source_Location (Dec_Node)));
      end if;
   end Do_Record_Object_Declaration;

   -------------------------------
   -- Do_Record_Type_Definition --
   -------------------------------

   function Do_Record_Type_Definition (N : Node_Id;
                                       Discs : List_Id) return Irep is
      --  Set up accumulators to record ASVAT.Model_Size.
      Is_Static    : Boolean := True;
      Static_Size  : Natural := 0;
      Dynamic_Size : Irep := Integer_Constant_To_Expr
        (Value           => Uint_0,
         Expr_Type       => Make_Signedbv_Type (32),
         Source_Location => Get_Source_Location (N));

      function Do_Record_Definition (N : Node_Id; Discs : List_Id) return Irep;

      --------------------------
      -- Do_Record_Definition --
      --------------------------

      function Do_Record_Definition (N : Node_Id;
                                     Discs : List_Id) return Irep is

         Components : constant Irep := Make_Struct_Union_Components;
         Disc_Iter : Node_Id := First (Discs);

         procedure Add_Record_Component (Comp_Name : String;
                                         Comp_Type_Node : Node_Id;
                                         Comp_Node : Node_Id;
                                         Add_To_List : Irep := Components);
         procedure Add_Record_Component_Raw (Comp_Name : String;
                                             Comp_Type : Irep;
                                             Comp_Node : Node_Id;
                                             Add_To_List : Irep := Components);
         procedure Do_Record_Component (Comp : Node_Id);
         procedure Do_Variant_Struct (Var : Node_Id;
                                      Union_Components : Irep);

         --------------------------
         -- Add_Record_Component --
         --------------------------

         procedure Add_Record_Component (Comp_Name : String;
                                         Comp_Type_Node : Node_Id;
                                         Comp_Node : Node_Id;
                                         Add_To_List : Irep := Components) is
            Is_Non_Static_Array : constant Boolean :=
              Is_Array_Type (Comp_Type_Node) and then
              not (All_Dimensions_Static (Comp_Type_Node));
         begin
            if Is_Non_Static_Array then
               if Is_Constrained (Etype (Comp_Type_Node)) then
                  Report_Unhandled_Node_Empty
                    (N        => Comp_Node,
                     Fun_Name => "Add_Record_Component",
                     Message  =>
                       "Array components with non-static bounds " &
                       "are unsupported");
               else
                  Report_Unhandled_Node_Empty
                    (N        => Comp_Node,
                     Fun_Name => "Add_Record_Component",
                     Message  =>
                       "Unconstrained array (sub)type components " &
                       "are unsupported");
               end if;
            end if;

            Declare_Itype (Comp_Type_Node);

            declare
               Comp_Type_Irep      : constant Irep :=
                 (if Is_Non_Static_Array then
                  --  Use a dummy static array type to improve error recovery.
                     Make_Static_Array (Dummy_Array_Size, Comp_Type_Node)
                  else
                     Do_Type_Reference (Comp_Type_Node));
            begin
               Add_Record_Component_Raw (Comp_Name,
                                         Comp_Type_Irep,
                                         Comp_Node,
                                         Add_To_List);

               ASVAT.Size_Model.Accumumulate_Size
                 (Is_Static     => Is_Static,
                  Accum_Static  => Static_Size,
                  Accum_Dynamic => Dynamic_Size,
                  Entity_To_Add => Comp_Type_Node);
            end;
         end Add_Record_Component;

         ------------------------------
         -- Add_Record_Component_Raw --
         ------------------------------

         procedure Add_Record_Component_Raw (Comp_Name : String;
                                             Comp_Type : Irep;
                                             Comp_Node : Node_Id;
                                             Add_To_List : Irep := Components)
         is
            Comp_Irep : constant Irep :=
              Make_Struct_Component (Comp_Name, Comp_Type);
         begin
            Set_Source_Location (Comp_Irep, Get_Source_Location (Comp_Node));
            Append_Component (Add_To_List, Comp_Irep);
         end Add_Record_Component_Raw;

         -------------------------
         -- Do_Record_Component --
         -------------------------

         procedure Do_Record_Component (Comp : Node_Id) is
            Comp_Name : Unbounded_String;
         begin
            if Nkind (Comp) /= N_Component_Declaration
              and then Nkind (Comp) /= N_Defining_Program_Unit_Name
              and then Nkind (Comp) /= N_Discriminant_Specification
              and then Nkind (Comp) /= N_Entry_Body
              and then Nkind (Comp) /= N_Entry_Declaration
              and then Nkind (Comp) /= N_Entry_Index_Specification
              and then Nkind (Comp) /= N_Exception_Declaration
              and then Nkind (Comp) /= N_Exception_Renaming_Declaration
              and then Nkind (Comp) /= N_Formal_Object_Declaration
              and then Nkind (Comp) /= N_Formal_Package_Declaration
              and then Nkind (Comp) /= N_Formal_Type_Declaration
              and then Nkind (Comp) /= N_Full_Type_Declaration
              and then Nkind (Comp) /= N_Implicit_Label_Declaration
              and then Nkind (Comp) /= N_Incomplete_Type_Declaration
              and then Nkind (Comp) /= N_Iterated_Component_Association
              and then Nkind (Comp) /= N_Iterator_Specification
              and then Nkind (Comp) /= N_Loop_Parameter_Specification
              and then Nkind (Comp) /= N_Number_Declaration
              and then Nkind (Comp) /= N_Object_Declaration
              and then Nkind (Comp) /= N_Object_Renaming_Declaration
              and then Nkind (Comp) /= N_Package_Body_Stub
              and then Nkind (Comp) /= N_Parameter_Specification
              and then Nkind (Comp) /= N_Private_Extension_Declaration
              and then Nkind (Comp) /= N_Private_Type_Declaration
              and then Nkind (Comp) /= N_Protected_Body
              and then Nkind (Comp) /= N_Protected_Body_Stub
              and then Nkind (Comp) /= N_Protected_Type_Declaration
              and then Nkind (Comp) /= N_Single_Protected_Declaration
              and then Nkind (Comp) /= N_Single_Task_Declaration
              and then Nkind (Comp) /= N_Subtype_Declaration
              and then Nkind (Comp) /= N_Task_Body
              and then Nkind (Comp) /= N_Task_Body_Stub
              and then Nkind (Comp) /= N_Task_Type_Declaration
            then
               Report_Unhandled_Node_Empty (Comp, "Do_Record_Component",
                                            "Wrong component nkind");
               return;
            end if;
            Comp_Name := To_Unbounded_String (Unique_Name (Defining_Identifier
                                              (Comp)));
            Add_Record_Component (To_String (Comp_Name),
                                  Etype (Defining_Identifier (Comp)),
                                  Comp);
         end Do_Record_Component;

         -----------------------
         -- Do_Variant_Struct --
         -----------------------

         procedure Do_Variant_Struct (Var : Node_Id;
                                      Union_Components : Irep) is
            Struct_Type : constant Irep :=
              Do_Record_Definition (Var, List_Id (Types.Empty));
            Type_Symbol : constant Irep :=
              Get_Fresh_Type_Name (Struct_Type, Var);
            Choice_Iter : constant Node_Id := First (Discrete_Choices (Var));
            Variant_Name : constant String :=
              Get_Variant_Union_Member_Name (Choice_Iter);
         begin
            Set_Tag (Struct_Type, Get_Identifier (Type_Symbol));
            Add_Record_Component_Raw (Variant_Name,
                                      Type_Symbol,
                                      Var,
                                      Union_Components);
         end Do_Variant_Struct;

         --  Local variables
         Comp_List      : constant Node_Id := Component_List (N);
         Component_Iter : Node_Id :=
           (if Present (Comp_List) then
                 First (Component_Items (Comp_List))
            else
               Types.Empty);
         Variants_Node  : constant Node_Id :=
           (if Present (Comp_List) then
                 Variant_Part (Comp_List)
            else
               Types.Empty);

         --  Start of processing for Do_Record_Definition

      begin
         --  Create fields for any discriminants:
         --  This order (discriminant, common fields, variant fields)
         --  seems to match GNAT's record-literal ordering (apparently
         --  regardless of source ordering).
         while Present (Disc_Iter) loop
            Add_Record_Component
              (Unique_Name (Defining_Identifier (Disc_Iter)),
               Etype (Discriminant_Type (Disc_Iter)),
               Disc_Iter);
            Next (Disc_Iter);
         end loop;

         --  Add regular fields
         while Present (Component_Iter) loop
            Do_Record_Component (Component_Iter);
            Next (Component_Iter);
         end loop;

         --  Add union of variants if applicable
         if Present (Variants_Node) then
            --  Create a field for the discriminant,
            --  plus a union of variant alternatives.
            declare
               Variant_Iter : Node_Id := First (Variants (Variants_Node));
               Union_Components : constant Irep :=
                 Make_Struct_Union_Components;
               Union_Irep : constant Irep := Make_Union_Type
                 (Tag => "Filled out further down with get_fresh_type_name",
                  Components => Union_Components);
            begin
               while Present (Variant_Iter) loop
                  Do_Variant_Struct (Variant_Iter, Union_Components);
                  Next (Variant_Iter);
               end loop;
               Set_Components (Union_Irep, Union_Components);
               declare
                  Union_Symbol : constant Irep :=
                    Get_Fresh_Type_Name (Union_Irep, Variants_Node);
               begin
                  Set_Tag (Union_Irep, Get_Identifier (Union_Symbol));
                  Add_Record_Component_Raw
                    ("_variants", Union_Symbol, Variants_Node);
               end;
            end;
         end if;

         return Make_Struct_Type
           (Tag => "This will be filled in later by Do_Type_Declaration",
            Components => Components);
      end Do_Record_Definition;

      The_Record : constant Irep := Do_Record_Definition (N, Discs);

   begin  --  Do_Record_Type_Definition.
      --  Set the ASVAT model size of the record.
      if Is_Static then
         ASVAT.Size_Model.Set_Static_Size
           (E          => Defining_Identifier (Parent (N)),
            Model_Size => Static_Size);
      else
         ASVAT.Size_Model.Set_Computed_Size
           (E         =>  Defining_Identifier (Parent (N)),
            Size_Expr => Dynamic_Size);
      end if;
      return The_Record;
   end Do_Record_Type_Definition;

   ---------------------------
   -- Do_Selected_Component --
   ---------------------------

   function Do_Selected_Component (N : Node_Id) return Irep is
      --  The prefix to a selected component may be an access to an
      --  record object which must be implicitly dereferenced.
      The_Prefix        : constant Node_Id := Prefix (N);
      Prefix_Etype      : constant Node_Id := Etype (The_Prefix);
      Prefix_Irep       : constant Irep := Do_Expression (The_Prefix);
      Is_Implicit_Deref : constant Boolean := Is_Access_Type (Prefix_Etype);
      Resolved_Type     : constant Irep :=
        (if Is_Implicit_Deref then
            Do_Type_Reference (Designated_Type (Prefix_Etype))
         else
            Do_Type_Reference (Prefix_Etype));
      Root            : constant Irep :=
        (if Is_Implicit_Deref then
            Make_Dereference_Expr
           (I_Type => Resolved_Type,
            Object => Prefix_Irep,
            Source_Location => Get_Source_Location (N))
         else
            Prefix_Irep);

      --  Where required the prefix has been implicitly dereferenced.
      Component      : constant Entity_Id := Entity (Selector_Name (N));

      --  Example:
      --  struct Foo { int a; };
      --  struct Foo bar;
      --  bar.a = 5;
      --
      --  The Unique_Name of the struct-component in declaration is Foo__a. But
      --  when parsing the assignment the component is that of the entity bar,
      --  thus it's unique-name is bar__a: which is not present in Foo. That's
      --  why we have to use the component of the original-record to get the
      --  right name.
      Orig_Component      : constant Entity_Id :=
        Original_Record_Component (Component);
      Comp_Etype          : constant Entity_Id := Etype (Component);
      Component_Type      : constant Irep := Do_Type_Reference (Comp_Etype);
      Component_Name      : constant String := Unique_Name (Orig_Component);
      Source_Location     : constant Irep := Get_Source_Location (N);
   begin
      if Do_Discriminant_Check (N) then
         --  ??? Can this even happen
         if Nkind (Parent (Etype (Prefix (N)))) /= N_Full_Type_Declaration
         then
            return Report_Unhandled_Node_Irep
              (N,
               "Do_Selected_Component",
               "Parent not full type declaration");
         end if;

         declare
            Record_Type : constant Node_Id := Type_Definition
              (Parent (Etype (Prefix (N))));
            Variant_Spec : constant Node_Id := Variant_Part
              (Component_List (Record_Type));

            function Find_Variant_Containing_Component return Node_Id;
            function Find_Variant_Containing_Component return Node_Id
            is
               Variant_Iter : Node_Id := First
                 (Variants (Variant_Part (Component_List (Record_Type))));
            begin
               while Present (Variant_Iter) loop
                  declare
                     pragma Assert (Present (Component_List (Variant_Iter)));
                     Item_Iter : Node_Id :=
                       First (Component_Items (Component_List (Variant_Iter)));
                  begin
                     while Present (Item_Iter) loop
                        if Defining_Identifier (Item_Iter) = Component then
                           return Variant_Iter;
                        end if;
                        Next (Item_Iter);
                     end loop;
                  end;
                  Next (Variant_Iter);
               end loop;
               --  XXX this was previously an "unsupported feature",
               --     not sure if/how this could ever happen
               pragma Assert
                 (False,
                  "A component not being present in any " &
                    "of the record variants shouldn't ever happen");
               return Variant_Iter;
            end Find_Variant_Containing_Component;

            Variant_Containing_Component : constant Node_Id :=
              Find_Variant_Containing_Component;

            Variant_Containing_Component_Constraint : constant Node_Id :=
              First (Discrete_Choices (Variant_Containing_Component));

            --  Emit a runtime check to see if we're actually accessing
            --  a component of the active variant
            Disc_Selector : constant Irep := Make_Member_Expr
              (Compound => Root,
               Component_Name => Unique_Name (Entity (Name (Variant_Spec))),
               I_Type => Do_Type_Reference (Etype (Name (Variant_Spec))),
               Source_Location => Source_Location);
            Disc_Check : constant Irep := Make_Op_Eq
              (Lhs => Disc_Selector,
               Rhs => Do_Expression (Variant_Containing_Component_Constraint),
               I_Type => CProver_Bool_T,
               Source_Location => Source_Location);
            Correct_Variant_Check : constant Irep :=
              Make_Runtime_Check (Disc_Check);

            --  Create the actual member access by interposing a union access:
            --  The actual access for member X of the Y == Z variant will look
            --  like (_check(Base.Disc == Z), Base._variants.Z.X)

            Union_Selector : constant Irep := Make_Member_Expr
              (I_Type => Anonymous_Type_Map (Variant_Spec),
               Compound => Root,
               Component_Name => "_variants",
               Source_Location => Source_Location);

            Substruct_Selector : constant Irep := Make_Member_Expr
              (I_Type => Anonymous_Type_Map (Variant_Containing_Component),
               Compound => Union_Selector,
               Component_Name => Get_Variant_Union_Member_Name
                 (Variant_Containing_Component_Constraint),
               Source_Location => Source_Location);

            Component_Selector : constant Irep := Make_Member_Expr
              (I_Type => Component_Type,
               Compound => Substruct_Selector,
               Component_Name => Component_Name,
               Source_Location => Source_Location);
         begin
            return Make_Op_Comma
              (Lhs => Correct_Variant_Check,
               Rhs => Component_Selector,
               Source_Location => Source_Location);
         end;
      else
         return Make_Member_Expr
           (I_Type => Component_Type,
            Compound => Root,
            Component_Name => Component_Name,
            Source_Location => Source_Location);
      end if;
   end Do_Selected_Component;

   -------------------------
   -- Find_Record_Variant --
   -------------------------

   --  Tries to find an N_Variant that applies when discriminant == Actual_Disc
   function Find_Record_Variant (Variant_Part : Node_Id;
                                 Actual_Disc : Node_Id) return Node_Id
   is
      Variant_Iter : Node_Id := First (Variants (Variant_Part));
   begin
      while Present (Variant_Iter) loop
         declare
            Choice_Iter : Node_Id :=
              First (Discrete_Choices (Variant_Iter));
         begin
            while Present (Choice_Iter) loop
               if Entity (Choice_Iter) = Entity (Actual_Disc) then
                  return Variant_Iter;
               end if;
               Next (Choice_Iter);
            end loop;
            Next (Variant_Iter);
         end;
      end loop;
      --  Not found?
      return Types.Empty;
   end Find_Record_Variant;

   -------------------------
   -- Get_Fresh_Type_Name --
   -------------------------

   function Get_Fresh_Type_Name (Actual_Type : Irep;
                                 Associated_Node : Node_Id) return Irep
   is
      Number_Str_Raw : constant String :=
        Integer'Image (Anonymous_Type_Counter);
      Number_Str : constant String :=
        Number_Str_Raw (2 .. Number_Str_Raw'Last);
      Fresh_Name : constant String := "__anonymous_type_" & Number_Str;
      Fresh_Symbol_Type : constant Irep := Make_Symbol_Type
        (Identifier => Fresh_Name);
   begin
      Anonymous_Type_Counter := Anonymous_Type_Counter + 1;

      New_Type_Symbol_Entry (Type_Name      => Intern (Fresh_Name),
                             Type_Of_Type   => Actual_Type,
                             A_Symbol_Table => Global_Symbol_Table);

      Anonymous_Type_Map.Insert (Associated_Node, Fresh_Symbol_Type);

      return Fresh_Symbol_Type;
   end Get_Fresh_Type_Name;

   -----------------------------------
   -- Get_Variant_Union_Member_Name --
   -----------------------------------

   function Get_Variant_Union_Member_Name (N : Node_Id) return String
   is
      Constraint_Iter : Node_Id := N;
      Variant_Name : Unbounded_String;
   begin
      while Present (Constraint_Iter) loop
         if Nkind (Constraint_Iter) in N_Has_Chars then
            Append (Variant_Name,
                    "_" & Get_Name_String (Chars (Constraint_Iter)));
         elsif Nkind (Constraint_Iter) = N_Others_Choice then
            Append (Variant_Name,
                    "_others");
         else
            Report_Unhandled_Node_Empty
              (N        => Constraint_Iter,
               Fun_Name => "Get_Variant_Union_Member_Name",
               Message  => "Unexpected constraint node");
            Append (Variant_Name,
                    "_$unknown_constraint");
         end if;
         Next (Constraint_Iter);
      end loop;
      return To_String (Variant_Name);
   end Get_Variant_Union_Member_Name;

   ----------------------------
   --  Make_Variant_Literal  --
   ----------------------------

   procedure Make_Variant_Literal (Variants : Node_Id;
                                   Chosen_Var : Node_Id;
                                   Union_Expr : out Irep;
                                   Struct_Expr : out Irep) is
      Variant_Union_Name : constant String :=
        Get_Variant_Union_Member_Name (First (Discrete_Choices (Chosen_Var)));
   begin
      Struct_Expr := Make_Struct_Expr
        (I_Type => Anonymous_Type_Map.Element (Chosen_Var),
         Source_Location => 0);
      Union_Expr := Make_Union_Expr
        (I_Type          => Anonymous_Type_Map.Element (Variants),
         Component_Name  => Variant_Union_Name,
         Source_Location => 0,
         Op0             => Struct_Expr);
   end Make_Variant_Literal;

   --------------------------------
   -- Remove_Entity_Substitution --
   --------------------------------

   procedure Remove_Entity_Substitution (E : Entity_Id) is
   begin
      Identifier_Substitution_Map.Delete (E);
   end Remove_Entity_Substitution;

end Records;
