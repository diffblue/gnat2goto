with Einfo;                 use Einfo;
with Namet;                 use Namet;
with Nlists;                use Nlists;
with Sem_Util;              use Sem_Util;
with Snames;                use Snames;
with Stand;                 use Stand;
with Treepr;                use Treepr;
with Uintp;                 use Uintp;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Follow;                use Follow;
with GNAT_Utils;            use GNAT_Utils;
with GOTO_Utils;            use GOTO_Utils;
with Uint_To_Binary;        use Uint_To_Binary;

package body Tree_Walk is

   procedure Append_Declare_And_Init
     (Symbol : Irep; Value : Irep; Block : Irep; Source_Loc : Source_Ptr);

   procedure Declare_Itype (Ty : Entity_Id);

   function Do_Address_Of (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Attribute_Reference,
        Post => Kind (Do_Address_Of'Result) = I_Address_Of_Expr;

   function Do_Aggregate_Literal (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Aggregate;

   function Do_Aggregate_Literal_Array (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Aggregate;

   function Do_Aggregate_Literal_Record (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Aggregate,
        Post => Kind (Do_Aggregate_Literal_Record'Result) = I_Struct_Expr;

   function Do_Array_Assignment (LHS_Expr : Irep; RHS_Expr : Irep; N : Node_Id)
                                return Irep
   with Post => Kind (Do_Array_Assignment'Result) = I_Code_Block;

   function Do_Array_Range (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Range;

   function Do_Assignment_Statement (N  : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Assignment_Statement,
        Post => Kind (Do_Assignment_Statement'Result) in
                I_Code_Assign | I_Code_Block;

   function Do_Bare_Range_Constraint (Range_Expr : Node_Id; Underlying : Irep)
                                     return Irep
   with Pre => Nkind (Range_Expr) = N_Range;

   function Do_Call_Parameters (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Procedure_Call_Statement | N_Function_Call,
        Post => Kind (Do_Call_Parameters'Result) = I_Argument_List;

   function Do_Case_Expression (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_Case_Expression,
        Post => Kind (Do_Case_Expression'Result) = I_Let_Expr;

   function Do_Constant (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_Integer_Literal,
        Post => Kind (Do_Constant'Result) = I_Constant_Expr;

   function Do_Constrained_Array_Definition (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Array_Type_Definition,
        Post => Kind (Do_Constrained_Array_Definition'Result) = I_Struct_Type;

   function Do_Defining_Identifier (E : Entity_Id) return Irep
   with Pre  => Nkind (E) = N_Defining_Identifier,
        Post => Kind (Do_Defining_Identifier'Result) in
           I_Symbol_Expr | I_Dereference_Expr;

   function Do_Dereference (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Explicit_Dereference,
        Post => Kind (Do_Dereference'Result) = I_Dereference_Expr;

   function Do_Derived_Type_Definition (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Derived_Type_Definition,
        Post => Kind (Do_Derived_Type_Definition'Result) in Class_Type;

   function Do_Enumeration_Definition (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Enumeration_Type_Definition,
        Post => Kind (Do_Enumeration_Definition'Result) = I_C_Enum_Type;

   function Do_Expression (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Subexpr,
        Post => Kind (Do_Expression'Result) in Class_Expr;

   procedure Do_Full_Type_Declaration (N : Node_Id)
   with Pre => Nkind (N) = N_Full_Type_Declaration;

   function Do_Function_Call (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Function_Call,
        Post => Kind (Do_Function_Call'Result) in Class_Expr;

   function Do_Handled_Sequence_Of_Statements (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Handled_Sequence_Of_Statements,
        Post => Kind (Do_Handled_Sequence_Of_Statements'Result) = I_Code_Block;

   function Do_Identifier (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Identifier,
        Post => Kind (Do_Identifier'Result) in
           I_Symbol_Expr | I_Dereference_Expr;

   function Do_If_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_If_Statement,
        Post => Kind (Do_If_Statement'Result) = I_Code_Ifthenelse;

   function Do_Indexed_Component (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Indexed_Component;

   function Do_Index_Or_Discriminant_Constraint
     (N : Node_Id; Underlying : Irep) return Irep
   with Pre  => Nkind (N) = N_Index_Or_Discriminant_Constraint;

   function Do_Itype_Array_Subtype (N : Entity_Id) return Irep
   with Pre => Is_Itype (N) and then Ekind (N) = E_Array_Subtype;

   function Do_Itype_Definition (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_Defining_Identifier;

   procedure Do_Itype_Reference (N : Node_Id)
   with Pre => Nkind (N) = N_Itype_Reference;

   function Do_Loop_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Loop_Statement,
        Post => Kind (Do_Loop_Statement'Result) in Class_Code;

   procedure Do_Object_Declaration (N : Node_Id; Block : Irep)
   with Pre => Nkind (N) = N_Object_Declaration
                 and then Kind (Block) = I_Code_Block;

   function Do_Op_Concat (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_Op_Concat;

   function Do_Operator_Simple (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Op,
        Post => Kind (Do_Operator_Simple'Result) in Class_Expr;

   function Do_Operator_General (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Op;

   function Do_Procedure_Call_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Procedure_Call_Statement,
        Post => Kind (Do_Procedure_Call_Statement'Result) =
                  I_Code_Function_Call;

   function Do_Range_Constraint (N : Node_Id; Underlying : Irep) return Irep;

   function Do_Record_Definition (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Record_Definition | N_Variant,
        Post => Kind (Do_Record_Definition'Result) = I_Struct_Type;

   function Do_Selected_Component (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Selected_Component,
        Post => Kind (Do_Selected_Component'Result) in
          I_Member_Expr | I_Op_Comma;

   function Do_Signed_Integer_Definition (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Signed_Integer_Type_Definition,
        Post => Kind (Do_Signed_Integer_Definition'Result) =
                  I_Bounded_Signedbv_Type;

   function Do_Simple_Return_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Simple_Return_Statement,
        Post => Kind (Do_Simple_Return_Statement'Result) = I_Code_Return;

   function Do_Slice (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Slice;

   procedure Do_Subprogram_Declaration (N : Node_Id)
   with Pre => Nkind (N) = N_Subprogram_Declaration;

   procedure Do_Subprogram_Body (N : Node_Id)
   with Pre => Nkind (N) = N_Subprogram_Body;

   function Do_Subprogram_Or_Block (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Subprogram_Body |
                             N_Task_Body       |
                             N_Block_Statement |
                             N_Package_Body    |
                             N_Entry_Body,
        Post => Kind (Do_Subprogram_Or_Block'Result) = I_Code_Block;

   function Do_Subprogram_Specification (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Subprogram_Specification,
        Post => Kind (Do_Subprogram_Specification'Result) = I_Code_Type;

   procedure Do_Subtype_Declaration (N : Node_Id)
   with Pre => Nkind (N) = N_Subtype_Declaration;

   function Do_Subtype_Indication (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Subtype_Indication,
        Post => Kind (Do_Subtype_Indication'Result) in Class_Type;

   function Do_Type_Conversion (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Type_Conversion,
        Post => Kind (Do_Type_Conversion'Result) in Class_Expr;

   procedure Do_Type_Declaration (New_Type_In : Irep; E : Entity_Id)
   with Pre => Is_Type (E) and then
               Kind (New_Type_In) in Class_Type;

   function Do_Type_Definition (N : Node_Id) return Irep;

   function Do_Type_Reference (E : Entity_Id) return Irep
   with Pre  => Is_Type (E),
        Post => Kind (Do_Type_Reference'Result) in Class_Type;

   function Do_Unconstrained_Array_Definition (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Array_Type_Definition,
        Post => Kind (Do_Unconstrained_Array_Definition'Result) =
                I_Struct_Type;

   function Get_Array_Component_Type (N : Node_Id) return Entity_Id
   with Post => Is_Type (Get_Array_Component_Type'Result);

   function Get_Array_Copy_Function (LHS_Element_Type : Entity_Id;
                                     RHS_Element_Type : Entity_Id;
                                     Index_Type : Entity_Id) return Irep
   with Post => Kind (Get_Array_Copy_Function'Result) = I_Symbol_Expr;

   function Get_Array_Dup_Function (Element_Type : Entity_Id;
                                    Index_Type : Entity_Id) return Irep
   with Post => Kind (Get_Array_Dup_Function'Result) = I_Symbol_Expr;

   function Get_Array_Index_Type (N : Node_Id) return Entity_Id
   with Post => Ekind (Get_Array_Index_Type'Result) = E_Signed_Integer_Type;

   function Get_Fresh_Type_Name (Actual_Type : Irep; Associated_Node : Node_Id)
                                return Irep;

   function Get_Variant_Union_Member_Name (N : Node_Id) return String;

   function Make_Array_First_Expr
     (Base_Type : Node_Id; Base_Irep : Irep) return Irep;

   function Make_Array_Index_Op
     (Base_Irep : Irep; Base_Type : Node_Id; Idx_Irep : Irep) return Irep;

   function Make_Array_Length_Expr
     (Array_Struct : Irep; Index_Type : Entity_Id) return Irep
   with Pre => Ekind (Index_Type) in Discrete_Kind;

   function Make_Increment
     (Sym : Irep; Sym_Type : Node_Id; Amount : Integer) return Irep;

   function Make_Integer_Constant (Val : Integer; Ty : Node_Id) return Irep;

   function Make_Pointer_Index (Base : Irep; Idx : Irep) return Irep;

   function Make_Runtime_Check (Condition : Irep) return Irep
   with Pre  => Kind (Get_Type (Condition)) = I_Bool_Type,
        Post => Kind (Make_Runtime_Check'Result) =
                I_Side_Effect_Expr_Function_Call;

   function Make_Struct_Component (Name : String; Ty : Irep) return Irep;

   function Make_Type_Symbol (Name : Symbol_Id; Defn : Irep) return Symbol;

   function Maybe_Make_Typecast (Expr     : Irep;
                                 Old_Type : Entity_Id;
                                 New_Type : Entity_Id) return Irep;

   procedure Process_Statement (N : Node_Id; Block : Irep)
   with Pre => Kind (Block) = I_Code_Block;
   --  Process statement or declaration

   function Process_Statements (L : List_Id) return Irep
   with Post => Kind (Process_Statements'Result) = I_Code_Block;
   --  Process list of statements or declarations

   -----------------------------
   -- Append_Declare_And_Init --
   -----------------------------

   procedure Append_Declare_And_Init
     (Symbol : Irep; Value : Irep; Block : Irep; Source_Loc : Source_Ptr)
   is
   begin
      Append_Op (Block, Make_Code_Decl (Symbol => Symbol,
                                        Source_Location => Source_Loc));
      Append_Op (Block, Make_Code_Assign (Lhs => Symbol,
                                          Rhs => Value,
                                          Source_Location => Source_Loc));
   end Append_Declare_And_Init;

   -------------------
   -- Declare_Itype --
   -------------------

   procedure Declare_Itype (Ty : Entity_Id) is
   begin
      if Is_Itype (Ty) then
         declare
            Ty_Name : constant Symbol_Id := Intern (Unique_Name (Ty));
            Ty_Symbol : Symbol;
            Ty_Cursor : Symbol_Maps.Cursor;
            Ty_New : Boolean;
         begin
            Global_Symbol_Table.Insert (Ty_Name, Ty_Symbol, Ty_Cursor, Ty_New);
            if Ty_New then
               declare
                  New_Type : constant Irep := Do_Itype_Definition (Ty);
                  New_Symbol : constant Symbol :=
                    Make_Type_Symbol (Ty_Name, New_Type);
               begin
                  Global_Symbol_Table.Replace_Element (Ty_Cursor, New_Symbol);
               end;
            end if;
         end;
      end if;
   end Declare_Itype;

   -------------------
   -- Do_Address_Of --
   -------------------

   function Do_Address_Of (N : Node_Id) return Irep is
     (Make_Address_Of (Do_Expression (Prefix (N))));

   --------------------------
   -- Do_Aggregate_Literal --
   --------------------------

   function Do_Aggregate_Literal (N : Node_Id) return Irep is
      N_Type : constant Entity_Id := Etype (N);
      --  TOCHECK: Parent type may be more than one step away?
   begin
      case Ekind (N_Type) is
         when E_Array_Type =>
            return Do_Aggregate_Literal_Array (N);
         when E_Array_Subtype =>
            return Do_Aggregate_Literal_Array (N);
         when E_Record_Subtype =>
            return Do_Aggregate_Literal_Record (N);
         when E_Record_Type =>
            return Do_Aggregate_Literal_Record (N);
         when others =>
            --  Unhandled aggregate kind
            pp (Union_Id (N));
            raise Program_Error;
      end case;
   end Do_Aggregate_Literal;

   --------------------------------
   -- Do_Aggregate_Literal_Array --
   --------------------------------

   function Do_Aggregate_Literal_Array (N : Node_Id) return Irep is
      Result_Type : constant Irep := Do_Type_Reference (Etype (N));
      Array_Expr : Irep;
      Result_Struct : constant Irep :=
        Make_Struct_Expr (I_Type => Result_Type,
                          Source_Location => Sloc (N));
      Pos_Iter : Node_Id := First (Expressions (N));
      Pos_Number : Natural := 0;
      With_Mode : Boolean;
      Element_Type_Ent : constant Entity_Id := Get_Array_Component_Type (N);
      Element_Type : constant Irep := Do_Type_Reference (Element_Type_Ent);
      --  TODO: multi-dimensional aggregates
      Bounds : constant Node_Id := Aggregate_Bounds (N);
      Low_Expr : constant Irep := Do_Expression (Low_Bound (Bounds));
      High_Expr : constant Irep := Do_Expression (High_Bound (Bounds));
      Index_Type_Node : constant Entity_Id := Etype (Etype (Bounds));
      Index_Type : constant Irep := Do_Type_Reference (Index_Type_Node);

      --  len = (high - low) + 1
      One_Expr : constant Irep := Make_Integer_Constant (1, Index_Type_Node);
      Diff_Expr : constant Irep :=
        Make_Op_Sub (Lhs => High_Expr,
                     Rhs => Low_Expr,
                     I_Type => Index_Type,
                     Source_Location => Sloc (N));
      Len_Expr : constant Irep :=
        Make_Op_Add (Lhs => Diff_Expr,
                     Rhs => One_Expr,
                     I_Type => Index_Type,
                     Source_Location => Sloc (N));

      Bare_Array_Type : constant Irep :=
        Make_Array_Type (I_Subtype => Element_Type,
                         Size => Len_Expr);

      Literal_Temp : constant Irep :=
        Fresh_Var_Symbol_Expr (Bare_Array_Type, "array_literal");

      Typecast_Expr : constant Irep :=
        Make_Op_Typecast (Op0 => Make_Address_Of (Literal_Temp),
                          I_Type => Make_Pointer_Type (Element_Type),
                          Source_Location => Sloc (N));
   begin
      --  Handle an "others" splat expression if present:
      if Present (Component_Associations (N)) then
         --  Produce something like array_of(others_expr)
         --                         with 1 => 100, 2 => 200, ...
         --  We expect only one named operand (others => ...):
         pragma Assert (List_Length (Component_Associations (N)) = 1);
         declare
            Others_Node : constant Node_Id :=
              First (Component_Associations (N));
            Others_Choices : constant List_Id := Choices (Others_Node);
            Expr : constant Irep := Do_Expression (Expression (Others_Node));
         begin
            pragma Assert (List_Length (Others_Choices) = 1);
            pragma Assert (Nkind (First (Others_Choices)) = N_Others_Choice);
            Array_Expr :=
              Make_Op_Array_Of (I_Type => Bare_Array_Type,
                                Op0 => Expr,
                                Source_Location => Sloc (N));
         end;
         With_Mode := True;
      else
         Array_Expr := Make_Array_Expr (I_Type => Bare_Array_Type,
                                        Source_Location => Sloc (N));
         With_Mode := False;
      end if;

      Set_Type (Array_Expr, Bare_Array_Type);

      while Present (Pos_Iter) loop
         declare
            Expr : constant Irep := Do_Expression (Pos_Iter);
         begin
            if With_Mode then
               declare
                  Pos_Str : constant String := Integer'Image (Pos_Number);
                  Pos_Constant : constant Irep :=
                    Make_Constant_Expr (Value => Pos_Str (2 .. Pos_Str'Last),
                                        I_Type => Make_Integer_Type,
                                        Source_Location => Sloc (N));
                  New_With : constant Irep :=
                    Make_With_Expr (Old => Array_Expr,
                                    Where => Pos_Constant,
                                    New_Value => Expr,
                                    I_Type => Bare_Array_Type,
                                    Source_Location => Sloc (N));
               begin
                  Array_Expr := New_With;
               end;
            else
               Append_Operand (Array_Expr, Expr);
            end if;
         end;
         Next (Pos_Iter);
         Pos_Number := Pos_Number + 1;
      end loop;

      --  We now have an array literal of some sort (Array_Expr).
      --  Now encase it in an array structure,
      --  and allocate memory to hold the literal:
      Append_Struct_Member (Result_Struct, Low_Expr);
      Append_Struct_Member (Result_Struct, High_Expr);
      declare
         Dup : constant Irep :=
           Get_Array_Dup_Function (Element_Type_Ent, Index_Type_Node);
         Call_Args : constant Irep := New_Irep (I_Argument_List);
         Call_Expr : constant Irep :=
           Make_Side_Effect_Expr_Function_Call (I_Function => Dup,
                                                Arguments => Call_Args,
                                                Source_Location => Sloc (N));
      begin
         Append_Argument (Call_Args, Typecast_Expr);
         Append_Argument (Call_Args, Len_Expr);
         Append_Struct_Member (Result_Struct, Call_Expr);
      end;

      return Make_Let_Expr (Symbol => Literal_Temp,
                            Value => Array_Expr,
                            Where => Result_Struct,
                            I_Type => Result_Type,
                            Source_Location => Sloc (N));
   end Do_Aggregate_Literal_Array;

   ---------------------------------
   -- Do_Aggregate_Literal_Record --
   ---------------------------------

   function Do_Aggregate_Literal_Record (N : Node_Id) return Irep is
      N_Type : constant Entity_Id := Etype (N);
      N_Type_Decl : constant Node_Id := Parent (N_Type);
      N_Underlying_Type : constant Node_Id := Etype (N_Type);
   begin
      --  It appears GNAT sorts the aggregate members for us into the order
      --  discriminant (if any), common members, variant members.
      --  However, let's check.
      declare
         Components   : constant Node_Id :=
           Component_List (Type_Definition (Parent (N_Underlying_Type)));
         Variant_Node : constant Node_Id := Variant_Part (Components);

         Component_Iter : Node_Id := First (Component_Items (Components));
         Actual_Iter    : Node_Id := First (Component_Associations (N));
         Disc_Constraint : Node_Id := Types.Empty;
         Struct_Expr : constant Irep := New_Irep (I_Struct_Expr);
      begin

         case Ekind (N_Type) is
            when E_Record_Subtype =>
               --  Perhaps carrying a variant constraint:
               Disc_Constraint :=
                 Constraint (Subtype_Indication (N_Type_Decl));
            when E_Record_Type =>
               --  Unconstrained, nothing to do
               null;
            when others =>
               --  Impossible, already filtered by caller
               raise Program_Error;
         end case;

         if Present (Variant_Node) then
            --  Expect a discriminant value
            pragma Assert (Entity (Name (Variant_Node)) =
                           Entity (First (Choices (Actual_Iter))));
            Append_Struct_Member (Struct_Expr,
                                  Do_Expression (Expression (Actual_Iter)));
            Next (Actual_Iter);
         end if;

         --  Next expect common members
         while Present (Component_Iter) loop
            pragma Assert (Present (Actual_Iter));
            pragma Assert (Defining_Identifier (Component_Iter) =
                           Entity (First (Choices (Actual_Iter))));
            Append_Struct_Member (Struct_Expr,
                                  Do_Expression (Expression (Actual_Iter)));
            Next (Component_Iter);
            Next (Actual_Iter);
         end loop;

         --  Extract variant members
         if Present (Variant_Node) then
            declare
               Variant_Union_Name : constant String :=
                 Get_Variant_Union_Member_Name (
                   First (Constraints (Disc_Constraint)));
               Union_Literal : constant Irep := New_Irep (I_Union_Expr);
               Variant_Substruct : constant Irep := New_Irep (I_Struct_Expr);
               Substruct_Component_List : Node_Id := Types.Empty;
               Variant_Found : Node_Id := Types.Empty;
               Variant_Iter : Node_Id := First (Variants (Variant_Node));

            begin
               Set_Type (Union_Literal,
                         Anonymous_Type_Map.Element (Variant_Node));
               Set_Component_Name (Union_Literal, Variant_Union_Name);

               --  Try to find a subrecord matching the aggregate's actual
               --  discriminant.
               while Present (Variant_Iter)
                 and then Substruct_Component_List = 0
               loop
                  declare
                     Choice_Iter : Node_Id :=
                       First (Discrete_Choices (Variant_Iter));
                  begin
                     while Present (Choice_Iter) and then
                       Substruct_Component_List = 0
                     loop
                        if Entity (Choice_Iter) =
                          Entity (First (Constraints (Disc_Constraint)))
                        then
                           Substruct_Component_List :=
                             First (
                               Component_Items (
                                 Component_List (Variant_Iter)));
                           Variant_Found := Variant_Iter;
                        end if;
                        Next (Choice_Iter);
                     end loop;
                     Next (Variant_Iter);
                  end;
               end loop;

               --  Check we found a matching subrecord
               pragma Assert (Present (Variant_Found));

               Set_Type (Variant_Substruct,
                         Anonymous_Type_Map.Element (Variant_Found));

               --  Try to parse remaining aggregate parts according to that
               --  subrecord.
               while Present (Substruct_Component_List) loop
                  pragma Assert (Present (Actual_Iter));
                  pragma Assert
                    (Defining_Identifier (Substruct_Component_List) =
                     Entity (First (Choices (Actual_Iter))));
                  Append_Struct_Member (
                    Variant_Substruct,
                    Do_Expression (Expression (Actual_Iter)));
                  Next (Substruct_Component_List);
                  Next (Actual_Iter);
               end loop;

               --  Add union literal to the outer struct:
               Set_Op0 (Union_Literal, Variant_Substruct);
               Append_Struct_Member (Struct_Expr, Union_Literal);
            end;

         end if;

         Set_Type (Struct_Expr, Do_Type_Reference (N_Underlying_Type));
         return Struct_Expr;
      end;

   end Do_Aggregate_Literal_Record;

   -------------------------
   -- Do_Array_Assignment --
   -------------------------

   --  TODO: multi-dimensional arrays
   function Do_Array_Assignment (LHS_Expr : Irep; RHS_Expr : Irep; N : Node_Id)
                                return Irep
   is
      --  Create temporaries for arguments to avoid repeated evaluation:
      --  TODO: figure out when this is redundant.
      LHS : constant Irep :=
        Fresh_Var_Symbol_Expr (Get_Type (LHS_Expr), "array_assign_lhs");
      RHS : constant Irep :=
        Fresh_Var_Symbol_Expr (Get_Type (RHS_Expr), "array_assign_rhs");
      Ret : constant Irep := New_Irep (I_Code_Block);
      LHS_Idx_Type : constant Node_Id := Get_Array_Index_Type (Name (N));
      RHS_Idx_Type : constant Node_Id := Get_Array_Index_Type (Expression (N));
      LHS_Length : constant Irep :=
        Make_Array_Length_Expr (LHS, LHS_Idx_Type);
      RHS_Length : Irep :=
        Make_Array_Length_Expr (RHS, RHS_Idx_Type);
      Copy_Args : constant Irep := New_Irep (I_Argument_List);
      LHS_Element_Type : constant Entity_Id :=
        Get_Array_Component_Type (Name (N));
      RHS_Element_Type : constant Entity_Id :=
        Get_Array_Component_Type (Expression (N));
      Copy_Func : constant Irep :=
        Get_Array_Copy_Function (LHS_Element_Type,
                                 RHS_Element_Type,
                                 LHS_Idx_Type);
      LHS_Data_Type : constant Irep :=
        Make_Pointer_Type (Do_Type_Reference (LHS_Element_Type));
      RHS_Data_Type : constant Irep :=
        Make_Pointer_Type (Do_Type_Reference (RHS_Element_Type));
   begin

      Append_Declare_And_Init (LHS, LHS_Expr, Ret, Sloc (N));
      Append_Declare_And_Init (RHS, RHS_Expr, Ret, Sloc (N));

      RHS_Length :=
        Maybe_Make_Typecast (RHS_Length, RHS_Idx_Type, LHS_Idx_Type);

      --  assert (RHS'Length == LHS'Length)
      declare
         Cond : constant Irep :=
           Make_Op_Eq (Lhs => LHS_Length,
                       Rhs => RHS_Length,
                       I_Type => Make_Bool_Type,
                       Source_Location => Sloc (N));
         Assert : constant Irep :=
           Make_Code_Assert (Assertion => Cond,
                             Source_Location => Sloc (N));
      begin
         Append_Op (Ret, Assert);
      end;

      --  array_copy (lhs, rhs, length)
      declare
         LHS_Data : constant Irep :=
           Make_Member_Expr (Compound => LHS,
                             Component_Name => "data",
                             I_Type => LHS_Data_Type,
                             Source_Location => Sloc (N));
         RHS_Data : constant Irep :=
           Make_Member_Expr (Compound => RHS,
                             Component_Name => "data",
                             I_Type => RHS_Data_Type,
                             Source_Location => Sloc (N));
      begin
         Append_Argument (Copy_Args, LHS_Data);
         Append_Argument (Copy_Args, RHS_Data);
         Append_Argument (Copy_Args, LHS_Length);
      end;

      Append_Op (Ret, Make_Code_Function_Call (I_Function => Copy_Func,
                                               Arguments => Copy_Args,
                                               Source_Location => Sloc (N)));

      return Ret;

   end Do_Array_Assignment;

   --------------------
   -- Do_Array_Range --
   --------------------

   --  This handled the oddball anonymous range nodes that can occur
   --  in array type declarations; they're effectively subtype indication
   --  nodes with an implied base type and a range constraint.
   function Do_Array_Range (N : Node_Id) return Irep
   is
      Underlying : constant Irep :=
        Do_Type_Reference (Etype (Etype (N)));
   begin
      return Do_Bare_Range_Constraint (N, Underlying);
   end Do_Array_Range;

   -----------------------------
   -- Do_Assignment_Statement --
   -----------------------------

   function Do_Assignment_Statement (N : Node_Id) return Irep
   is
      LHS : constant Irep := Do_Expression (Name (N));
      RHS : constant Irep := Do_Expression (Expression (N));
   begin
      if Ekind (Etype (Name (N))) in Array_Kind then
         return Do_Array_Assignment (LHS, RHS, N);
      end if;
      return R : constant Irep := New_Irep (I_Code_Assign) do
         Set_Source_Location (R, Sloc (N));
         Set_Lhs (R, LHS);
         if Do_Range_Check (Expression (N)) then
            --  Implicit typecast. Make it explicit.
            declare
               Cast_RHS : constant Irep := New_Irep (I_Op_Typecast);
            begin
               Set_Op0 (Cast_RHS, RHS);
               Set_Type (Cast_RHS, Get_Type (LHS));
               Set_Range_Check (Cast_RHS, True);
               Set_Rhs (R, Cast_RHS);
            end;
         else
            Set_Rhs (R, RHS);
         end if;
      end return;
   end Do_Assignment_Statement;

   ------------------------------
   -- Do_Bare_Range_Constraint --
   ------------------------------

   function Do_Bare_Range_Constraint (Range_Expr : Node_Id; Underlying : Irep)
                                     return Irep
   is
      Resolved_Underlying : constant Irep :=
        Follow_Symbol_Type (Underlying, Global_Symbol_Table);
      --  ??? why not get this from the entity
   begin
      return R : constant Irep := New_Irep (I_Bounded_Signedbv_Type) do
         Set_Width (R, Get_Width (Resolved_Underlying));
         Set_Lower_Bound (R, Do_Constant (Low_Bound (Range_Expr)));
         Set_Upper_Bound (R, Do_Constant (High_Bound (Range_Expr)));
      end return;
   end Do_Bare_Range_Constraint;

   ------------------------
   -- Do_Call_Parameters --
   ------------------------

   function Do_Call_Parameters (N : Node_Id) return Irep
   is
      Args : constant Irep := New_Irep (I_Argument_List);

      function Wrap_Argument (Base : Irep; Is_Out : Boolean) return Irep is
        (if Is_Out
         then Make_Address_Of (Base)
         else Base);

      procedure Handle_Parameter (Formal : Entity_Id; Actual : Node_Id);

      ----------------------
      -- Handle_Parameter --
      ----------------------

      procedure Handle_Parameter (Formal : Entity_Id; Actual : Node_Id) is
         Is_Out        : constant Boolean := Out_Present (Parent (Formal));
         Actual_Irep   : constant Irep :=
           Wrap_Argument (Do_Expression (Actual), Is_Out);

      begin
         Append_Argument (Args, Actual_Irep);
      end Handle_Parameter;

      procedure Handle_Parameters is new
        Iterate_Call_Parameters (Handle_Parameter);

   --  Start of processing for Do_Call_Parameters

   begin
      Handle_Parameters (N);
      return Args;
   end Do_Call_Parameters;

   ------------------------
   -- Do_Case_Expression --
   ------------------------

   function Do_Case_Expression (N : Node_Id) return Irep is

      --  Appease the style police
      function Make_Case_Test (Alts : List_Id) return Irep;

      Ret : constant Irep := New_Irep (I_Let_Expr);
      Value : constant Irep := Do_Expression (Expression (N));
      Bound_Var : constant Irep :=
        Fresh_Var_Symbol_Expr (Get_Type (Value), "case_binder");

      --------------------
      -- Make_Case_Test --
      --------------------

      function Make_Case_Test (Alts : List_Id) return Irep is
         function Make_Single_Test (Alt : Node_Id) return Irep;
         function Make_Single_Test (Alt : Node_Id) return Irep is
            Ret : constant Irep := New_Irep (I_Op_Eq);
            Rhs : constant Irep := Do_Expression (Alt);
         begin
            Set_Lhs (Ret, Bound_Var);
            Set_Rhs (Ret, Rhs);
            Set_Type (Ret, New_Irep (I_Bool_Type));
            return Ret;
         end Make_Single_Test;
         First_Alt_Test : constant Irep := Make_Single_Test (First (Alts));
         This_Alt : Node_Id := First (Alts);
      begin
         Next (This_Alt);
         if not Present (This_Alt) then
            return First_Alt_Test;
         end if;
         declare
            Big_Or : constant Irep := New_Irep (I_Op_Or);
         begin
            Append_Op (Big_Or, First_Alt_Test);
            while Present (This_Alt) loop
               Append_Op (Big_Or, Make_Single_Test (This_Alt));
               Next (This_Alt);
            end loop;
            return Big_Or;
         end;
      end Make_Case_Test;

      Case_Body_Leaf : Irep := Ireps.Empty;
      This_Alt : Node_Id := First (Alternatives (N));
   begin
      Set_Symbol (Ret, Bound_Var);
      Set_Value (Ret, Value);

      --  Do-while loop because there must be at least one alternative.
      loop
         declare
            This_Expr : constant Irep := Do_Expression (Expression (This_Alt));
            This_Alt_Copy : constant Node_Id := This_Alt;
            This_Test : Irep;
         begin
            Next (This_Alt);
            if not Present (This_Alt) then
               --  Omit test, this is either `others`
               --  or the last case of complete coverage
               This_Test := This_Expr;
            else
               This_Test := New_Irep (I_If_Expr);
               Set_Cond (This_Test,
                         Make_Case_Test (Discrete_Choices (This_Alt_Copy)));
               Set_True_Case (This_Test, This_Expr);
               Set_Type (This_Test, Get_Type (This_Expr));
            end if;
            if Case_Body_Leaf = Ireps.Empty then
               --  First case
               Set_Where (Ret, This_Test);
               Set_Type (Ret, Get_Type (This_Test));
            else
               --  Subsequent case, add to list of conditionals
               Set_False_Case (Case_Body_Leaf, This_Test);
            end if;
            Case_Body_Leaf := This_Test;
         end;
         exit when not Present (This_Alt);
      end loop;
      return Ret;
   end Do_Case_Expression;

   -------------------------
   -- Do_Compilation_Unit --
   -------------------------

   function Do_Compilation_Unit (N : Node_Id) return Symbol
   is
      U           : constant Node_Id := Unit (N);
      Unit_Symbol : Symbol;
   begin
      case Nkind (U) is
         when N_Subprogram_Body =>
            declare
               Unit_Type : constant Irep :=
                 Do_Subprogram_Specification (Specification (U));
               Unit_Name : constant Symbol_Id :=
                 Intern (Unique_Name (Unique_Defining_Entity (U)));
            begin
               --  Register the symbol *before* we compile the body, for
               --  recursive calls.
               Unit_Symbol.Name       := Unit_Name;
               Unit_Symbol.PrettyName := Unit_Name;
               Unit_Symbol.BaseName   := Unit_Name;
               Unit_Symbol.Mode       := Intern ("C");
               Unit_Symbol.SymType    := Unit_Type;
               Global_Symbol_Table.Insert (Unit_Name, Unit_Symbol);

               Unit_Symbol.Value      := Do_Subprogram_Or_Block (U);
               Global_Symbol_Table.Replace (Unit_Name, Unit_Symbol);
            end;

         when others =>
            Print_Tree_Node (U);
            raise Program_Error;
      end case;

      return Unit_Symbol;
   end Do_Compilation_Unit;

   -----------------
   -- Do_Constant --
   -----------------

   function Do_Constant (N : Node_Id) return Irep is
      Ret           : constant Irep := New_Irep (I_Constant_Expr);
      Constant_Type : constant Irep := Do_Type_Reference (Etype (N));
   begin
      Set_Source_Location (Ret, Sloc (N));
      Set_Type (Ret, Constant_Type);
      --  ??? FIXME
      Set_Value (Ret, Convert_Uint_To_Binary (Intval (N), 32));
      return Ret;
   end Do_Constant;

   -------------------------------------
   -- Do_Constrained_Array_Definition --
   -------------------------------------

   --  No difference between representations at the moment:
   function Do_Constrained_Array_Definition (N : Node_Id) return Irep
   is (Do_Unconstrained_Array_Definition (N));

   ----------------------------
   -- Do_Defining_Identifier --
   ----------------------------

   function Do_Defining_Identifier (E : Entity_Id) return Irep is
      Sym          : constant Irep := New_Irep (I_Symbol_Expr);
      Result_Type  : constant Irep := Do_Type_Reference (Etype (E));

      Is_Out_Param : constant Boolean :=
        Ekind (E) in E_In_Out_Parameter | E_Out_Parameter;

      Symbol_Type  : constant Irep :=
        (if Is_Out_Param
         then Make_Pointer_Type (Result_Type)
         else Result_Type);

   begin
      Set_Source_Location (Sym, Sloc (E));
      Set_Identifier      (Sym, Unique_Name (E));
      Set_Type            (Sym, Symbol_Type);

      if Is_Out_Param then
         return Deref : constant Irep := New_Irep (I_Dereference_Expr) do
            Set_Type   (Deref, Result_Type);
            Set_Object (Deref, Sym);
         end return;
      else
         return Sym;
      end if;
   end Do_Defining_Identifier;

   --------------------
   -- Do_Dereference --
   --------------------

   function Do_Dereference (N : Node_Id) return Irep is
      Ret : constant Irep := New_Irep (I_Dereference_Expr);
   begin
      Set_Type   (Ret, Do_Type_Reference (Etype (N)));
      Set_Object (Ret, Do_Expression (Prefix (N)));
      return Ret;
   end Do_Dereference;

   --------------------------------
   -- Do_Derived_Type_Definition --
   --------------------------------

   function Do_Derived_Type_Definition (N : Node_Id) return Irep is
      Subtype_Irep : constant Irep :=
        Do_Subtype_Indication (Subtype_Indication (N));
   begin
      if Abstract_Present (N)
        or else Null_Exclusion_Present (N)
        or else Present (Record_Extension_Part (N))
        or else Limited_Present (N)
        or else Task_Present (N)
        or else Protected_Present (N)
        or else Synchronized_Present (N)
        or else Present (Interface_List (N))
        or else Interface_Present (N)
      then
         pp (Union_Id (N));
         raise Program_Error;
      end if;

      return Subtype_Irep;
   end Do_Derived_Type_Definition;

   -------------------------------
   -- Do_Enumeration_Definition --
   -------------------------------

   function Do_Enumeration_Definition (N : Node_Id) return Irep is
      Ret : constant Irep := New_Irep (I_C_Enum_Type);
      Enum_Body : constant Irep := New_Irep (I_C_Enum_Members);
      Enum_Type_Symbol : constant Irep := New_Irep (I_Symbol_Type);
      Member : Node_Id := First (Literals (N));
   begin
      Set_Identifier (Enum_Type_Symbol,
                      Unique_Name (Defining_Identifier (Parent (N))));
      loop
         declare
            Element : constant Irep := New_Irep (I_C_Enum_Member);
            Val_String : constant String :=
              UI_Image (Enumeration_Rep (Member));
            Val_Name : constant String := Unique_Name (Member);
            Base_Name : constant String := Get_Name_String (Chars (Member));
            Member_Symbol : Symbol;
            Member_Symbol_Init : constant Irep := New_Irep (I_Constant_Expr);
            Typecast_Expr : constant Irep := New_Irep (I_Op_Typecast);
            Member_Size : constant Int := UI_To_Int (Esize (Etype (Member)));
         begin
            Set_Value (Element, Val_String);
            Set_Identifier (Element, Val_Name);
            Set_Base_Name (Element, Base_Name);
            Append_Member (Enum_Body, Element);
            Member_Symbol.Name := Intern (Val_Name);
            Member_Symbol.PrettyName := Intern (Base_Name);
            Member_Symbol.BaseName := Intern (Base_Name);
            Member_Symbol.Mode := Intern ("C");
            Member_Symbol.IsStaticLifetime := True;
            Member_Symbol.IsStateVar := True;
            Member_Symbol.SymType := Enum_Type_Symbol;
            Set_Type (Member_Symbol_Init,
                      Make_Int_Type (Integer (Member_Size)));
            Set_Value (Member_Symbol_Init,
                       Convert_Uint_To_Binary (Enumeration_Rep (Member),
                                               Member_Size));
            Set_Op0 (Typecast_Expr, Member_Symbol_Init);
            Set_Type (Typecast_Expr, Enum_Type_Symbol);
            Member_Symbol.Value := Typecast_Expr;
            Global_Symbol_Table.Insert (Member_Symbol.Name, Member_Symbol);
         end;
         Next (Member);
         exit when not Present (Member);
      end loop;
      Set_Subtype (Ret, Make_Int_Type (32));
      Set_Body (Ret, Enum_Body);
      return Ret;
   end Do_Enumeration_Definition;

   -------------------
   -- Do_Expression --
   -------------------

   function Do_Expression (N : Node_Id) return Irep is
   begin
      Declare_Itype (Etype (N));
      return
        (case Nkind (N) is
            when N_Identifier           => Do_Identifier (N),
            when N_Selected_Component   => Do_Selected_Component (N),
            when N_Op                   => Do_Operator_General (N),
            when N_Integer_Literal      => Do_Constant (N),
            when N_Type_Conversion      => Do_Type_Conversion (N),
            when N_Function_Call        => Do_Function_Call (N),
            when N_Attribute_Reference  =>
               (case Get_Attribute_Id (Attribute_Name (N)) is
                  when Attribute_Access => Do_Address_Of (N),
                  when others           => raise Program_Error),
            when N_Explicit_Dereference => Do_Dereference (N),
            when N_Case_Expression      => Do_Case_Expression (N),
            when N_Aggregate            => Do_Aggregate_Literal (N),
            when N_Indexed_Component    => Do_Indexed_Component (N),
            when N_Slice                => Do_Slice (N),
            when others                 => raise Program_Error);
   end Do_Expression;

   ------------------------------
   -- Do_Full_Type_Declaration --
   ------------------------------

   procedure Do_Full_Type_Declaration (N : Node_Id) is
      New_Type : constant Irep := Do_Type_Definition (Type_Definition (N));
      E        : constant Entity_Id := Defining_Identifier (N);
   begin
      Do_Type_Declaration (New_Type, E);

      --  Declare the implicit initial subtype too
      if Etype (E) /= E then
         Do_Type_Declaration (New_Type, Etype (E));
      end if;
   end Do_Full_Type_Declaration;

   ----------------------
   -- Do_Function_Call --
   ----------------------

   function Do_Function_Call (N : Node_Id) return Irep
   is
      Func_Name    : constant Symbol_Id :=
        Intern (Unique_Name (Entity (Name (N))));

      Func_Symbol  : Symbol renames Global_Symbol_Table (Func_Name);

      The_Function : constant Irep := New_Irep (I_Symbol_Expr);

   begin
      Set_Identifier (The_Function, Unintern (Func_Name));
      Set_Type       (The_Function, Func_Symbol.SymType);
      --  ??? why not get this from the entity

      return R : constant Irep := New_Irep (I_Side_Effect_Expr_Function_Call)
      do
         Set_Source_Location (R, Sloc (N));
         Set_Function        (R, The_Function);
         Set_Arguments       (R, Do_Call_Parameters (N));
         Set_Type (R, Get_Return_Type (Func_Symbol.SymType));
      end return;
   end Do_Function_Call;

   ---------------------------------------
   -- Do_Handled_Sequence_Of_Statements --
   ---------------------------------------

   function Do_Handled_Sequence_Of_Statements (N : Node_Id) return Irep is
      Stmts : constant List_Id := Statements (N);
   begin
      return Process_Statements (Stmts);
   end Do_Handled_Sequence_Of_Statements;

   -------------------
   -- Do_Identifier --
   -------------------

   function Do_Identifier (N : Node_Id) return Irep is
      E : constant Entity_Id := Entity (N);
   begin
      return Do_Defining_Identifier (E);
   end Do_Identifier;

   ---------------------
   -- Do_If_Statement --
   ---------------------

   function Do_If_Statement (N : Node_Id) return Irep is

      --  ??? chained if-then-else are more idiomatically iterated with WHILE
      --  (at least in GNAT) than with recursion.

      function Do_If_Block (N : Node_Id) return Irep;
      procedure Do_Elsifs (Else_Ifs  : Node_Id;
                           Else_List : List_Id;
                           Ret       : Irep);

      ---------------
      -- Do_Elsifs --
      ---------------

      procedure Do_Elsifs (Else_Ifs  : Node_Id;
                           Else_List : List_Id;
                           Ret       : Irep)
      is
      begin
         if Present (Else_Ifs) then
            declare
               Sub_If   : constant Irep := Do_If_Block (Else_Ifs);
               Next_Eif : Node_Id := Else_Ifs;
            begin
               Next (Next_Eif);
               Do_Elsifs (Next_Eif, Else_List, Sub_If);
               Set_Else_Case (Ret, Sub_If);
            end;
         else
            if Present (Else_List) then
               Set_Else_Case (Ret, Process_Statements (Else_List));
            end if;
         end if;
      end Do_Elsifs;

      -----------------
      -- Do_If_Block --
      -----------------

      function Do_If_Block (N : Node_Id) return Irep is
         Cond_Expr : constant Irep := Do_Expression (Condition (N));
         If_Block  : constant Irep := Process_Statements (Then_Statements (N));
         Ret       : constant Irep := New_Irep (I_Code_Ifthenelse);

      begin
         Set_Source_Location (Ret, Sloc (N));
         Set_Cond (Ret, Cond_Expr);
         Set_Then_Case (Ret, If_Block);
         return Ret;
      end Do_If_Block;

      --  Local variables

      Ret : constant Irep := Do_If_Block (N);

   --  Start of processing for Do_If_Statement

   begin
      Do_Elsifs (First (Elsif_Parts (N)), Else_Statements (N), Ret);
      return Ret;
   end Do_If_Statement;

   -----------------------------------------
   -- Do_Index_Or_Discriminant_Constraint --
   -----------------------------------------

   --  For now, don't encode the constraint in the Irep form; we'll generate
   --  appropriate checks in the front-end, rather than delegating to CBMC
   --  as for range checks.
   function Do_Index_Or_Discriminant_Constraint
     (N : Node_Id; Underlying : Irep) return Irep
   is (Underlying);

   --------------------------
   -- Do_Indexed_Component --
   --------------------------

   --  TODO: multi-dimensional arrays.
   function Do_Indexed_Component (N : Node_Id) return Irep is
      (Make_Array_Index_Op
         (Do_Expression (Prefix (N)),
          Etype (Prefix (N)),
          Do_Expression (First (Expressions (N)))));

   ----------------------------
   -- Do_Itype_Array_Subtype --
   ----------------------------

   function Do_Itype_Array_Subtype (N : Node_Id) return Irep is
   begin
      --  Since we don't note the bounds at the irep level, just
      --  call this an alias:
      return R : constant Irep := New_Irep (I_Symbol_Type) do
         Set_Identifier (R, Unique_Name (Etype (N)));
      end return;
   end Do_Itype_Array_Subtype;

   -------------------------
   -- Do_Itype_Definition --
   -------------------------

   function Do_Itype_Definition (N : Node_Id) return Irep is
   begin
      --  Most type-defining functions use the defining statement,
      --  e.g. an N_Constrained_Array_Definition. This on the other
      --  hand must reverse-engineer the type from its Entity.
      --  Possibly in the long term, since we need this anyhow, it
      --  might become the only way to get a type definition.
      return (case Ekind (N) is
         when E_Array_Subtype => Do_Itype_Array_Subtype (N),
         when others => raise Program_Error);
   end Do_Itype_Definition;

   ------------------------
   -- Do_Itype_Reference --
   ------------------------

   procedure Do_Itype_Reference (N : Node_Id) is
      Typedef : constant Node_Id := Etype (Itype (N));

      function Do_Anonymous_Type_Definition return Irep;

      ----------------------------------
      -- Do_Anonymous_Type_Definition --
      ----------------------------------

      function Do_Anonymous_Type_Definition return Irep is
      begin
         case Ekind (Typedef) is
            when E_Anonymous_Access_Type =>
               return
                 Make_Pointer_Type
                   (Base => Do_Type_Reference (Designated_Type (Typedef)));

            when others =>
               raise Program_Error;
         end case;

      end Do_Anonymous_Type_Definition;

      New_Type : constant Irep := Do_Anonymous_Type_Definition;
   begin
      Do_Type_Declaration (New_Type, Typedef);
   end Do_Itype_Reference;

   -----------------------
   -- Do_Loop_Statement --
   -----------------------

   function Do_Loop_Statement (N : Node_Id) return Irep is

      Iter_Scheme : constant Node_Id := Iteration_Scheme (N);
      Body_Block  : constant Irep := Process_Statements (Statements (N));

      function Do_For_Statement return Irep;
      function Do_While_Statement (Cond : Irep) return Irep;

      ----------------------
      -- Do_For_Statement --
      ----------------------

      function Do_For_Statement return Irep is
         Ret : constant Irep := New_Irep (I_Code_For);
      begin
         --  ??? What happens to Body_Block here?
         Set_Source_Location (Ret, Sloc (N));
         return Ret;
      end Do_For_Statement;

      ------------------------
      -- Do_While_Statement --
      ------------------------

      function Do_While_Statement (Cond : Irep) return Irep is
         Ret : constant Irep := New_Irep (I_Code_While);
      begin
         Set_Source_Location (Ret, Sloc (N));
         Set_Cond (Ret, Cond);
         Set_Body (Ret, Body_Block);
         return Ret;
      end Do_While_Statement;

   begin

      if not Present (Iter_Scheme) then
         declare
            Const_True : constant Irep := New_Irep (I_Constant_Expr);
         begin
            --  mimic C-style 8-bit bool; this might also work with 1-bit type
            Set_Value (Const_True, "00000001");
            Set_Type (Const_True, Make_Int_Type (8));
            return Do_While_Statement (Const_True);
         end;
      elsif Present (Condition (Iter_Scheme)) then
         declare
            Cond : constant Irep := Do_Expression (Condition (Iter_Scheme));
         begin
            return Do_While_Statement (Cond);
         end;
      else
         return Do_For_Statement;
      end if;

   end Do_Loop_Statement;

   ---------------------------
   -- Do_Object_Declaration --
   ---------------------------

   procedure Do_Object_Declaration (N : Node_Id; Block : Irep) is
      Id   : constant Irep := Do_Defining_Identifier (Defining_Identifier (N));
      Decl : constant Irep := New_Irep (I_Code_Decl);
   begin
      Set_Source_Location (Decl, (Sloc (N)));
      Set_Symbol (Decl, Id);
      Append_Op (Block, Decl);

      if Has_Init_Expression (N) then
         declare
            Init_Expr      : constant Irep := Do_Expression (Expression (N));
            Init_Statement : constant Irep := New_Irep (I_Code_Assign);
         begin
            Set_Lhs (Init_Statement, Id);
            Set_Rhs (Init_Statement, Init_Expr);
            Append_Op (Block, Init_Statement);
         end;
      end if;
   end Do_Object_Declaration;

   ------------------
   -- Do_Op_Concat --
   ------------------

   function Do_Op_Concat (N : Node_Id) return Irep
   is
      LHS_Node : constant Node_Id := Left_Opnd (N);
      RHS_Node : constant Node_Id := Right_Opnd (N);
      LHS : Irep := Do_Expression (LHS_Node);
      RHS : Irep := Do_Expression (RHS_Node);
      New_Component_Type : constant Entity_Id := Get_Array_Component_Type (N);
      New_Pointer_Type : constant Irep := New_Irep (I_Pointer_Type);
      New_Index_Type : constant Entity_Id := Get_Array_Index_Type (N);
      New_First : Irep;
      New_Last :  constant Irep := New_Irep (I_Op_Sub);
      New_Limit : constant Irep := New_Irep (I_Op_Add);
      New_Length : constant Irep := New_Irep (I_Op_Add);
      New_Data_Expr : constant Irep :=
        New_Irep (I_Side_Effect_Expr_Cpp_New_Array);
      New_Data : Irep := New_Data_Expr;
      New_Data_RHS : constant Irep := New_Irep (I_Op_Add);
      Result : constant Irep := New_Irep (I_Struct_Expr);
      Result_Comma_Outer : constant Irep := New_Irep (I_Op_Comma);
      Result_Comma_Inner : constant Irep := New_Irep (I_Op_Comma);
      Ret : Irep := Result_Comma_Outer;
      LHS_Length : Irep;
      RHS_Length : Irep;
      LHS_Copy : Irep;
      RHS_Copy : Irep;

      --  Style-mandatory prototypes:

      function Get_Length (Opnd : Node_Id;
                           Opnd_Irep : Irep;
                           Is_Component : Boolean) return Irep;

      procedure Make_Binder (Expr : in out Irep; Target : in out Irep);

      function Make_Copy (Target_Ptr : Irep;
                          Source_Node : Node_Id;
                          Source_Irep : Irep;
                          Source_Length : Irep;
                          Is_Singleton : Boolean) return Irep;

      --  Support function bodies:

      function Get_Length (Opnd : Node_Id;
                           Opnd_Irep : Irep;
                           Is_Component : Boolean) return Irep
      is begin
         if Is_Component then
            return Make_Integer_Constant (1, New_Index_Type);
         else
            declare
               Index_Type : constant Node_Id := Get_Array_Index_Type (Opnd);
               Length_Expr : constant Irep :=
                 Make_Array_Length_Expr (Opnd_Irep, Index_Type);
            begin
               return Maybe_Make_Typecast (Length_Expr,
                                           Index_Type,
                                           New_Index_Type);
            end;
         end if;
      end Get_Length;

      procedure Make_Binder (Expr : in out Irep; Target : in out Irep) is
         Fresh : constant Irep :=
           Fresh_Var_Symbol_Expr (Get_Type (Expr), "op_binder");
         Let : constant Irep := New_Irep (I_Let_Expr);
      begin
         Set_Type (Let, Get_Type (Ret));
         Set_Symbol (Let, Fresh);
         Set_Value (Let, Expr);
         Set_Where (Let, Ret);
         --  Replace expr by the bound variable, and the target
         --  by the new enclosing let-expr.
         Expr := Fresh;
         Target := Let;
      end Make_Binder;

      function Make_Copy (Target_Ptr : Irep;
                          Source_Node : Node_Id;
                          Source_Irep : Irep;
                          Source_Length : Irep;
                          Is_Singleton : Boolean) return Irep is
      begin
         if Is_Singleton then
            declare
               Deref : constant Irep := New_Irep (I_Dereference_Expr);
               Assign : constant Irep := New_Irep (I_Side_Effect_Expr_Assign);
            begin
               --  Simply assign *target = source
               Set_Object (Deref, Target_Ptr);
               Set_Type (Deref, Get_Subtype (Get_Type (Target_Ptr)));
               Set_Lhs (Assign, Deref);
               Set_Rhs (Assign, Maybe_Make_Typecast (Source_Irep,
                                                     Etype (Source_Node),
                                                     New_Component_Type));
               return Assign;
            end;
         else
            declare
               --  return memcpy (target, src, src_length);
               Source_Eltype : constant Entity_Id :=
                 Get_Array_Component_Type (Source_Node);
               Source_Elirep : constant Irep :=
                 Do_Type_Reference (Source_Eltype);
               Source_Ptr : constant Irep := New_Irep (I_Pointer_Type);
               Callee : constant Irep :=
                 Get_Array_Copy_Function (New_Component_Type,
                                          Source_Eltype,
                                          New_Index_Type);
               Source_Data : constant Irep := New_Irep (I_Member_Expr);
               Call : constant Irep :=
                 New_Irep (I_Side_Effect_Expr_Function_Call);
               Call_Args : constant Irep :=
                 New_Irep (I_Argument_List);
            begin
               Set_Subtype (Source_Ptr, Source_Elirep);
               Set_Type (Source_Data, Source_Ptr);
               Set_Compound (Source_Data, Source_Irep);
               Set_Component_Name (Source_Data, "data");

               Append_Argument (Call_Args, Target_Ptr);
               Append_Argument (Call_Args, Source_Data);
               Append_Argument (Call_Args, Source_Length);
               Set_Arguments (Call, Call_Args);
               Set_Function (Call, Callee);

               return  Call;
            end;
         end if;
      end Make_Copy;

      --  This is a wild guess based on what seems to be constrained
      --  when you'd expect it to be so.
      Ultimate_Ancestor : constant Entity_Id := Etype (Entity (N));

   --  Start of processing for Do_Op_Concat
   begin

      --  Must set this before using Make_Binder
      Set_Type (Ret, Do_Type_Reference (Etype (N)));
      Set_Type (Result, Get_Type (Ret));
      Set_Type (Result_Comma_Outer, Get_Type (Ret));
      Set_Type (Result_Comma_Inner, Get_Type (Ret));

      --  Introduce a binder for the new allocation so we don't re-evaluate
      --  the alloc when referencing it several times:
      Set_Type (New_Data_Expr, New_Pointer_Type);
      Make_Binder (New_Data, Ret);

      --  Introduce binders for the operands if they are arrays:
      if not Is_Component_Left_Opnd (N) then
         Make_Binder (LHS, Ret);
      end if;
      if not Is_Component_Right_Opnd (N) then
         Make_Binder (RHS, Ret);
      end if;

      --  Get lengths (either from the operands, or 1 if a singleton)
      LHS_Length := Get_Length (LHS_Node, LHS, Is_Component_Left_Opnd (N));
      RHS_Length := Get_Length (RHS_Node, RHS, Is_Component_Right_Opnd (N));

      --  New array lower bound is given by rules:
      --  If the result is a constrained array, that array's lower bound
      --  otherwise if the LHS is a singleton, the result index type's least
      --  value, otherwise the LHS operand's lower bound.
      if Is_Constrained (Ultimate_Ancestor)
        or else Is_Component_Left_Opnd (N)
      then
         New_First := Do_Expression (
                         Low_Bound (
                            Scalar_Range (
                               Etype (
                                  First_Index (Ultimate_Ancestor)))));
      else
         New_First := New_Irep (I_Member_Expr);
         Set_Compound (New_First, LHS);
         Set_Component_Name (New_First, "first1");
         declare
            LHS_Idx_Type : constant Entity_Id :=
              Get_Array_Index_Type (LHS_Node);
         begin
            Set_Type (New_First, Do_Type_Reference (LHS_Idx_Type));
            New_First :=
              Maybe_Make_Typecast (New_First, LHS_Idx_Type, New_Index_Type);
         end;
      end if;

      --  New upper bound is simply new lower bound + lengths (less one,
      --  because Ada bounds are inclusive)
      Set_Lhs (New_Length, LHS_Length);
      Set_Rhs (New_Length, RHS_Length);
      Set_Type (New_Length, Get_Type (New_First));

      Set_Lhs (New_Limit, New_First);
      Set_Rhs (New_Limit, New_Length);
      Set_Type (New_Limit, Get_Type (New_First));

      Set_Lhs (New_Last, New_Limit);
      Set_Rhs (New_Last, Make_Integer_Constant (1, New_Index_Type));
      Set_Type (New_Last, Get_Type (New_First));

      --  Build the data array:
      Set_Subtype (New_Pointer_Type, Do_Type_Reference (New_Component_Type));
      Set_Size (New_Data_Expr, New_Length);
      LHS_Copy :=
        Make_Copy (New_Data,
                   LHS_Node,
                   LHS,
                   LHS_Length,
                   Is_Component_Left_Opnd (N));
      Set_Lhs (Result_Comma_Outer, LHS_Copy);

      --  Target for the RHS write:
      Set_Lhs (New_Data_RHS, New_Data);
      Set_Rhs (New_Data_RHS, LHS_Length);
      Set_Type (New_Data_RHS, New_Pointer_Type);
      RHS_Copy :=
        Make_Copy (New_Data_RHS,
                   RHS_Node,
                   RHS,
                   RHS_Length,
                   Is_Component_Right_Opnd (N));
      Set_Rhs (Result_Comma_Outer, Result_Comma_Inner);
      Set_Lhs (Result_Comma_Inner, RHS_Copy);

      --  Finally populate the result struct:
      Append_Struct_Member (Result, New_First);
      Append_Struct_Member (Result, New_Last);
      Append_Struct_Member (Result, New_Data);
      Set_Rhs (Result_Comma_Inner, Result);

      return Ret;

   end Do_Op_Concat;

   -------------------------
   -- Do_Operator_General --
   -------------------------

   function Do_Operator_General (N : Node_Id) return Irep is
   begin
      if Nkind (N) = N_Op_Concat then
         return Do_Op_Concat (N);
      else
         return Do_Operator_Simple (N);
      end if;
   end Do_Operator_General;

   ------------------------
   -- Do_Operator_Simple --
   ------------------------

   function Do_Operator_Simple (N : Node_Id) return Irep is
      LHS : constant Irep := Do_Expression (Left_Opnd (N));
      RHS : constant Irep := Do_Expression (Right_Opnd (N));

      function Op_To_Kind (N : N_Op) return Irep_Kind;

      ----------------
      -- Op_To_Kind --
      ----------------

      function Op_To_Kind (N : N_Op) return Irep_Kind is
      begin
         return
           (case N is
               when N_Op_Divide   => I_Op_Div,
               when N_Op_Add      => I_Op_Add,
               when N_Op_Subtract => I_Op_Sub,
               when N_Op_Multiply => I_Op_Mul,
               when N_Op_Rem      => I_Op_Rem,
               when N_Op_Mod      => I_Op_Mod,
               when N_Op_And      => I_Op_And,
               when N_Op_Or       => I_Op_Or,
               when N_Op_Eq       => I_Op_Eq,
               when N_Op_Ne       => I_Op_Notequal,
               when N_Op_Ge       => I_Op_Geq,
               when N_Op_Gt       => I_Op_Gt,
               when N_Op_Le       => I_Op_Leq,
               when N_Op_Lt       => I_Op_Lt,
               when N_Op_Concat
                  | N_Op_Expon
                  | N_Op_Xor
                  | N_Op_Rotate_Left
                  | N_Op_Rotate_Right
                  | N_Op_Shift_Left
                  | N_Op_Shift_Right
                  | N_Op_Shift_Right_Arithmetic
                  | N_Op_Abs
                  | N_Op_Minus
                  | N_Op_Not
                  | N_Op_Plus
          =>
             raise Program_Error);
      end Op_To_Kind;

      Op_Kind : constant Irep_Kind := Op_To_Kind (N_Op (Nkind (N)));
      Ret     : constant Irep      := New_Irep (Op_Kind);

   --  Start of processing for Do_Operator_Simple

   begin
      Set_Source_Location (Ret, Sloc (N));
      Set_Lhs (Ret, LHS);
      Set_Rhs (Ret, RHS);
      Set_Type (Ret, Do_Type_Reference (Etype (N)));

      if Do_Overflow_Check (N) then
         Set_Overflow_Check (Ret, True);
      end if;

      if Nkind (N) in N_Op_Divide | N_Op_Mod | N_Op_Rem
        and then Do_Division_Check (N)
      then
         Set_Div_By_Zero_Check (Ret, True);
      end if;

      return Ret;
   end Do_Operator_Simple;

   ---------------------------------
   -- Do_Procedure_Call_Statement --
   ---------------------------------

   function Do_Procedure_Call_Statement (N : Node_Id) return Irep
   is
      Callee : constant String := Unique_Name (Entity (Name (N)));
      --  ??? use Get_Entity_Name from gnat2why to handle entries and entry
      --  families (and most likely extend it for accesses to subprograms).

      Proc : constant Irep := New_Irep (I_Symbol_Expr);
      R    : constant Irep := New_Irep (I_Code_Function_Call);
   begin
      Set_Identifier (Proc, Callee);
      Set_Type (Proc,
                Global_Symbol_Table (Intern (Callee)).SymType);
      --  ??? Why not look at type of entity?

      Set_Source_Location (R, Sloc (N));
      --  Set_LHS (R, Empty);  -- ??? what is the "nil" irep?
      Set_Function (R, Proc);
      Set_Arguments (R, Do_Call_Parameters (N));

      return R;
   end Do_Procedure_Call_Statement;

   -------------------------
   -- Do_Range_Constraint --
   -------------------------

   function Do_Range_Constraint (N : Node_Id; Underlying : Irep)
                                 return Irep
   is (Do_Bare_Range_Constraint (Range_Expression (N), Underlying));

   --------------------------
   -- Do_Record_Definition --
   --------------------------

   function Do_Record_Definition (N : Node_Id) return Irep is

      Components : constant Irep := New_Irep (I_Struct_Union_Components);

      procedure Add_Record_Component (Comp_Name : String;
                                      Comp_Type_Node : Node_Id;
                                      Comp_Node : Node_Id;
                                      Add_To_List : Irep := Components);
      procedure Add_Record_Component_Raw (Comp_Name : String;
                                          Comp_Type : Irep;
                                          Comp_Node : Node_Id;
                                          Add_To_List : Irep := Components);
      procedure Do_Record_Component (Comp : Node_Id);
      procedure Do_Variant_Struct (Var : Node_Id; Union_Components : Irep);

      --------------------------
      -- Add_Record_Component --
      --------------------------

      procedure Add_Record_Component (Comp_Name : String;
                                      Comp_Type_Node : Node_Id;
                                      Comp_Node : Node_Id;
                                      Add_To_List : Irep := Components) is
      begin
         Add_Record_Component_Raw (Comp_Name,
                                   Do_Type_Reference (Comp_Type_Node),
                                   Comp_Node,
                                   Add_To_List);
      end Add_Record_Component;

      ------------------------------
      -- Add_Record_Component_Raw --
      ------------------------------

      procedure Add_Record_Component_Raw (Comp_Name : String;
                                          Comp_Type : Irep;
                                          Comp_Node : Node_Id;
                                          Add_To_List : Irep := Components) is
         Comp_Irep : constant Irep :=
           Make_Struct_Component (Comp_Name, Comp_Type);
      begin
         Set_Source_Location (Comp_Irep, Sloc (Comp_Node));
         Append_Component (Add_To_List, Comp_Irep);
      end Add_Record_Component_Raw;

      -------------------------
      -- Do_Record_Component --
      -------------------------

      procedure Do_Record_Component (Comp : Node_Id) is
         Comp_Name : constant String :=
           Unique_Name (Defining_Identifier (Comp));
         Comp_Defn : constant Node_Id := Component_Definition (Comp);
      begin
         if Present (Subtype_Indication (Comp_Defn)) then
            Add_Record_Component (Comp_Name,
                                  Entity (Subtype_Indication (Comp_Defn)),
                                  Comp);
         else
            pp (Union_Id (Comp_Defn));
            raise Program_Error;
         end if;
      end Do_Record_Component;

      -----------------------
      -- Do_Variant_Struct --
      -----------------------

      procedure Do_Variant_Struct (Var : Node_Id; Union_Components : Irep) is
         Struct_Type : constant Irep := Do_Record_Definition (Var);
         Type_Symbol : constant Irep := Get_Fresh_Type_Name (Struct_Type, Var);
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
      Component_Iter : Node_Id := First (Component_Items (Component_List (N)));
      Variants_Node  : constant Node_Id := Variant_Part (Component_List (N));
      Ret            : constant Irep := New_Irep (I_Struct_Type);

   --  Start of processing for Do_Record_Definition

   begin

      --  Create a field for the discriminant.
      --  This order (discriminant, common fields, variant fields)
      --  seems to match GNAT's record-literal ordering (apparently
      --  regardless of source ordering).
      if Present (Variants_Node) then
         declare
            Disc_Name : constant Node_Id := Name (Variants_Node);
         begin
            Add_Record_Component (Unique_Name (Entity (Disc_Name)),
                                  Etype (Disc_Name),
                                  Variants_Node);
         end;
      end if;

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
            Union_Irep : constant Irep := New_Irep (I_Union_Type);
            Union_Components : constant Irep :=
              New_Irep (I_Struct_Union_Components);
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
               Add_Record_Component_Raw (
                 "_variants", Union_Symbol, Variants_Node);
            end;
         end;
      end if;

      Set_Components (Ret, Components);

      return Ret;
   end Do_Record_Definition;

   ---------------------------
   -- Do_Selected_Component --
   ---------------------------

   function Do_Selected_Component (N : Node_Id) return Irep is
      Root           : constant Irep := Do_Expression (Prefix (N));
      Component      : constant Entity_Id := Entity (Selector_Name (N));
      Component_Type : constant Irep := Do_Type_Reference (Etype (Component));
      Ret            : constant Irep := New_Irep (I_Member_Expr);
   begin
      Set_Source_Location (Ret, Sloc (N));
      Set_Component_Name (Ret, Unique_Name (Component));
      Set_Type (Ret, Component_Type);
      if Do_Discriminant_Check (N) then

         declare
            Component_Variant : Node_Id := Types.Empty;
            Record_Type : constant Node_Id :=
              Type_Definition (Parent (Etype (Prefix (N))));
            Variant_Iter : Node_Id :=
              First (Variants (Variant_Part (Component_List (Record_Type))));
            Variant_Spec : constant Node_Id :=
              Variant_Part (Component_List (Record_Type));
            Union_Selector : constant Irep := New_Irep (I_Member_Expr);
            Substruct_Selector : constant Irep := New_Irep (I_Member_Expr);
            Disc_Selector : constant Irep := New_Irep (I_Member_Expr);
            Disc_Check : constant Irep := New_Irep (I_Op_Eq);
            Comma_Expr : constant Irep := New_Irep (I_Op_Comma);
         begin

            --  Find the variant this belongs to:
            while Present (Variant_Iter) and then Component_Variant = 0 loop
               declare
                  Item_Iter : Node_Id :=
                    First (Component_Items (Component_List (Variant_Iter)));
               begin
                  while Present (Item_Iter) and then Component_Variant = 0 loop
                     if Defining_Identifier (Item_Iter) = Component then
                        Component_Variant := Variant_Iter;
                     end if;
                     Next (Item_Iter);
                  end loop;
               end;
               Next (Variant_Iter);
            end loop;

            pragma Assert (Present (Component_Variant));

            --  Add a discriminant-check side-effect:
            Set_Compound (Disc_Selector, Root);
            Set_Component_Name (
              Disc_Selector, Unique_Name (Entity (Name (Variant_Spec))));
            Set_Type (
              Disc_Selector, Do_Type_Reference (Etype (Name (Variant_Spec))));
            Set_Lhs (Disc_Check, Disc_Selector);
            Set_Rhs (
              Disc_Check,
              Do_Expression (First (Discrete_Choices (Component_Variant))));
            Set_Type (Disc_Check, New_Irep (I_Bool_Type));
            Set_Lhs (Comma_Expr, Make_Runtime_Check (Disc_Check));

            --  Create the actual member access by interposing a union access:
            --  The actual access for member X of the Y == Z variant will look
            --  like (_check(Base.Disc == Z), Base._variants.Z.X)

            declare
               Variant_Constraint_Node : constant Node_Id :=
                 First (Discrete_Choices (Component_Variant));
               Variant_Name : constant String :=
                 Get_Variant_Union_Member_Name (Variant_Constraint_Node);
            begin
               Set_Component_Name (Substruct_Selector, Variant_Name);
            end;

            Set_Type (Substruct_Selector,
                      Anonymous_Type_Map.Element (Component_Variant));
            Set_Component_Name (Union_Selector, "_variants");
            Set_Type (Union_Selector,
                      Anonymous_Type_Map.Element (Variant_Spec));
            Set_Compound (Union_Selector, Root);
            Set_Compound (Substruct_Selector, Union_Selector);
            Set_Compound (Ret, Substruct_Selector);

            Set_Rhs (Comma_Expr, Ret);
            Set_Type (Comma_Expr, Get_Type (Ret));
            return Comma_Expr;

         end;
      else
         Set_Compound (Ret, Root);
         return Ret;
      end if;
   end Do_Selected_Component;

   ----------------------------------
   -- Do_Signed_Integer_Definition --
   ----------------------------------

   function Do_Signed_Integer_Definition (N : Node_Id) return Irep is
      Lower : constant Irep := Do_Constant (Low_Bound (N));
      Upper : constant Irep := Do_Constant (High_Bound (N));
      Ret   : constant Irep := New_Irep (I_Bounded_Signedbv_Type);

      E : constant Entity_Id := Defining_Entity (Parent (N));
      --  Type entity

      pragma Assert (Is_Type (E));

   begin
      Set_Lower_Bound (Ret, Lower);
      Set_Upper_Bound (Ret, Upper);
      Set_Width       (Ret, Positive (UI_To_Int (Esize (E))));
      return  Ret;
   end Do_Signed_Integer_Definition;

   --------------------------------
   -- Do_Simple_Return_Statement --
   --------------------------------

   function Do_Simple_Return_Statement (N : Node_Id) return Irep is
      Expr : constant Node_Id := Expression (N);
      R    : constant Irep := New_Irep (I_Code_Return);
   begin
      Set_Source_Location (R, Sloc (N));
      if Present (Expr) then
         Set_Return_Value (R, Do_Expression (Expr));
      end if;
      return R;
   end Do_Simple_Return_Statement;

   --------------
   -- Do_Slice --
   --------------

   function Do_Slice (N : Node_Id) return Irep is
      Result_Type : constant Irep := Do_Type_Reference (Etype (N));
      Base : constant Irep := Do_Expression (Prefix (N));
      Let_Sym : constant Irep :=
        Fresh_Var_Symbol_Expr (Result_Type, "slice_prefix");
      Idx_Type : constant Entity_Id :=
        Etype (First_Index (Etype (N)));
      Old_First_Expr : constant Irep :=
        Make_Array_First_Expr (Etype (Prefix (N)), Let_Sym);
      New_First_Expr : constant Irep :=
        Do_Expression (Low_Bound (Scalar_Range (Idx_Type)));
      New_Last_Expr : constant Irep :=
        Do_Expression (High_Bound (Scalar_Range (Idx_Type)));
      Offset : constant Irep := New_Irep (I_Op_Sub);
      Ret : constant Irep := New_Irep (I_Let_Expr);
      Element_Type : constant Entity_Id := Get_Array_Component_Type (N);
      Pointer_Type : constant Irep := New_Irep (I_Pointer_Type);
      Old_Data : constant Irep := New_Irep (I_Member_Expr);
      New_Data : constant Irep := New_Irep (I_Op_Add);
      Result : constant Irep := New_Irep (I_Struct_Expr);
   begin
      Set_Subtype (Pointer_Type, Do_Type_Reference (Element_Type));

      --  Adjust data pointer:
      Set_Lhs (Offset, New_First_Expr);
      Set_Rhs (Offset, Old_First_Expr);
      Set_Compound (Old_Data, Let_Sym);
      Set_Component_Name (Old_Data, "data");
      Set_Type (Old_Data, Pointer_Type);
      Set_Lhs (New_Data, Old_Data);
      Set_Rhs (New_Data, Offset);
      Set_Type (New_Data, Pointer_Type);

      --  Build result structure:
      Append_Struct_Member (Result, New_First_Expr);
      Append_Struct_Member (Result, New_Last_Expr);
      Append_Struct_Member (Result, New_Data);
      Set_Type (Result, Result_Type);

      --  Return:
      Set_Symbol (Ret, Let_Sym);
      Set_Value (Ret, Base);
      Set_Where (Ret, Result);
      Set_Type (Ret, Result_Type);

      return Ret;
   end Do_Slice;

   ------------------------
   -- Do_Subprogram_Body --
   ------------------------

   procedure Do_Subprogram_Body (N : Node_Id) is
      Proc_Name   : constant Symbol_Id :=
        Intern (Unique_Name (Corresponding_Spec (N)));
      Proc_Body   : constant Irep := Do_Subprogram_Or_Block (N);
      Proc_Symbol : Symbol := Global_Symbol_Table (Proc_Name);
   begin
      Proc_Symbol.Value := Proc_Body;
      Global_Symbol_Table.Replace (Proc_Name, Proc_Symbol);
   end Do_Subprogram_Body;

   -------------------------------
   -- Do_Subprogram_Declaration --
   -------------------------------

   procedure Do_Subprogram_Declaration (N : Node_Id) is
      Proc_Type : constant Irep :=
        Do_Subprogram_Specification (Specification (N));

      Proc_Name : constant Symbol_Id :=
        Intern (Unique_Name (Corresponding_Body (N)));

      Proc_Symbol : Symbol;

   begin
      Proc_Symbol.Name       := Proc_Name;
      Proc_Symbol.BaseName   := Proc_Name;
      Proc_Symbol.PrettyName := Proc_Name;
      Proc_Symbol.SymType    := Proc_Type;
      Proc_Symbol.Mode       := Intern ("C");

      Global_Symbol_Table.Insert (Proc_Name, Proc_Symbol);
   end Do_Subprogram_Declaration;

   ----------------------------
   -- Do_Subprogram_Or_Block --
   ----------------------------

   function Do_Subprogram_Or_Block (N : Node_Id) return Irep is
      Decls : constant List_Id := Declarations (N);
      HSS   : constant Node_Id := Handled_Statement_Sequence (N);
      Decls_Rep : Irep;
   begin
      Decls_Rep := (if Present (Decls)
                    then Process_Statements (Decls)
                    else New_Irep (I_Code_Block));

      Set_Source_Location (Decls_Rep, Sloc (N));
      if Present (HSS) then
         Process_Statement (HSS, Decls_Rep);
      end if;

      return Decls_Rep;
   end Do_Subprogram_Or_Block;

   --------------------------------
   -- Do_Subprogram_Specification --
   --------------------------------

   function Do_Subprogram_Specification (N : Node_Id) return Irep is
      Ret : constant Irep := New_Irep (I_Code_Type);
      Param_List : constant Irep := New_Irep (I_Parameter_List);
      Param_Iter : Node_Id := First (Parameter_Specifications (N));
   begin
      while Present (Param_Iter) loop
         declare
            Is_Out : constant Boolean := Out_Present (Param_Iter);

            Param_Type_Base : constant Irep :=
              Do_Type_Reference (Etype (Parameter_Type (Param_Iter)));

            Param_Type      : constant Irep :=
              (if Is_Out
               then Make_Pointer_Type (Param_Type_Base)
               else Param_Type_Base);

            Param_Name : constant String :=
                Unique_Name (Defining_Identifier (Param_Iter));

            Param_Irep : constant Irep := New_Irep (I_Code_Parameter);
            Param_Symbol : Symbol;

         begin
            Set_Source_Location (Param_Irep, Sloc (Param_Iter));
            Set_Type            (Param_Irep, Param_Type);
            Set_Identifier      (Param_Irep, Param_Name);
            Set_Base_Name       (Param_Irep, Param_Name);
            Append_Parameter (Param_List, Param_Irep);
            --  Add the param to the symtab as well:
            Param_Symbol.Name          := Intern (Param_Name);
            Param_Symbol.PrettyName    := Param_Symbol.Name;
            Param_Symbol.BaseName      := Param_Symbol.Name;
            Param_Symbol.SymType       := Param_Type;
            Param_Symbol.IsThreadLocal := True;
            Param_Symbol.IsFileLocal   := True;
            Param_Symbol.IsLValue      := True;
            Param_Symbol.IsParameter   := True;
            Global_Symbol_Table.Insert (Param_Symbol.Name, Param_Symbol);
            Next (Param_Iter);
         end;
      end loop;
      Set_Return_Type
        (Ret,
         (if Nkind (N) = N_Function_Specification
          then Do_Type_Reference (Etype (Result_Definition (N)))
          else New_Irep (I_Void_Type)));
      Set_Parameters (Ret, Param_List);
      return Ret;
   end Do_Subprogram_Specification;

   ----------------------------
   -- Do_Subtype_Declaration --
   ----------------------------

   procedure Do_Subtype_Declaration (N : Node_Id) is
      New_Type : constant Irep :=
        Do_Subtype_Indication (Subtype_Indication (N));

   begin
      Do_Type_Declaration (New_Type, Defining_Identifier (N));
   end Do_Subtype_Declaration;

   ---------------------------
   -- Do_Subtype_Indication --
   ---------------------------

   function Do_Subtype_Indication (N : Node_Id) return Irep
   is
      Underlying : constant Irep :=
        Do_Type_Reference (Etype (Subtype_Mark (N)));

      Constr : constant Node_Id := Constraint (N);

   begin
      if Present (Constr) then
         case Nkind (Constr) is
            when N_Range_Constraint =>
               return Do_Range_Constraint (Constr, Underlying);
            when N_Index_Or_Discriminant_Constraint =>
               return Do_Index_Or_Discriminant_Constraint (Constr, Underlying);
            when others =>
               Print_Tree_Node (N);
               raise Program_Error;
         end case;
      else
         return Underlying;
      end if;
   end Do_Subtype_Indication;

   ------------------------
   -- Do_Type_Conversion --
   ------------------------

   function Do_Type_Conversion (N : Node_Id) return Irep is
      To_Convert : constant Irep := Do_Expression (Expression (N));
      New_Type   : constant Irep := Do_Type_Reference (Etype (N));
      Ret        : constant Irep := New_Irep (I_Op_Typecast);
   begin
      Set_Source_Location (Ret, Sloc (N));
      if Do_Range_Check (Expression (N)) then
         Set_Range_Check (Ret, True);
      end if;
      Set_Op0  (Ret, To_Convert);
      Set_Type (Ret, New_Type);
      return Ret;
   end Do_Type_Conversion;

   -------------------------
   -- Do_Type_Declaration --
   -------------------------

   procedure Do_Type_Declaration (New_Type_In : Irep; E : Entity_Id) is
      New_Type        : constant Irep := New_Type_In;
      New_Type_Name   : constant Symbol_Id := Intern (Unique_Name (E));
      New_Type_Symbol : constant Symbol :=
        Make_Type_Symbol (New_Type_Name, New_Type);
   begin
      if Kind (New_Type) = I_Struct_Type then
         Set_Tag (New_Type, Unintern (New_Type_Name));
      end if;
      Symbol_Maps.Insert (Global_Symbol_Table, New_Type_Name, New_Type_Symbol);
   end Do_Type_Declaration;

   ------------------------
   -- Do_Type_Definition --
   ------------------------

   function Do_Type_Definition (N : Node_Id) return Irep is
   begin
      case Nkind (N) is
         when N_Record_Definition =>
            return Do_Record_Definition (N);
         when N_Signed_Integer_Type_Definition =>
            return Do_Signed_Integer_Definition (N);
         when N_Derived_Type_Definition =>
            return Do_Derived_Type_Definition (N);
         when N_Enumeration_Type_Definition =>
            return Do_Enumeration_Definition (N);
         when N_Constrained_Array_Definition =>
            return Do_Constrained_Array_Definition (N);
         when N_Unconstrained_Array_Definition =>
            return Do_Unconstrained_Array_Definition (N);
         when others =>
            pp (Union_Id (N));
            raise Program_Error;
      end case;
   end Do_Type_Definition;

   -----------------------
   -- Do_Type_Reference --
   -----------------------

   function Do_Type_Reference (E : Entity_Id) return Irep is
   begin
      if E = Standard_Integer then
         return Make_Int_Type (32);

      elsif E = Standard_Boolean then
         return New_Irep (I_Bool_Type);

      else
         declare
            Ret : constant Irep := New_Irep (I_Symbol_Type);
         begin
            Set_Identifier (Ret, Unique_Name (E));

            return Ret;
         end;

      end if;
   end Do_Type_Reference;

   ---------------------------------------
   -- Do_Unconstrained_Array_Definition --
   ---------------------------------------

   function Do_Unconstrained_Array_Definition (N : Node_Id) return Irep is
      Ret : constant Irep := New_Irep (I_Struct_Type);
      Ret_Components : constant Irep := New_Irep (I_Struct_Union_Components);
      Data_Type : constant Irep := New_Irep (I_Pointer_Type);
      Data_Member : constant Irep :=
        Make_Struct_Component ("data", Data_Type);
      Sub_Identifier : constant Node_Id :=
        Subtype_Indication (Component_Definition (N));
      Sub : constant Irep :=
        Do_Type_Reference (Etype (Sub_Identifier));
      Dimension_Iter : Node_Id :=
        First ((if Nkind (N) = N_Unconstrained_Array_Definition then
                   Subtype_Marks (N) else
                   Discrete_Subtype_Definitions (N)));
      Dimension_Number : Positive := 1;
   begin

      --  Define a structure with explicit first, last and data-pointer members

      while Present (Dimension_Iter) loop
         declare
            Number_Str_Raw : constant String :=
              Integer'Image (Dimension_Number);
            Number_Str : constant String :=
              Number_Str_Raw (2 .. Number_Str_Raw'Last);
            First_Name : constant String := "first" & Number_Str;
            Last_Name : constant String := "last" & Number_Str;
            Dimension_Type : constant Irep :=
              Do_Type_Reference (Etype (Dimension_Iter));
            First_Comp : constant Irep :=
              Make_Struct_Component (First_Name, Dimension_Type);
            Last_Comp : constant Irep :=
              Make_Struct_Component (Last_Name, Dimension_Type);
         begin

            --  Declare the dimension index type if required:
            case Nkind (Dimension_Iter) is
               when N_Subtype_Indication =>
                  Do_Type_Declaration (Do_Subtype_Indication (Dimension_Iter),
                                       Etype (Dimension_Iter));
               when N_Range =>
                  Do_Type_Declaration (Do_Array_Range (Dimension_Iter),
                                       Etype (Dimension_Iter));
               when others =>
                  null;
            end case;

            Append_Component (Ret_Components, First_Comp);
            Append_Component (Ret_Components, Last_Comp);

         end;
         Dimension_Number := Dimension_Number + 1;
         Next (Dimension_Iter);
      end loop;

      Set_Subtype (Data_Type, Sub);
      Append_Component (Ret_Components, Data_Member);

      Set_Components (Ret, Ret_Components);
      return Ret;

   end Do_Unconstrained_Array_Definition;

   ------------------------------
   -- Get_Array_Component_Type --
   ------------------------------

   function Get_Array_Component_Type (N : Node_Id) return Entity_Id is
      Ty : Entity_Id := Etype (N);
   begin
      while Ekind (Ty) = E_Array_Subtype loop
         Ty := Etype (Ty);
      end loop;
      return Component_Type (Ty);
   end Get_Array_Component_Type;

   -----------------------------
   -- Get_Array_Copy_Function --
   -----------------------------

   function Get_Array_Copy_Function (LHS_Element_Type : Entity_Id;
                                     RHS_Element_Type : Entity_Id;
                                     Index_Type : Entity_Id) return Irep is
      Map_Key : constant Array_Copy_Key :=
        (LHS_Element_Type, RHS_Element_Type, Index_Type);
      Map_Cursor : Array_Copy_Maps.Cursor;
      Map_Inserted : Boolean;
   begin
      Array_Copy_Map.Insert (Map_Key, Ireps.Empty, Map_Cursor, Map_Inserted);
      if not Map_Inserted then
         return Array_Copy_Maps.Element (Map_Cursor);
      end if;

      --  Create a new copy function:
      declare
         Func_Type : constant Irep := New_Irep (I_Code_Type);
         Func_Args : constant Irep := New_Irep (I_Parameter_List);
         Write_Ptr_Arg : constant Irep := New_Irep (I_Code_Parameter);
         Read_Ptr_Arg : constant Irep := New_Irep (I_Code_Parameter);
         LHS_Ptr_Type : constant Irep :=
           Make_Pointer_Type (Do_Type_Reference (LHS_Element_Type));
         RHS_Ptr_Type : constant Irep :=
           Make_Pointer_Type (Do_Type_Reference (RHS_Element_Type));
         Len_Arg : constant Irep := New_Irep (I_Code_Parameter);
         Len_Type : constant Irep := Do_Type_Reference (Index_Type);
         Func_Symbol : Symbol;
         Map_Size_Str : constant String :=
           Integer'Image (Integer (Array_Copy_Map.Length));
         Func_Name : constant String :=
           "__ada_copy_array" & Map_Size_Str (2 .. Map_Size_Str'Last);
         Body_Block : constant Irep := New_Irep (I_Code_Block);
         Body_Loop : constant Irep := New_Irep (I_Code_For);
         Loop_Test : constant Irep := New_Irep (I_Op_Lt);
         Loop_Assign : constant Irep := New_Irep (I_Code_Assign);
         RHS_Element : Irep;
         RHS_Cast : Irep;
         Counter_Sym : constant Irep :=
           Fresh_Var_Symbol_Expr (Len_Type, "idx");
      begin
         --  Create type (lhs_el_type*, rhs_el_type*, index_type) -> void
         Set_Type (Write_Ptr_Arg, LHS_Ptr_Type);
         Set_Identifier (Write_Ptr_Arg, Func_Name & "::out");
         Set_Base_Name (Write_Ptr_Arg, "out");
         Set_Type (Read_Ptr_Arg, RHS_Ptr_Type);
         Set_Identifier (Read_Ptr_Arg, Func_Name & "::in");
         Set_Base_Name (Read_Ptr_Arg, "in");
         Set_Type (Len_Arg, Len_Type);
         Set_Identifier (Len_Arg, Func_Name & "::len");
         Set_Base_Name (Len_Arg, "len");
         Append_Parameter (Func_Args, Write_Ptr_Arg);
         Append_Parameter (Func_Args, Read_Ptr_Arg);
         Append_Parameter (Func_Args, Len_Arg);
         Set_Parameters (Func_Type, Func_Args);
         Set_Return_Type (Func_Type, New_Irep (I_Void_Type));

         --  Create function body (declarations and a copy for-loop):
         Append_Declare_And_Init
           (Counter_Sym, Make_Integer_Constant (0, Index_Type), Body_Block, 0);

         Set_Iter (Body_Loop, Make_Increment (Counter_Sym, Index_Type, 1));
         Set_Lhs (Loop_Test, Counter_Sym);
         Set_Rhs (Loop_Test, Param_Symbol (Len_Arg));
         Set_Cond (Body_Loop, Loop_Test);

         Set_Lhs (Loop_Assign,
                  Make_Pointer_Index (Param_Symbol (Write_Ptr_Arg),
                                      Counter_Sym));
         RHS_Element := Make_Pointer_Index (Param_Symbol (Read_Ptr_Arg),
                                            Counter_Sym);
         if LHS_Element_Type = RHS_Element_Type then
            RHS_Cast := RHS_Element;
         else
            RHS_Cast := New_Irep (I_Op_Typecast);
            Set_Type (RHS_Cast, Get_Type (Get_Lhs (Loop_Assign)));
            Set_Op0 (RHS_Cast, RHS_Element);
         end if;

         Set_Rhs (Loop_Assign, RHS_Element);
         Set_Loop_Body (Body_Loop, Loop_Assign);

         Append_Op (Body_Block, Body_Loop);

         --  Make function symbol:
         Func_Symbol.SymType := Func_Type;
         Func_Symbol.Name := Intern (Func_Name);
         Func_Symbol.PrettyName := Func_Symbol.Name;
         Func_Symbol.BaseName := Func_Symbol.Name;
         Func_Symbol.Mode := Intern ("C");
         Func_Symbol.Value := Body_Block;
         Global_Symbol_Table.Insert (Intern (Func_Name), Func_Symbol);

         --  Record it for the future:
         Array_Copy_Map.Replace_Element (Map_Cursor,
                                         Symbol_Expr (Func_Symbol));

         return Array_Copy_Maps.Element (Map_Cursor);
      end;

   end Get_Array_Copy_Function;

   ----------------------------
   -- Get_Array_Dup_Function --
   ----------------------------

   function Get_Array_Dup_Function (Element_Type : Entity_Id;
                                    Index_Type : Entity_Id) return Irep is
      Map_Key : constant Array_Dup_Key := (Element_Type, Index_Type);
      Map_Cursor : Array_Dup_Maps.Cursor;
      Map_Inserted : Boolean;
   begin
      Array_Dup_Map.Insert (Map_Key, Ireps.Empty, Map_Cursor, Map_Inserted);
      if not Map_Inserted then
         return Array_Dup_Maps.Element (Map_Cursor);
      end if;

      --  Create a new duplicator function:
      declare

         Func_Type : constant Irep := New_Irep (I_Code_Type);
         Func_Args : constant Irep := New_Irep (I_Parameter_List);
         Ptr_Arg : constant Irep := New_Irep (I_Code_Parameter);
         Ptr_Type : constant Irep :=
           Make_Pointer_Type (Do_Type_Reference (Element_Type));
         Len_Arg : constant Irep := New_Irep (I_Code_Parameter);
         Len_Type : constant Irep := Do_Type_Reference (Index_Type);
         Func_Symbol : Symbol;
         Map_Size_Str : constant String :=
           Integer'Image (Integer (Array_Dup_Map.Length));
         Func_Name : constant String :=
           "__ada_dup_array" & Map_Size_Str (2 .. Map_Size_Str'Last);
         Array_Copy : constant Irep :=
           Fresh_Var_Symbol_Expr (Ptr_Type, "new_array");
         Array_Alloc : constant Irep :=
           New_Irep (I_Side_Effect_Expr_Cpp_New_Array);
         Body_Block : constant Irep := New_Irep (I_Code_Block);
         Call_Inst : constant Irep := New_Irep (I_Code_Function_Call);
         Call_Args : constant Irep := New_Irep (I_Argument_List);
         Return_Inst : constant Irep := New_Irep (I_Code_Return);

      begin

         --  Create type (element_type*, index_type) -> element_type*
         Set_Type (Ptr_Arg, Ptr_Type);
         Set_Identifier (Ptr_Arg, Func_Name & "::ptr");
         Set_Base_Name (Ptr_Arg, "ptr");
         Set_Type (Len_Arg, Len_Type);
         Set_Identifier (Len_Arg, Func_Name & "::len");
         Set_Base_Name (Len_Arg, "len");
         Append_Parameter (Func_Args, Ptr_Arg);
         Append_Parameter (Func_Args, Len_Arg);
         Set_Parameters (Func_Type, Func_Args);
         Set_Return_Type (Func_Type, Ptr_Type);

         --  Create body (allocate and then call array_copy)
         Set_Size (Array_Alloc, Param_Symbol (Len_Arg));
         Set_Type (Array_Alloc, Ptr_Type);
         Append_Declare_And_Init (Array_Copy, Array_Alloc, Body_Block, 0);
         Append_Argument (Call_Args, Array_Copy);
         Append_Argument (Call_Args, Param_Symbol (Ptr_Arg));
         Append_Argument (Call_Args, Param_Symbol (Len_Arg));
         Set_Arguments (Call_Inst, Call_Args);
         Set_Function (Call_Inst,
                       Get_Array_Copy_Function (Element_Type,
                                                Element_Type,
                                                Index_Type));
         Append_Op (Body_Block, Call_Inst);

         Set_Return_Value (Return_Inst, Array_Copy);
         Append_Op (Body_Block, Return_Inst);

         --  Make function symbol:
         Func_Symbol.SymType := Func_Type;
         Func_Symbol.Name := Intern (Func_Name);
         Func_Symbol.PrettyName := Func_Symbol.Name;
         Func_Symbol.BaseName := Func_Symbol.Name;
         Func_Symbol.Mode := Intern ("C");
         Func_Symbol.Value := Body_Block;
         Global_Symbol_Table.Insert (Intern (Func_Name), Func_Symbol);

         --  Record it for the future:
         Array_Dup_Map.Replace_Element (Map_Cursor, Symbol_Expr (Func_Symbol));

         return Array_Dup_Maps.Element (Map_Cursor);

      end;
   end Get_Array_Dup_Function;

   --------------------------
   -- Get_Array_Index_Type --
   --------------------------

   function Get_Array_Index_Type (N : Node_Id) return Entity_Id is
      Ret : Entity_Id := Etype (First_Index (Etype (N)));
   begin
      --  Many array index types are itypes with ranges private to
      --  this particular context. Use the underlying, unconstrained
      --  numeric type instead.
      while Ekind (Ret) = E_Signed_Integer_Subtype loop
         Ret := Etype (Ret);
      end loop;
      return Ret;
   end Get_Array_Index_Type;

   -------------------------
   -- Get_Fresh_Type_Name --
   -------------------------

   function Get_Fresh_Type_Name (Actual_Type : Irep;
                                 Associated_Node : Node_Id) return Irep
   is
      Ret : constant Irep := New_Irep (I_Symbol_Type);
      Number_Str_Raw : constant String :=
        Integer'Image (Anonymous_Type_Counter);
      Number_Str : constant String :=
        Number_Str_Raw (2 .. Number_Str_Raw'Last);
      Fresh_Name : constant String := "__anonymous_type_" & Number_Str;
      Type_Symbol : Symbol;
   begin
      Anonymous_Type_Counter := Anonymous_Type_Counter + 1;

      Type_Symbol.SymType := Actual_Type;
      Type_Symbol.IsType := True;
      Type_Symbol.Name := Intern (Fresh_Name);
      Type_Symbol.PrettyName := Intern (Fresh_Name);
      Type_Symbol.BaseName := Intern (Fresh_Name);
      Type_Symbol.Mode := Intern ("C");
      Global_Symbol_Table.Insert (Intern (Fresh_Name), Type_Symbol);

      Set_Identifier (Ret, Fresh_Name);

      Anonymous_Type_Map.Insert (Associated_Node, Ret);

      return Ret;
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
         Append (Variant_Name,
                 "_" & Get_Name_String (Chars (Constraint_Iter)));
         Next (Constraint_Iter);
      end loop;
      return To_String (Variant_Name);
   end Get_Variant_Union_Member_Name;

   ---------------------------
   -- Make_Array_First_Expr --
   ---------------------------

   function Make_Array_First_Expr
     (Base_Type : Node_Id; Base_Irep : Irep) return Irep
   is
      Idx_Type : constant Node_Id := Etype (First_Index (Base_Type));
      First : constant Irep := New_Irep (I_Member_Expr);
   begin
      Set_Component_Name (First, "first1");
      Set_Compound (First, Base_Irep);
      Set_Type (First, Do_Type_Reference (Idx_Type));
      return First;
   end Make_Array_First_Expr;

   -------------------------
   -- Make_Array_Index_Op --
   -------------------------

   function Make_Array_Index_Op
     (Base_Irep : Irep; Base_Type : Node_Id; Idx_Irep : Irep) return Irep
   is
      First_Irep : constant Irep :=
        Make_Array_First_Expr (Base_Type, Base_Irep);
      Zero_Based_Index : constant Irep := New_Irep (I_Op_Sub);
      Result_Type : constant Irep :=
        Do_Type_Reference (Component_Type (Base_Type));
      Data : constant Irep := New_Irep (I_Member_Expr);
      Offset : constant Irep := New_Irep (I_Op_Add);
      Deref : constant Irep := New_Irep (I_Dereference_Expr);
      Pointer_Type : constant Irep := New_Irep (I_Pointer_Type);
   begin
      Set_Lhs (Zero_Based_Index, Idx_Irep);
      Set_Rhs (Zero_Based_Index, First_Irep);
      Set_Type (Zero_Based_Index, Get_Type (Idx_Irep));
      Set_Component_Name (Data, "data");
      Set_Compound (Data, Base_Irep);
      Set_Subtype (Pointer_Type, Result_Type);
      Set_Type (Data, Pointer_Type);
      Set_Lhs (Offset, Data);
      Set_Rhs (Offset, Idx_Irep);
      Set_Type (Offset, Pointer_Type);
      Set_Object (Deref, Offset);
      Set_Type (Deref, Result_Type);
      return Deref;
   end Make_Array_Index_Op;

   ----------------------------
   -- Make_Array_Length_Expr --
   ----------------------------

   function Make_Array_Length_Expr
     (Array_Struct : Irep; Index_Type : Entity_Id) return Irep
   is
      First_Expr : constant Irep := New_Irep (I_Member_Expr);
      Last_Expr : constant Irep := New_Irep (I_Member_Expr);
      Diff : constant Irep := New_Irep (I_Op_Sub);
      Sum : constant Irep := New_Irep (I_Op_Add);
      Index_Type_Irep : constant Irep := Do_Type_Reference (Index_Type);
      One : constant Irep := Make_Integer_Constant (1, Index_Type);
   begin
      Set_Compound (First_Expr, Array_Struct);
      Set_Component_Name (First_Expr, "first1");
      Set_Type (First_Expr, Index_Type_Irep);
      Set_Compound (Last_Expr, Array_Struct);
      Set_Component_Name (Last_Expr, "last1");
      Set_Type (Last_Expr, Index_Type_Irep);
      Set_Lhs (Diff, Last_Expr);
      Set_Rhs (Diff, First_Expr);
      Set_Type (Diff, Index_Type_Irep);
      Set_Lhs (Sum, Diff);
      Set_Rhs (Sum, One);
      Set_Type (Sum, Index_Type_Irep);
      return Sum;
   end Make_Array_Length_Expr;

   --------------------
   -- Make_Increment --
   --------------------

   function Make_Increment
     (Sym : Irep; Sym_Type : Node_Id; Amount : Integer) return Irep
   is
      Inc : constant Irep := New_Irep (I_Side_Effect_Expr_Assign);
      Plus : constant Irep := New_Irep (I_Op_Add);
      Amount_Expr : constant Irep :=
        Make_Integer_Constant (Amount, Sym_Type);
   begin
      Set_Lhs (Plus, Sym);
      Set_Rhs (Plus, Amount_Expr);
      Set_Type (Plus, Get_Type (Sym));

      Set_Lhs (Inc, Sym);
      Set_Rhs (Inc, Plus);
      return Inc;
   end Make_Increment;

   ---------------------------
   -- Make_Integer_Constant --
   ---------------------------

   function Make_Integer_Constant (Val : Integer; Ty : Node_Id) return Irep is
      Type_Width : constant Int := UI_To_Int (Esize (Ty));
      Val_Binary : constant String := Convert_Int_To_Binary (Val, Type_Width);
      Ret : constant Irep := New_Irep (I_Constant_Expr);
   begin
      Set_Type (Ret, Do_Type_Reference (Ty));
      Set_Value (Ret, Val_Binary);
      return Ret;
   end Make_Integer_Constant;

   ------------------------
   -- Make_Pointer_Index --
   ------------------------

   function Make_Pointer_Index (Base : Irep; Idx : Irep) return Irep is
      Offset : constant Irep := New_Irep (I_Op_Add);
      Deref : constant Irep := New_Irep (I_Dereference_Expr);
      Pointer_Type : constant Irep := Get_Type (Base);
      Element_Type : constant Irep := Get_Subtype (Pointer_Type);
   begin
      Set_Lhs (Offset, Base);
      Set_Rhs (Offset, Idx);
      Set_Type (Offset, Pointer_Type);
      Set_Object (Deref, Offset);
      Set_Type (Deref, Element_Type);
      return Deref;
   end Make_Pointer_Index;

   ------------------------
   -- Make_Runtime_Check --
   ------------------------

   function Make_Runtime_Check (Condition : Irep) return Irep
   is
      Call_Expr : constant Irep := New_Irep (I_Side_Effect_Expr_Function_Call);
      Call_Args : constant Irep := New_Irep (I_Argument_List);
      Void_Type : constant Irep := New_Irep (I_Void_Type);
   begin

      if Check_Function_Symbol = Ireps.Empty then
         --  Create the check function on demand:
         declare
            Fn_Symbol : Symbol;
            Assertion : constant Irep := New_Irep (I_Code_Assert);
            Formal_Params : constant Irep := New_Irep (I_Parameter_List);
            Formal_Param : constant Irep := New_Irep (I_Code_Parameter);
            Formal_Expr : constant Irep := New_Irep (I_Symbol_Expr);
            Fn_Type : constant Irep := New_Irep (I_Code_Type);
            Bool_Type : constant Irep := New_Irep (I_Bool_Type);
         begin
            Set_Identifier (Formal_Param, "__ada_runtime_check::arg");
            Set_Base_Name (Formal_Param, "arg");
            Set_Type (Formal_Param, Bool_Type);
            Set_Identifier (Formal_Expr, Get_Identifier (Formal_Param));
            Set_Type (Formal_Expr, Get_Type (Formal_Param));
            Append_Parameter (Formal_Params, Formal_Param);
            Set_Parameters (Fn_Type, Formal_Params);
            Set_Return_Type (Fn_Type, Void_Type);
            Set_Assertion (Assertion, Formal_Expr);

            Fn_Symbol.Name := Intern ("__ada_runtime_check");
            Fn_Symbol.PrettyName := Fn_Symbol.Name;
            Fn_Symbol.BaseName := Fn_Symbol.Name;
            Fn_Symbol.Value := Assertion;
            Fn_Symbol.SymType := Fn_Type;
            Global_Symbol_Table.Insert (Fn_Symbol.Name, Fn_Symbol);

            Check_Function_Symbol := New_Irep (I_Symbol_Expr);
            Set_Identifier (Check_Function_Symbol, Unintern (Fn_Symbol.Name));
            Set_Type (Check_Function_Symbol, Fn_Symbol.SymType);
         end;
      end if;

      --  Create a call to the (newly created?) function:
      Set_Function (Call_Expr, Check_Function_Symbol);
      Set_Type (Call_Expr, Void_Type);
      Append_Argument (Call_Args, Condition);
      Set_Arguments (Call_Expr, Call_Args);

      return Call_Expr;

   end Make_Runtime_Check;

   -----------------------------
   --  Make_Struct_Component  --
   -----------------------------

   function Make_Struct_Component (Name : String; Ty : Irep) return Irep is
      Ret : constant Irep := New_Irep (I_Struct_Union_Component);
   begin
      --  Set attributes we don't use yet:
      Set_Access (Ret, "public");
      Set_Is_Padding (Ret, False);
      Set_Anonymous (Ret, False);
      --  Real attributes:
      Set_Name        (Ret, Name);
      Set_Pretty_Name (Ret, Name);
      Set_Base_Name   (Ret, Name);
      Set_Type        (Ret, Ty);
      return Ret;
   end Make_Struct_Component;

   ------------------------
   --  Make_Type_Symbol  --
   ------------------------

   function Make_Type_Symbol (Name : Symbol_Id; Defn : Irep) return Symbol is
      (Name => Name,
       PrettyName => Name,
       BaseName => Name,
       SymType => Defn,
       Mode => Intern ("C"),
       IsType => True,
       others => <>);

   ---------------------------
   --  Maybe_Make_Typecast  --
   ---------------------------

   function Maybe_Make_Typecast (Expr : Irep;
                                 Old_Type : Entity_Id;
                                 New_Type : Entity_Id) return Irep
   is
   begin
      if Old_Type = New_Type then
         return Expr;
      end if;
      declare
         Ret : constant Irep := New_Irep (I_Op_Typecast);
      begin
         Set_Type (Ret, Do_Type_Reference (New_Type));
         Set_Op0 (Ret, Expr);
         return Ret;
      end;
   end Maybe_Make_Typecast;

   -------------------------
   --  Process_Statement  --
   -------------------------

   procedure Process_Statement (N : Node_Id; Block : Irep) is
   begin
      --  Deal with the statement
      case Nkind (N) is
         when N_Assignment_Statement =>
            Append_Op (Block, Do_Assignment_Statement (N));

         when N_Procedure_Call_Statement =>
            Append_Op (Block, Do_Procedure_Call_Statement (N));

         when N_Simple_Return_Statement =>
            Append_Op (Block, Do_Simple_Return_Statement (N));

         when N_Object_Declaration =>
            Do_Object_Declaration (N, Block);

         when N_Handled_Sequence_Of_Statements =>
            Append_Op (Block, Do_Handled_Sequence_Of_Statements (N));

         when N_If_Statement =>
            Append_Op (Block, Do_If_Statement (N));

         when N_Implicit_Label_Declaration =>
            --  Ignore for now, as I guess an implicit label can't be
            --  referenced.
            null;

         when N_Loop_Statement =>
            Append_Op (Block, Do_Loop_Statement (N));

         when N_Full_Type_Declaration =>
            Do_Full_Type_Declaration (N);

         when N_Subtype_Declaration =>
            Do_Subtype_Declaration (N);

         when N_Freeze_Entity =>
            --  Ignore, nothing to generate
            null;

         when N_Itype_Reference =>
            Do_Itype_Reference (N);

         when N_Subprogram_Declaration =>
            Do_Subprogram_Declaration (N);

         when N_Subprogram_Body =>
            Do_Subprogram_Body (N);

         when N_Null_Statement =>
            null;

         when others =>
            pp (Union_Id (N));
            --  ??? To be added later
            raise Program_Error;

      end case;
   end Process_Statement;

   ------------------------
   -- Process_Statements --
   ------------------------

   function Process_Statements (L : List_Id) return Irep is
      Reps : constant Irep := New_Irep (I_Code_Block);
      Stmt : Node_Id := First (L);

   begin
      while Present (Stmt) loop
         Process_Statement (Stmt, Reps);
         Next (Stmt);
      end loop;

      return Reps;
   end Process_Statements;

   function "<" (Left, Right : Array_Dup_Key) return Boolean is
   begin
      if Left.Element_Type /= Right.Element_Type then
         return Left.Element_Type < Right.Element_Type;
      end if;
      return Left.Index_Type < Right.Index_Type;
   end "<";

   function "<" (Left, Right : Array_Copy_Key) return Boolean is
   begin
      if Left.LHS_Element_Type /= Right.LHS_Element_Type then
         return Left.LHS_Element_Type < Right.LHS_Element_Type;
      end if;
      if Left.RHS_Element_Type /= Right.RHS_Element_Type then
         return Left.RHS_Element_Type < Right.RHS_Element_Type;
      end if;
      return Left.Index_Type < Right.Index_Type;
   end "<";

end Tree_Walk;
