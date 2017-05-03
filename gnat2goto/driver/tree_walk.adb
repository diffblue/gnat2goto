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

   function Do_Address_Of (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Attribute_Reference,
        Post => Kind (Do_Address_Of'Result) = I_Address_Of_Expr;

   function Do_Aggregate_Literal (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Aggregate,
        Post => Kind (Do_Aggregate_Literal'Result) = I_Struct_Expr;

   function Do_Assignment_Statement (N  : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Assignment_Statement,
        Post => Kind (Do_Assignment_Statement'Result) = I_Code_Assign;

   function Do_Call_Parameters (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Procedure_Call_Statement | N_Function_Call,
        Post => Kind (Do_Call_Parameters'Result) = I_Argument_List;

   function Do_Case_Expression (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_Case_Expression,
        Post => Kind (Do_Case_Expression'Result) = I_Let_Expr;

   function Do_Constant (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_Integer_Literal,
        Post => Kind (Do_Constant'Result) = I_Constant_Expr;

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

   function Do_Function_Call (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Function_Call,
        Post => Kind (Do_Function_Call'Result) in Class_Expr;

   function Do_Index_Or_Discriminant_Constraint
     (N : Node_Id; Underlying : Irep) return Irep
   with Pre  => Nkind (N) = N_Index_Or_Discriminant_Constraint;

   procedure Do_Itype_Reference (N : Node_Id)
   with Pre => Nkind (N) = N_Itype_Reference;

   function Do_Loop_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Loop_Statement,
        Post => Kind (Do_Loop_Statement'Result) in Class_Code;

   procedure Do_Object_Declaration (N : Node_Id; Block : Irep)
   with Pre => Nkind (N) = N_Object_Declaration
                 and then Kind (Block) = I_Code_Block;

   function Do_Operator (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Op,
        Post => Kind (Do_Operator'Result) in Class_Expr;

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

   function Get_Fresh_Type_Name (Actual_Type : Irep; Associated_Node : Node_Id)
                                return Irep;

   function Get_Variant_Union_Member_Name (N : Node_Id) return String;

   function Make_Runtime_Check (Condition : Irep) return Irep
   with Pre  => Kind (Get_Type (Condition)) = I_Bool_Type,
        Post => Kind (Make_Runtime_Check'Result) =
                I_Side_Effect_Expr_Function_Call;

   procedure Process_Statement (N : Node_Id; Block : Irep)
   with Pre => Kind (Block) = I_Code_Block;
   --  Process statement or declaration

   function Process_Statements (L : List_Id) return Irep
   with Post => Kind (Process_Statements'Result) = I_Code_Block;
   --  Process list of statements or declarations

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
      N_Type_Decl : constant Node_Id := Parent (N_Type);
      N_Underlying_Type : constant Node_Id := Etype (N_Type);
      Disc_Constraint : Node_Id := Types.Empty;
      Struct_Expr : constant Irep := New_Irep (I_Struct_Expr);

   begin
      case Ekind (N_Type) is
         when E_Record_Subtype =>
            --  Perhaps carrying a variant constraint:
            Disc_Constraint := Constraint (Subtype_Indication (N_Type_Decl));
         when E_Record_Type =>
            --  Unconstrained, nothing to do
            null;
         when others =>
            --  Unhandled aggregate kind
            pp (Union_Id (N));
            raise Program_Error;
      end case;

      --  It appears GNAT sorts the aggregate members for us into the order
      --  discriminant (if any), common members, variant members.
      --  However, let's check.
      declare
         Components   : constant Node_Id :=
           Component_List (Type_Definition (Parent (N_Underlying_Type)));
         Variant_Node : constant Node_Id := Variant_Part (Components);

         Component_Iter : Node_Id := First (Component_Items (Components));
         Actual_Iter    : Node_Id := First (Component_Associations (N));
      begin

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

   end Do_Aggregate_Literal;

   -----------------------------
   -- Do_Assignment_Statement --
   -----------------------------

   function Do_Assignment_Statement (N : Node_Id) return Irep
   is
      LHS : constant Irep := Do_Expression (Name (N));
      RHS : constant Irep := Do_Expression (Expression (N));
   begin
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
      function Fresh_Case_Bound_Var_Name return String;
      function Fresh_Case_Bound_Var_Symbol_Expr (Ty : Irep) return Irep;
      function Make_Case_Test (Alts : List_Id) return Irep;

      -------------------------------
      -- Fresh_Case_Bound_Var_Name --
      -------------------------------

      function Fresh_Case_Bound_Var_Name return String is
         Binder_Number_Str_Raw : constant String :=
           Integer'Image (Case_Binder_Counter);
         Binder_Number_Str : constant String :=
           Binder_Number_Str_Raw (2 .. Binder_Number_Str_Raw'Last);
      begin
         --  Note this is intentionally an illegal Ada identifier
         --  to avoid clashes.
         Case_Binder_Counter := Case_Binder_Counter + 1;
         return "__case_bound_var_" & Binder_Number_Str;
      end Fresh_Case_Bound_Var_Name;

      --------------------------------------
      -- Fresh_Case_Bound_Var_Symbol_Expr --
      --------------------------------------

      function Fresh_Case_Bound_Var_Symbol_Expr (Ty : Irep) return Irep is
         Id : constant String := Fresh_Case_Bound_Var_Name;
         Ret : constant Irep := New_Irep (I_Symbol_Expr);
      begin
         Set_Identifier (Ret, Id);
         Set_Type (Ret, Ty);
         return Ret;
      end Fresh_Case_Bound_Var_Symbol_Expr;

      Ret : constant Irep := New_Irep (I_Let_Expr);
      Value : constant Irep := Do_Expression (Expression (N));
      Bound_Var : constant Irep :=
        Fresh_Case_Bound_Var_Symbol_Expr (Get_Type (Value));

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
      return
        (case Nkind (N) is
            when N_Identifier           => Do_Identifier (N),
            when N_Selected_Component   => Do_Selected_Component (N),
            when N_Op                   => Do_Operator (N),
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

   -----------------
   -- Do_Operator --
   -----------------

   function Do_Operator (N : Node_Id) return Irep is
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

   --  Start of processing for Do_Operator

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
   end Do_Operator;

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
   is
      Resolved_Underlying : constant Irep :=
        Follow_Symbol_Type (Underlying, Global_Symbol_Table);
      --  ??? why not get this from the entity

      Range_Expr : constant Node_Id := Range_Expression (N);
   begin
      return R : constant Irep := New_Irep (I_Bounded_Signedbv_Type) do
         Set_Width (R, Get_Width (Resolved_Underlying));
         Set_Lower_Bound (R, Do_Constant (Low_Bound (Range_Expr)));
         Set_Upper_Bound (R, Do_Constant (High_Bound (Range_Expr)));
      end return;
   end Do_Range_Constraint;

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
         Comp_Irep : constant Irep := New_Irep (I_Struct_Union_Component);
      begin
         Set_Source_Location (Comp_Irep, Sloc (Comp_Node));
         --  Set attributes we don't use yet:
         Set_Access (Comp_Irep, "public");
         Set_Is_Padding (Comp_Irep, False);
         Set_Anonymous (Comp_Irep, False);
         --  Real attributes:
         Set_Name        (Comp_Irep, Comp_Name);
         Set_Pretty_Name (Comp_Irep, Comp_Name);
         Set_Base_Name   (Comp_Irep, Comp_Name);
         Set_Type        (Comp_Irep, Comp_Type);

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
            Append_Op (Comma_Expr, Make_Runtime_Check (Disc_Check));

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

            Append_Op (Comma_Expr, Ret);
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

   -------------------------------
   -- Do_Subprogram_Body --
   -------------------------------

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
      New_Type_Symbol : Symbol;

   begin
      if Kind (New_Type) = I_Struct_Type then
         Set_Tag (New_Type, Unintern (New_Type_Name));
      end if;
      New_Type_Symbol.Name       := New_Type_Name;
      New_Type_Symbol.PrettyName := New_Type_Name;
      New_Type_Symbol.BaseName   := New_Type_Name;
      New_Type_Symbol.SymType    := New_Type;
      New_Type_Symbol.Mode       := Intern ("C");
      New_Type_Symbol.IsType     := True;

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

end Tree_Walk;
