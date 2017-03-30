with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Nlists;                use Nlists;
with Sem_util;              use Sem_Util;
with Stand;                 use Stand;
with Treepr;                use Treepr;
with Uintp;                 use Uintp;

with Uint_To_Binary;        use Uint_To_Binary;
with Follow;                use Follow;

package body Tree_Walk is

   function Do_Assignment_Statement (N  : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Assignment_Statement,
        Post => Kind (Do_Assignment_Statement'Result) = I_Code_Assign;

   function Do_Argument_List (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Procedure_Call_Statement | N_Function_Call,
        Post => Kind (Do_Argument_List'Result) = I_Argument_List;

   function Do_Procedure_Call_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Procedure_Call_Statement,
        Post => Kind (Do_Procedure_Call_Statement'Result) =
                  I_Code_Function_Call;

   function Do_Simple_Return_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Simple_Return_Statement,
        Post => Kind (Do_Simple_Return_Statement'Result) = I_Code_Return;

   procedure Do_Object_Declaration (N : Node_Id; Block : Irep)
   with Pre => Nkind (N) = N_Object_Declaration and Kind (Block) = I_Code_Block;

   function Do_If_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_If_Statement,
        Post => Kind (Do_If_Statement'Result) = I_Code_Ifthenelse;

   function Do_Loop_Statement (N : Node_Id) return Irep
   with Pre  => NKind (N) = N_Loop_Statement,
        Post => Kind (Do_Loop_Statement'Result) in Class_Code;

   function Do_Expression (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Subexpr,
        Post => Kind (Do_Expression'Result) in Class_Expr;

   function Do_Handled_Sequence_Of_Statements (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Handled_Sequence_Of_Statements,
        Post => Kind (Do_Handled_Sequence_Of_Statements'Result) = I_Code_Block;

   function Do_Defining_Identifier (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Defining_Identifier,
        Post => Kind (Do_Defining_Identifier'Result) = I_Symbol_Expr;

   function Do_Identifier (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Identifier,
        Post => Kind (Do_Identifier'Result) = I_Symbol_Expr;

   function Do_Operator (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Op,
        Post => Kind (Do_Operator'Result) in Class_Expr;

   function Do_Function_Call (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Function_Call,
        Post => Kind (Do_Function_Call'Result) in Class_Expr;

   function Do_Type_Conversion (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Type_Conversion,
        Post => Kind (Do_Type_Conversion'Result) in Class_Expr;

   function Do_Constant (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_Integer_Literal,
        Post => Kind (Do_Constant'Result) = I_Constant_Expr;

   function Do_Subprogram_Or_Block (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Subprogram_Body |
                             N_Task_Body       |
                             N_Block_Statement |
                             N_Package_Body    |
                             N_Entry_Body,
        Post => Kind (Do_Subprogram_Or_Block'Result) = I_Code_Block;

   procedure Do_Type_Declaration (New_Type_In : Irep; Name_Node : Node_Id)
   with Pre => Nkind (Name_Node) = N_Defining_Identifier and then
               Kind (New_Type_In) in Class_Type;

   procedure Do_Full_Type_Declaration (N : Node_Id)
   with Pre => Nkind (N) = N_Full_Type_Declaration;

   procedure Do_Subtype_Declaration (N : Node_Id)
   with Pre => Nkind (N) = N_Subtype_Declaration;

   function Do_Subtype_Indication (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Subtype_Indication,
        Post => Kind (Do_Subtype_Indication'Result) in Class_Type;

   function Do_Record_Definition (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Record_Definition,
        Post => Kind (Do_Record_Definition'Result) = I_Struct_Type;

   function Do_Signed_Integer_Definition (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Signed_Integer_Type_Definition,
        Post => Kind (Do_Signed_Integer_Definition'Result) =
                  I_Bounded_Signedbv_Type;

   function Do_Derived_Type_Definition (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Derived_Type_Definition,
        Post => Kind (Do_Derived_Type_Definition'Result) in Class_Type;

   function Do_Type_Reference (N : Node_Id) return Irep
   --  ??? Pre
   with Post => Kind (Do_Type_Reference'Result) in Class_Type;

   function Do_Selected_Component (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Selected_Component,
        Post => Kind (Do_Selected_Component'Result) = I_Member_Expr;

   procedure Do_Subprogram_Declaration (N : Node_Id)
   with Pre => Nkind (N) = N_Subprogram_Declaration;

   function Do_Subprogram_Specification (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Subprogram_Specification,
        Post => Kind (Do_Subprogram_Specification'Result) = I_Code_Type;

   procedure Do_Subprogram_Body (N : Node_Id)
   with Pre => Nkind (N) = N_Subprogram_Body;

   procedure Process_Statement (N : Node_Id; Block : Irep)
   with Pre => Kind (Block) = I_Code_Block;
   --  Process statement or declaration

   function Process_Statement_List (L : List_Id) return Irep
   with Post => Kind (Process_Statement_List'Result) = I_Code_Block;
   --  Process list of statements or declarations

   function Make_Int_Type (Width : Positive) return Irep is
   begin
      return I : constant Irep := New_Irep (I_Signedbv_Type) do
         Set_Width (I, Width);
      end return;
   end Make_Int_Type;

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
            Set_RHS (R, RHS);
         end if;
      end return;
   end Do_Assignment_Statement;

   ---------------------------------
   -- Do_Procedure_Call_Statement --
   ---------------------------------

   function Do_Procedure_Call_Statement (N : Node_Id) return Irep
   is
      Proc_Name : constant Unbounded_String :=
        To_Unbounded_String (Unique_Name (N));

      Proc : constant Irep := New_Irep (I_Symbol_Expr);
      R    : constant Irep := New_Irep (I_Code_Function_Call);
   begin
      Set_Identifier (Proc, To_String (Proc_Name));
      Set_Type (Proc, Global_Symbol_Table (Proc_Name).SymType);
      --  ??? Why not look at type of entity?

      Set_Source_Location (R, Sloc (N));
      --  Set_LHS (R, Empty);  -- ??? what is the "nil" irep?
      Set_Function (R, Proc);
      Set_Arguments (R, Do_Argument_List (N));

      return R;
   end Do_Procedure_Call_Statement;

   --------------------------------
   -- Do_Simple_Return_Statement --
   --------------------------------

   function Do_Simple_Return_Statement (N : Node_Id) return Irep is
   begin
      return R : constant Irep := New_Irep (I_Code_Return) do
         Set_Source_Location (R, Sloc (N));
         if Present (Expression (N)) then
            Set_Return_Value (R, Do_Expression (Expression (N)));
         end if;
      end return;
   end;

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
               Unit_Name : constant Unbounded_String :=
                 To_Unbounded_String (Unique_Name (Unique_Defining_Entity (U)));
            begin
               -- Register the symbol *before* we compile the body, for
               -- recursive calls.
               Unit_Symbol.Name       := Unit_Name;
               Unit_Symbol.PrettyName := Unit_Name;
               Unit_Symbol.BaseName   := Unit_Name;
               Unit_Symbol.Mode       := To_Unbounded_String ("C");
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

   ----------------------------
   -- Do_Defining_Identifier --
   ----------------------------

   function Do_Defining_Identifier (N : Node_Id) return Irep is
   begin
      return R : constant Irep := New_Irep (I_Symbol_Expr) do
         Set_Source_Location (R, Sloc (N));
         Set_Identifier (R, Unique_Name (N));
         --  !!! use of chars
         Set_Type (R, Do_Type_Reference (Etype (N)));
      end return;
   end Do_Defining_Identifier;

   ----------------------
   -- Do_Argument_List --
   ----------------------

   function Do_Argument_List (N : Node_Id) return Irep
   is
      Arg_Iter : Node_Id := First (Parameter_Associations (N));
   begin
      --  !!! this will not work for named or mixed parameters
      return R : constant Irep := New_Irep (I_Argument_List) do
         while Present (Arg_Iter) loop
            Append_Argument (R, Do_Expression (Arg_Iter));
            Next (Arg_Iter);
         end loop;
      end return;
   end Do_Argument_List;

   ----------------------
   -- Do_Function_Call --
   ----------------------

   function Do_Function_Call (N : Node_Id) return Irep
   is
      Func_Name    : constant Unbounded_String :=
        To_Unbounded_String (Unique_Name (Entity (Name (N))));

      Func_Symbol  : constant Symbol := Global_Symbol_Table (Func_Name);

      The_Function : constant Irep := New_Irep (I_Symbol_Expr);
   begin
      Set_Identifier (The_Function, To_String (Func_Name));
      Set_Type (The_Function, Func_Symbol.SymType);
      --  ??? why not get this from the entity

      return R : constant Irep := New_Irep (I_Side_Effect_Expr_Function_Call)
      do
         Set_Source_Location (R, Sloc (N));
         Set_Function (R, The_Function);
         Set_Arguments (R, Do_Argument_List (N));
      end return;
   end Do_Function_Call;

   -------------------
   -- Do_Expression --
   -------------------

   function Do_Expression (N : Node_Id) return Irep is
   begin
      case Nkind (N) is
         when N_Identifier =>         return Do_Identifier (N);
         when N_Selected_Component => return Do_Selected_Component (N);
         when N_Op =>                 return Do_Operator (N);
         when N_Integer_Literal =>    return Do_Constant (N);
         when N_Type_Conversion =>    return Do_Type_Conversion (N);
         when N_Function_Call =>      return Do_Function_Call (N);

         when others =>
            raise Program_Error;
      end case;
   end Do_Expression;

   -------------------------
   -- Do_Range_Constraint --
   -------------------------

   function Do_Range_Constraint (N : Node_Id; Underlying : Irep)
                                 return Irep
   is
      Resolved_Underlying : constant Irep :=
        Follow_Symbol_Type (Underlying, Global_Symbol_Table);
   begin
      return R : constant Irep := New_Irep (I_Bounded_Signedbv_Type) do
         Set_Width (R, Get_Width (Resolved_Underlying));
         Set_Lower_Bound (R, Do_Constant (Low_Bound (Range_Expression (N))));
         Set_Upper_Bound (R, Do_Constant (High_Bound (Range_Expression (N))));
      end return;
   end;

   ---------------------------
   -- Do_Subtype_Indication --
   ---------------------------

   function Do_Subtype_Indication (N : Node_Id) return Irep
   is
      Underlying : constant Irep :=
        Do_Type_Reference (EType (Subtype_Mark (N)));
   begin
      if Present (Constraint (N)) then
         case Nkind (Constraint (N)) is
            when N_Range_Constraint =>
               return Do_Range_Constraint (Constraint (N), Underlying);

            when others =>
               Print_Tree_Node (N);
               raise Program_Error;
         end case;
      else
         return Underlying;
      end if;
   end Do_Subtype_Indication;

   --------------------------
   -- Do_Record_Definition --
   --------------------------

   function Do_Record_Definition (N : Node_Id) return Irep is
      Ret : constant Irep := New_Irep (I_Struct_Type);
      Components : constant Irep := New_Irep (I_Struct_Union_Components);
      Component_Iter : Node_Id := First (Component_Items (Component_List (N)));
      procedure Do_Record_Component (Comp : Node_Id) is
         Comp_Name : constant String := Unique_Name (Defining_Identifier (Comp));
         Comp_Defn : constant Node_Id := Component_Definition (Comp);
         Comp_Irep : constant Irep := New_Irep (I_Struct_Union_Component);
      begin
         Set_Source_Location (Comp_Irep, Sloc (Comp));
         -- Set attributes we don't use yet:
         Set_Access (Comp_Irep, "public");
         Set_Is_Padding (Comp_Irep, False);
         Set_Anonymous (Comp_Irep, False);
         if not Present (Subtype_Indication (Comp_Defn)) then
            Pp (Union_Id (Comp_Defn));
            raise Program_Error;
         else
            declare
               Comp_Type : constant Irep := Do_Type_Reference (Entity (Subtype_Indication (Comp_Defn)));
            begin
               Set_Name (Comp_Irep, Comp_Name);
               Set_Pretty_Name (Comp_Irep, Comp_Name);
               Set_Base_Name (Comp_Irep, Comp_Name);
               Set_Type (Comp_Irep, Comp_Type);
            end;
         end if;
         Append_Component (Components, Comp_Irep);
      end;
   begin
      while Present (Component_Iter) loop
         Do_Record_Component (Component_Iter);
         Next (Component_Iter);
      end loop;
      Set_Components (Ret, Components);
      return Ret;
   end Do_Record_Definition;

   function Pick_Underlying_Type_Width (Val : Uint) return Integer is
      Ret : Integer := 8;
   begin
      while Val >= UI_From_Int (Int (2)) ** UI_From_Int (Int (Ret - 1)) or else
        Val < -(UI_From_Int (Int (2)) ** UI_From_Int (Int (Ret - 1))) loop
         Ret := Ret * 2;
      end loop;
      return Ret;
   end Pick_Underlying_Type_Width;

   ----------------------------------
   -- Do_Signed_Integer_Definition --
   ----------------------------------

   function Do_Signed_Integer_Definition (N : Node_Id) return Irep is
      Lower : constant Irep := Do_Constant (Low_Bound (N));
      Upper : constant Irep := Do_Constant (High_Bound (N));
      Ret : constant Irep := New_Irep (I_Bounded_Signedbv_Type);
   begin
      Set_Lower_Bound (Ret, Lower);
      Set_Upper_Bound (Ret, Upper);
      Set_Width (Ret, Integer'Max (Pick_Underlying_Type_Width (Intval (Low_Bound (N))),
                                   Pick_Underlying_Type_Width (Intval (High_Bound (N)))));
      return  Ret;
   end Do_Signed_Integer_Definition;

   --------------------------------
   -- Do_Derived_Type_Definition --
   --------------------------------

   function Do_Derived_Type_Definition (N : Node_Id) return Irep is
      Subtype_Irep : constant Irep := Do_Subtype_Indication (Subtype_Indication (N));
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
         Pp (Union_Id (N));
         raise Program_Error;
      end if;
      return Subtype_Irep;
   end Do_Derived_Type_Definition;

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
         when others =>
            Pp (Union_Id (N));
            raise Program_Error;
      end case;
   end Do_Type_Definition;

   -------------------------
   -- Do_Type_Declaration --
   -------------------------

   procedure Do_Type_Declaration (New_Type_In : Irep; Name_Node : Node_Id) is
      New_Type : constant Irep := New_Type_In;
      New_Type_Name : constant Unbounded_String :=
        To_Unbounded_String (Unique_Name (Name_Node));
      New_Type_Symbol : Symbol;
   begin
      if Kind (New_Type) = I_Struct_Type then
         Set_Tag (New_Type, To_String (New_Type_Name));
      end if;
      New_Type_Symbol.Name := New_Type_Name;
      New_Type_Symbol.PrettyName := New_Type_Name;
      New_Type_Symbol.BaseName := New_Type_Name;
      New_Type_Symbol.SymType := New_Type;
      New_Type_Symbol.Mode := To_Unbounded_String ("C");
      New_Type_Symbol.IsType := True;
      Symbol_Maps.Insert (Global_Symbol_Table, New_Type_Name, New_Type_Symbol);
   end;

   ------------------------------
   -- Do_Full_Type_Declaration --
   ------------------------------

   procedure Do_Full_Type_Declaration (N : Node_Id) is
      New_Type : constant Irep := Do_Type_Definition (Type_Definition (N));
   begin
      Do_Type_Declaration (New_Type, Defining_Identifier (N));
      -- Declare the implicit initial subtype too:
      if Etype (Defining_Identifier(N)) /= Defining_Identifier (N) then
         Do_Type_Declaration (New_Type, Etype (Defining_Identifier (N)));
      end if;
   end Do_Full_Type_Declaration;

   ----------------------------
   -- Do_Subtype_Declaration --
   ----------------------------

   procedure Do_Subtype_Declaration (N : Node_Id) is
      New_Type : constant Irep := Do_Subtype_Indication (Subtype_Indication (N));
   begin
      Do_Type_Declaration (New_Type, Defining_Identifier (N));
   end Do_Subtype_Declaration;

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
            Param_Type : constant Irep :=
              Do_Type_Reference (EType (Parameter_Type (Param_Iter)));
            Param_Name : constant String :=
              Unique_Name (Defining_Identifier (Param_Iter));
            Param_Irep : constant Irep := New_Irep (I_Code_Parameter);
            Param_Symbol : Symbol;
         begin
            Set_Source_Location (Param_Irep, Sloc (Param_Iter));
            Set_Type (Param_Irep, Param_Type);
            Set_Identifier (Param_Irep, Param_Name);
            Set_Base_Name (Param_Irep, Param_Name);
            Append_Parameter (Param_List, Param_Irep);
            -- Add the param to the symtab as well:
            Param_Symbol.Name := To_Unbounded_String (Param_Name);
            Param_Symbol.PrettyName := Param_Symbol.Name;
            Param_Symbol.BaseName := Param_Symbol.Name;
            Param_Symbol.SymType := Param_Type;
            Param_Symbol.IsThreadLocal := True;
            Param_Symbol.IsFileLocal := True;
            Param_Symbol.IsLValue := True;
            Param_Symbol.IsParameter := True;
            Symbol_Maps.Insert (Global_Symbol_Table, Param_Symbol.Name, Param_Symbol);
            Next (Param_Iter);
         end;
      end loop;
      if Nkind(N) = N_Function_Specification then
         Set_Return_Type (Ret, Do_Type_Reference (EType (Result_Definition (N))));
      else
         Set_Return_Type (Ret, New_Irep (I_Void_Type));
      end if;
      Set_Parameters (Ret, Param_List);
      return Ret;
   end Do_Subprogram_Specification;

   -------------------------------
   -- Do_Subprogram_Declaration --
   -------------------------------

   procedure Do_Subprogram_Declaration (N : Node_Id) is
      Proc_Symbol : Symbol;
      Proc_Type : constant Irep := Do_Subprogram_Specification (Specification (N));
      Proc_Name : constant Unbounded_String :=
        To_Unbounded_String (Unique_Name (Corresponding_Body (N)));
   begin
      Proc_Symbol.Name := Proc_Name;
      Proc_Symbol.BaseName := Proc_Name;
      Proc_Symbol.PrettyName := Proc_Name;
      Proc_Symbol.SymType := Proc_Type;
      Proc_Symbol.Mode := To_Unbounded_String ("C");
      Symbol_Maps.Insert (Global_Symbol_Table, Proc_Name, Proc_Symbol);
   end Do_Subprogram_Declaration;

   -------------------------------
   -- Do_Subprogram_Body --
   -------------------------------

   procedure Do_Subprogram_Body (N : Node_Id) is
      Proc_Name : constant Unbounded_String :=
        To_Unbounded_String (Unique_Name (Corresponding_Spec (N)));
      Proc_Symbol : Symbol := Global_Symbol_Table (Proc_Name);
      Proc_Body : constant Irep := Do_Subprogram_Or_Block (N);
   begin
      Proc_Symbol.Value := Proc_Body;
      Symbol_Maps.Replace (Global_Symbol_Table, Proc_Name, Proc_Symbol);
   end Do_Subprogram_Body;

   -----------------------
   -- Do_Type_Reference --
   -----------------------

   function Do_Type_Reference (N : Node_Id) return Irep is
   begin
      if N = Standard_Integer then
         return Make_Int_Type (32);
      elsif Nkind (N) = N_Defining_Identifier then
         declare
            Ret : constant Irep := New_Irep (I_Symbol_Type);
            Id : constant String := Unique_Name (N);
         begin
            if (Id = "integer") then
               return Make_Int_Type (32);
            elsif (Id = "boolean") then
               return New_Irep (I_Bool_Type);
            end if;

            Set_Identifier (Ret, Id);
            return Ret;
         end;
      else
         Pp (Union_Id (N));
         raise Program_Error;
      end if;
   end;

   ---------------------------------------
   -- Do_Handled_Sequence_Of_Statements --
   ---------------------------------------

   function Do_Handled_Sequence_Of_Statements (N : Node_Id) return Irep is
      Stmts : constant List_Id := Statements (N);
   begin
      return Process_Statement_List (Stmts);
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

      function Do_If_Block (N : Node_Id) return Irep is
         Cond_Expr : constant Irep := Do_Expression (Condition (N));
         If_Block : constant Irep := Process_Statement_List (Then_Statements (N));
         Ret : constant Irep := New_Irep (I_Code_Ifthenelse);
      begin
         Set_Source_Location (Ret, Sloc (N));
         Set_Cond (Ret, Cond_Expr);
         Set_Then_Case (Ret, If_Block);
         return Ret;
      end;

      procedure Do_Elsifs (Else_Ifs : Node_Id;
                           Else_List : List_Id;
                           Ret : Irep) is
      begin
         if not Present (Else_Ifs) then
            if (Present (Else_List)) then
               Set_Else_Case (Ret, Process_Statement_List (Else_List));
            end if;
         else
            declare
               Sub_If : constant Irep := Do_If_Block (Else_Ifs);
               Next_Eif : Node_Id := Else_Ifs;
            begin
               Next (Next_Eif);
               Do_Elsifs (Next_Eif, Else_List, Sub_If);
               Set_Else_Case (Ret, Sub_If);
            end;
         end if;
      end;

      Ret : constant Irep := Do_If_Block (N);

   begin
      Do_Elsifs (First (Elsif_Parts (N)), Else_Statements (N), Ret);
      return Ret;
   end;

   -----------------------
   -- Do_Loop_Statement --
   -----------------------

   function Do_Loop_Statement (N : Node_Id) return Irep is

      Iter_Scheme : constant Node_Id := Iteration_Scheme (N);
      Body_Block : constant Irep := Process_Statement_List (Statements (N));

      function Do_For_Statement return Irep is
         Ret : constant Irep := New_Irep (I_Code_For);
      begin
         -- ??? What happens to Body_Block here?
         Set_Source_Location (Ret, Sloc (N));
         return Ret;
      end;

      function Do_While_Statement (Cond : Irep) return Irep is
         Ret : constant Irep := New_Irep (I_Code_While);
      begin
         Set_Source_Location (Ret, Sloc (N));
         Set_Cond (Ret, Cond);
         Set_Body (Ret, Body_Block);
         return Ret;
      end;

   begin

      if not Present (Iter_Scheme) then
         declare
            Const_True : constant Irep := New_Irep (I_Constant_Expr);
         begin
            -- mimic C-style 8-bit bool. This might also work with 1-bit type.
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

   end;

   ---------------------------
   -- Do_Object_Declaration --
   ---------------------------

   procedure Do_Object_Declaration (N : Node_Id; Block : Irep) is
      Id   : constant Irep := Do_Defining_Identifier (Defining_Identifier(N));
      Decl : constant Irep := New_Irep (I_Code_Decl);
   begin
      Set_Source_Location (Decl, (Sloc (N)));
      Set_Symbol (Decl, Id);
      Append_Op (Block, Decl);
      if Has_Init_Expression (N) then
         declare
            Init_Expr : constant Irep := Do_Expression (Expression (N));
            Init_Statement : constant Irep := New_Irep (I_Code_Assign);
         begin
            Set_Lhs (Init_Statement, Id);
            Set_Rhs (Init_Statement, Init_Expr);
            Append_Op (Block, Init_Statement);
         end;
      end if;
   end Do_Object_Declaration;

   ------------------------
   -- Do_Type_Conversion --
   ------------------------

   function Do_Type_Conversion (N : Node_Id) return Irep is
      To_Convert : constant Irep := Do_Expression (Expression (N));
      New_Type : constant Irep := Do_Type_Reference (EType (N));
      Ret : constant Irep := New_Irep (I_Op_Typecast);
   begin
      Set_Source_Location (Ret, Sloc (N));
      if Do_Range_Check (Expression (N)) then
         Set_Range_Check (Ret, True);
      end if;
      Set_Op0 (Ret, To_Convert);
      Set_Type (Ret, New_Type);
      return Ret;
   end;

   -----------------
   -- Do_Operator --
   -----------------

   function Do_Operator (N : Node_Id) return Irep is
      LHS : constant Irep := Do_Expression (Left_Opnd (N));
      RHS : constant Irep := Do_Expression (Right_Opnd (N));

      function Op_To_Kind (N : N_Op) return Irep_Kind is
      begin
         case N is
            when N_Op_Divide =>
               return I_Op_Div;
            when N_Op_Add =>
               return I_Op_Add;
            when N_Op_Subtract =>
               return I_Op_Sub;
            when N_Op_Multiply =>
               return I_Op_Mul;
            when N_Op_Rem =>
               return I_Op_Rem;
            when N_Op_Mod =>
               return I_Op_Mod;
            when N_Op_And =>
               return I_Op_And;
            when N_Op_Or =>
               return I_Op_Or;
            when N_Op_Eq =>
               return I_Op_Eq;
            when N_Op_Ne =>
               return I_Op_Notequal;
            when N_Op_Ge =>
               return I_Op_Geq;
            when N_Op_Gt =>
               return I_Op_Gt;
            when N_Op_Le =>
               return I_Op_Leq;
            when N_Op_Lt =>
               return I_Op_Lt;
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
               raise Program_Error;
         end case;
      end;

      Op_Kind : constant Irep_Kind := Op_To_Kind (N_Op (Nkind (N)));
      Ret : constant Irep := New_Irep (Op_Kind);
   begin
      Set_Source_Location (Ret, Sloc (N));
      Set_Lhs (Ret, LHS);
      Set_Rhs (Ret, RHS);
      Set_Type (Ret, Do_Type_Reference (EType (N)));
      if Do_Overflow_Check (N) then
         Set_Overflow_Check (Ret, True);
      end if;
      if Nkind (N) in N_Op_Divide | N_Op_Mod | N_Op_Rem and then Do_Division_Check (N) then
         Set_Div_By_Zero_Check (Ret, True);
      end if;
      return Ret;
   end Do_Operator;

   function Do_Constant (N : Node_Id) return Irep is
      Ret : constant Irep := New_Irep (I_Constant_Expr);
      Constant_Type : constant Irep := Do_Type_Reference (EType (N));
   begin
      Set_Source_Location (Ret, Sloc (N));
      Set_Type (Ret, Constant_Type);
      -- FIXME
      Set_Value (Ret, Convert_Uint_To_Binary (Intval (N), 32));
      return Ret;
   end;

   ---------------------------
   -- Do_Selected_Component --
   ---------------------------

   function Do_Selected_Component (N : Node_Id) return Irep is
      Root : constant Irep := Do_Expression (Prefix (N));
      Component_Type : constant Irep := Do_Type_Reference (EType (Selector_Name (N)));
      Ret : constant Irep := New_Irep (I_Member_Expr);
   begin
      Set_Source_Location (Ret, Sloc (N));
      Set_Compound (Ret, Root);
      Set_Component_Name (Ret, Unique_Name (Selector_Name (N)));
      Set_Type (Ret, Component_Type);
      return Ret;
   end;

   ----------------------------
   -- Do_Subprogram_Or_Block --
   ----------------------------

   function Do_Subprogram_Or_Block (N : Node_Id) return Irep is
      Decls : constant List_Id := Declarations (N);
      HSS   : constant Node_Id := Handled_Statement_Sequence (N);
      Decls_Rep : Irep;
   begin
      Decls_Rep := (if Present (Decls)
                    then Process_Statement_List (Decls)
                    else New_Irep (I_Code_Block));

      Set_Source_Location (Decls_Rep, Sloc (N));
      if Present (HSS) then
         Process_Statement (HSS, Decls_Rep);
      end if;

      return Decls_Rep;
   end Do_Subprogram_Or_Block;

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
            -- Ignore for now, as I guess an implicit label can't be referenced
            null;

         when N_Loop_Statement =>
            Append_Op (Block, Do_Loop_Statement (N));

         when N_Full_Type_Declaration =>
            Do_Full_Type_Declaration (N);

         when N_Subtype_Declaration =>
            Do_Subtype_Declaration (N);

         when N_Freeze_Entity =>
            -- Ignore, nothing to generate
            null;

         when N_Subprogram_Declaration =>
            Do_Subprogram_Declaration (N);

         when N_Subprogram_Body =>
            Do_Subprogram_Body (N);

         when others =>
            pp (Union_Id (N));
            --  ??? To be added later
            raise Program_Error;

      end case;
   end Process_Statement;

   ----------------------------
   -- Process_Statement_List --
   ----------------------------

   function Process_Statement_List (L : List_Id) return Irep is
      Reps : constant Irep := New_Irep (I_Code_Block);
      Stmt : Node_Id := First (L);

   begin
      while Present (Stmt) loop
         Process_Statement (Stmt, Reps);
         Next (Stmt);
      end loop;

      return Reps;
   end Process_Statement_List;

end Tree_Walk;
