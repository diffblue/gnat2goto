with Nlists; use Nlists;
with Stand;  use Stand;
with Treepr; use Treepr;
with Namet;  use Namet;

with Iinfo; use Iinfo;
--with Irep_Helpers; use Irep_Helpers;
with Uint_To_Binary; use Uint_To_Binary;

package body Tree_Walk is

   function Do_Assignment_Statement (N  : Node_Id) return Irep_Code_Assign
   with Pre => Nkind (N) = N_Assignment_Statement;

   procedure Do_Object_Declaration (N : Node_Id; Block : in out Irep_Code_Block)
   with Pre => Nkind (N) = N_Object_Declaration;

   function Do_Expression (N : Node_Id) return Irep_Expr
   with Pre => Nkind (N) in N_Subexpr;

   function Do_Handled_Sequence_Of_Statements (N : Node_Id) return Irep_Code_Block
   with Pre => Nkind (N) = N_Handled_Sequence_Of_Statements;

   function Do_Defining_Identifier (N : Node_Id) return Irep_Symbol_Expr
   with Pre => Nkind (N) = N_Defining_Identifier;

   function Do_Identifier (N : Node_Id) return Irep_Symbol_Expr
   with Pre => Nkind (N) = N_Identifier;

   function Do_Operator (N : Node_Id) return Irep_Expr
     with Pre => Nkind (N) in N_Op;

   function Do_Constant (N : Node_Id) return Irep_Constant_Expr
     with Pre => Nkind (N) = N_Integer_Literal;

   function Do_Subprogram_Or_Block (N : Node_Id) return Irep_Code_Block
   with Pre => Nkind (N) in N_Subprogram_Body |
                            N_Task_Body       |
                            N_Block_Statement |
                            N_Package_Body    |
                            N_Entry_Body;

   procedure Process_Statement (N : Node_Id; Block : in out Irep_Code_Block);
   --  Process statement or declaration

   function Process_Statement_List (L : List_Id) return Irep_Code_Block;
   --  Process list of statements or declarations

   function Get_Int32_Type return Irep_Signedbv_Type is
      Ret : Irep_Signedbv_Type := Make_Irep_Signedbv_Type;
   begin
      Set_Width (Ret, 32);
      return Ret;
   end Get_Int32_Type;

   -----------------------------
   -- Do_Assignment_Statement --
   -----------------------------

   function Do_Assignment_Statement (N : Node_Id) return Irep_Code_Assign is
      LHS : constant Irep_Symbol_Expr := Do_Identifier (Name (N));
      RHS : constant Irep_Expr := Do_Expression (Expression (N));
      Ret : Irep_Code_Assign := Make_Irep_Code_Assign;
   begin
      Set_Lhs(Ret, Irep(LHS));
      Set_Rhs(Ret, Irep(RHS));
      return Ret;
   end Do_Assignment_Statement;

   -------------------------
   -- Do_Compilation_Unit --
   -------------------------

   function Do_Compilation_Unit (N : Node_Id) return Irep_Code_Block is
      U : constant Node_Id := Unit (N);
   begin
      case Nkind (U) is
         when N_Subprogram_Body =>
            return Do_Subprogram_Or_Block (U);
         when others =>
            pp (Union_Id (U));
            raise Program_Error;
      end case;
   end Do_Compilation_Unit;

   ----------------------------
   -- Do_Defining_Identifier --
   ----------------------------

   function Do_Defining_Identifier (N : Node_Id) return Irep_Symbol_Expr is
      Ret : Irep_Symbol_Expr := Make_Irep_Symbol_Expr;
   begin
      pragma Assert (Etype (N) = Standard_Integer);
      Set_Identifier (Ret, Get_Name_String (Chars (N)));
      Set_Type (Ret, Irep (Get_Int32_Type));
      return Ret;
   end Do_Defining_Identifier;

   -------------------
   -- Do_Expression --
   -------------------

   function Do_Expression (N : Node_Id) return Irep_Expr is
   begin
      case Nkind (N) is
         when N_Identifier =>
            return Irep_Expr (Do_Identifier (N));
         when N_Op =>
            return Do_Operator (N);
	 when N_Integer_Literal =>
	    return Irep_Expr (Do_Constant (N));
         when others =>
            raise Program_Error;
      end case;
   end Do_Expression;

   ---------------------------------------
   -- Do_Handled_Sequence_Of_Statements --
   ---------------------------------------

   function Do_Handled_Sequence_Of_Statements (N : Node_Id) return Irep_Code_Block is
      Stmts : constant List_Id := Statements (N);
   begin
      return Process_Statement_List (Stmts);
   end Do_Handled_Sequence_Of_Statements;

   -------------------
   -- Do_Identifier --
   -------------------

   function Do_Identifier (N : Node_Id) return Irep_Symbol_Expr is
      E : constant Entity_Id := Entity (N);
   begin
      return Do_Defining_Identifier (E);
   end Do_Identifier;

   ---------------------------
   -- Do_Object_Declaration --
   ---------------------------

   procedure Do_Object_Declaration (N : Node_Id; Block : in out Irep_Code_Block) is
      Id : constant Irep_Symbol_Expr := Do_Defining_Identifier (Defining_Identifier(N));
      Decl : Irep_Code_Decl := Make_Irep_Code_Decl;
   begin
      Set_Symbol (Decl, Irep (Id));
      Add_Op (Block, Irep (Decl));
      if Has_Init_Expression (N) then
         declare
            Init_Expr : constant Irep_Expr := Do_Expression (Expression (N));
            Init_Statement : Irep_Code_Assign := Make_Irep_Code_Assign;
         begin
            Set_Lhs (Init_Statement, Irep (Id));
            Set_Rhs (Init_Statement, Irep (Init_Expr));
            Add_Op (Block, Irep (Init_Statement));
         end;
      end if;
   end Do_Object_Declaration;

   -----------------
   -- Do_Operator --
   -----------------

   function Do_Operator (N : Node_Id) return Irep_Expr is
      LHS : constant Irep_Expr := Do_Expression (Left_Opnd (N));
      RHS : constant Irep_Expr := Do_Expression (Right_Opnd (N));
      Ret : Irep_Binary_Expr := Make_Irep_Binary_Expr;
   begin
      Set_Lhs (Ret, Irep (LHS));
      Set_Rhs (Ret, Irep (RHS));
      Set_Type (Ret, Irep (Get_Int32_Type));
      case N_Op (Nkind (N)) is
         when N_Op_Divide =>
	    Ret.Id := Make_Irep_Div.Id;
         when N_Op_Add =>
	    Ret.Id := Make_Irep_Plus.Id;
         when N_Op_Concat
            | N_Op_Expon
            | N_Op_Subtract
            | N_Op_Mod
            | N_Op_Multiply
            | N_Op_Rem
            | N_Op_And
            | N_Op_Eq
            | N_Op_Ge
            | N_Op_Gt
            | N_Op_Le
            | N_Op_Lt
            | N_Op_Ne
            | N_Op_Or
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

      return Irep_Expr (Ret);
   end Do_Operator;

   function Do_Constant (N : Node_Id) return Irep_Constant_Expr is
      Ret : Irep_Constant_Expr := Make_Irep_Constant_Expr;
   begin
      pragma Assert (Etype (N) = Standard_Integer);
      Set_Type (Ret, Irep(Get_Int32_Type));
      Set_Value (Ret, Convert_Uint_To_Binary (Intval (N), 32));
      return Ret;
   end;

   ----------------------------
   -- Do_Subprogram_Or_Block --
   ----------------------------

   function Do_Subprogram_Or_Block (N : Node_Id) return Irep_Code_Block is
      Decls : constant List_Id := Declarations (N);
      HSS   : constant Node_Id := Handled_Statement_Sequence (N);

      Decls_Rep : Irep_Code_Block;

   begin
      Decls_Rep := (if Present (Decls)
                    then Process_Statement_List (Decls)
                    else Make_Irep_Code_Block);

      if Present (HSS) then
         Process_Statement (HSS, Decls_Rep);
      end if;

      return Decls_Rep;
   end Do_Subprogram_Or_Block;

   -------------------------
   --  Process_Statement  --
   -------------------------

   procedure Process_Statement (N : Node_Id; Block : in out Irep_Code_Block) is
   begin
      --  Deal with the statement
      case Nkind (N) is
         when N_Assignment_Statement =>
            Add_Op (Block, Irep (Do_Assignment_Statement (N)));

         when N_Object_Declaration =>
            Do_Object_Declaration (N, Block);

         when N_Handled_Sequence_Of_Statements =>
            Add_Op (Block, Irep (Do_Handled_Sequence_Of_Statements (N)));

         when others =>
            pp (Union_Id (N));
            --  ??? To be added later
            raise Program_Error;

      end case;
   end Process_Statement;

   ----------------------------
   -- Process_Statement_List --
   ----------------------------

   function Process_Statement_List (L : List_Id) return Irep_Code_Block is
      Reps : Irep_Code_Block := Make_Irep_Code_Block;
      Stmt : Node_Id := First (L);

   begin
      while Present (Stmt) loop
         Process_Statement (Stmt, Reps);
         Next (Stmt);
      end loop;

      return Reps;
   end Process_Statement_List;

end Tree_Walk;
