with Nlists; use Nlists;
with Stand;  use Stand;
with Treepr; use Treepr;

package body Tree_Walk is

   function Ignore (N : Node_Id) return IRep is (Dummy);
   --  Return a Dummy IR; intended as a boilerplate to consume N

   function Combine (IR1, IR2 : IRep) return IRep;
   --  Return a Dummy IR; intended as a boilerplate to consume IR1 and IR2

   -------------
   -- Combine --
   -------------

   function Combine (IR1, IR2 : IRep) return IRep is
      pragma Unreferenced (IR1);
      pragma Unreferenced (IR2);
   begin
      return Dummy;
   end Combine;

   function Do_Assignment_Statement (N  : Node_Id) return IRep
   with Pre => Nkind (N) = N_Assignment_Statement;

   function Do_Expression (N : Node_Id) return IRep
   with Pre => Nkind (N) in N_Subexpr;

   function Do_Handled_Sequence_Of_Statements (N : Node_Id) return IRep
   with Pre => Nkind (N) = N_Handled_Sequence_Of_Statements;

   function Do_Identifier (N : Node_Id) return IRep
   with Pre => Nkind (N) = N_Identifier;

   function Do_Operator (N : Node_Id) return IRep
   with Pre => Nkind (N) in N_Op;

   function Do_Subprogram_Or_Block (N : Node_Id) return IRep
   with Pre => Nkind (N) in N_Subprogram_Body |
                            N_Task_Body       |
                            N_Block_Statement |
                            N_Package_Body    |
                            N_Entry_Body;

   function Process_Statement (N : Node_Id) return IRep;
   --  Process statement or declaration

   function Process_Statement_List (L : List_Id) return IRep;
   --  Process list of statements or declarations

   -----------------------------
   -- Do_Assignment_Statement --
   -----------------------------

   function Do_Assignment_Statement (N : Node_Id) return IRep is
      LHS : constant IRep := Do_Identifier (Name (N));
      RHS : constant IRep := Do_Expression (Expression (N));

   begin
      return Combine (LHS, RHS);
   end Do_Assignment_Statement;

   -------------------------
   -- Do_Compilation_Unit --
   -------------------------

   function Do_Compilation_Unit (N : Node_Id) return IRep is
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

   -------------------
   -- Do_Expression --
   -------------------

   function Do_Expression (N : Node_Id) return IRep is
   begin
      case Nkind (N) is
         when N_Identifier =>
            return Do_Identifier (N);

         when N_Op =>
            return Do_Operator (N);

         when others =>
            raise Program_Error;
      end case;
   end Do_Expression;

   ---------------------------------------
   -- Do_Handled_Sequence_Of_Statements --
   ---------------------------------------

   function Do_Handled_Sequence_Of_Statements (N : Node_Id) return IRep is
      Stmts : constant List_Id := Statements (N);
   begin
      return Process_Statement_List (Stmts);
   end Do_Handled_Sequence_Of_Statements;

   -------------------
   -- Do_Identifier --
   -------------------

   function Do_Identifier (N : Node_Id) return IRep is
      E : constant Entity_Id := Entity (N);

   begin
      pragma Assert (Etype (E) = Standard_Integer);
      return Ignore (E);
   end Do_Identifier;

   -----------------
   -- Do_Operator --
   -----------------

   function Do_Operator (N : Node_Id) return IRep is
   begin
      case N_Op (Nkind (N)) is
         when N_Op_Divide =>
            return Ignore (N);

         when N_Op_Add =>
            return Ignore (N);

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
   end Do_Operator;

   ----------------------------
   -- Do_Subprogram_Or_Block --
   ----------------------------

   function Do_Subprogram_Or_Block (N : Node_Id) return IRep is
      Decls : constant List_Id := Declarations (N);
      HSS   : constant Node_Id := Handled_Statement_Sequence (N);

      Decls_Rep : IRep;
      HSS_Rep   : IRep;

   begin
      Decls_Rep := (if Present (Decls)
                    then Process_Statement_List (Decls)
                    else Dummy);

      HSS_Rep := (if Present (HSS)
                  then Process_Statement (HSS)
                  else Dummy);

      return Combine (Decls_Rep, HSS_Rep);
   end Do_Subprogram_Or_Block;

   -------------------------
   --  Process_Statement  --
   -------------------------

   function Process_Statement (N : Node_Id) return IRep is
   begin
      --  Deal with the statement
      case Nkind (N) is
         when N_Assignment_Statement =>
            return Do_Assignment_Statement (N);

         when N_Handled_Sequence_Of_Statements =>
            return Do_Handled_Sequence_Of_Statements (N);

         when others =>
            pp (Union_Id (N));
            --  ??? To be added later
            raise Program_Error;

      end case;
   end Process_Statement;

   ----------------------------
   -- Process_Statement_List --
   ----------------------------

   function Process_Statement_List (L : List_Id) return IRep is
      Reps : IRep := Dummy;
      Stmt : Node_Id := First (L);

   begin
      while Present (Stmt) loop
         Reps := Combine (Reps, Process_Statement (Stmt));
         Next (Stmt);
      end loop;

      return Reps;
   end Process_Statement_List;

end Tree_Walk;
