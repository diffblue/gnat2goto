with Namet;                 use Namet;
with Nlists;                use Nlists;
with Sem;
with Sem_Eval;              use Sem_Eval;
with Sem_Util;              use Sem_Util;
with Sem_Aux;               use Sem_Aux;
with Snames;                use Snames;
with Stringt;               use Stringt;
with Treepr;                use Treepr;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Follow;                use Follow;
with GNAT_Utils;            use GNAT_Utils;
with GOTO_Utils;            use GOTO_Utils;
with Uint_To_Binary;        use Uint_To_Binary;
with Stand;
with Binary_To_Hex;         use Binary_To_Hex;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Exceptions;

with GNAT2GOTO.Options;

with Range_Check; use Range_Check;
with Arrays; use Arrays;
with Gnat2goto_Itypes; use Gnat2goto_Itypes;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Sinput;
with ASVAT.Address_Model;    use type
  ASVAT.Address_Model.Address_To_Access_Functions;
package body Tree_Walk is

   procedure Add_Entity_Substitution (E : Entity_Id; Subst : Irep);

   function Do_Aggregate_Literal (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Aggregate;

   function Do_Aggregate_Literal_Record (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Aggregate,
        Post => Kind (Do_Aggregate_Literal_Record'Result) = I_Struct_Expr;

   function Do_Assignment_Statement (N  : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Assignment_Statement,
        Post => Kind (Do_Assignment_Statement'Result) = I_Code_Assign;

   function Do_Call_Parameters (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Procedure_Call_Statement | N_Function_Call,
        Post => Kind (Do_Call_Parameters'Result) = I_Argument_List;

   function Do_If_Expression (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_If_Expression;

   function Do_Case_Expression (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_Case_Expression,
        Post => Kind (Do_Case_Expression'Result) = I_Let_Expr;

   function Do_Qualified_Expression (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_Qualified_Expression;

   function Do_Constant (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_Integer_Literal,
        Post => Kind (Do_Constant'Result) in
           I_Constant_Expr | I_Op_Typecast;

   function Do_Character_Constant (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_Character_Literal,
        Post => Kind (Do_Character_Constant'Result) = I_Constant_Expr;

   function Do_String_Constant (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_String_Literal,
        Post => Kind (Do_String_Constant'Result) = I_String_Constant_Expr;

   function Do_Real_Constant (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_Real_Literal,
        Post => Kind (Do_Real_Constant'Result) = I_Constant_Expr;

   function Do_Modular_Type_Definition (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_Modular_Type_Definition,
        Post => Kind (Do_Modular_Type_Definition'Result)
                in Class_Type;

   function Do_Null_Expression (N : Node_Id) return Irep
     with Pre => Nkind (N) = N_Null;

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

   procedure Do_Full_Type_Declaration (N : Node_Id)
   with Pre => Nkind (N) = N_Full_Type_Declaration;

   function Do_Function_Call (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Function_Call,
        Post => Kind (Do_Function_Call'Result) in Class_Expr;

   function Make_Assume_Expr (N : Node_Id; Assumption : Irep) return Irep;

   function Do_Nondet_Function_Call (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Function_Call,
        Post => Kind (Do_Nondet_Function_Call'Result) in Class_Expr;

   function Do_Handled_Sequence_Of_Statements (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Handled_Sequence_Of_Statements,
        Post => Kind (Do_Handled_Sequence_Of_Statements'Result) = I_Code_Block;

   function Do_If_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_If_Statement,
        Post => Kind (Do_If_Statement'Result) = I_Code_Ifthenelse;

   procedure Do_Incomplete_Type_Declaration (N : Node_Id)
   with Pre => Nkind (N) = N_Incomplete_Type_Declaration;

   function Do_Exit_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Exit_Statement,
        Post => Kind (Do_Exit_Statement'Result) in Class_Code;

   function Do_Index_Or_Discriminant_Constraint
     (N : Node_Id; Underlying : Irep) return Irep
   with Pre  => Nkind (N) = N_Index_Or_Discriminant_Constraint;

   function Do_Loop_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Loop_Statement,
        Post => Kind (Do_Loop_Statement'Result) in Class_Code;

   function Do_Case_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Case_Statement,
        Post => Kind (Do_Case_Statement'Result) = I_Code_Block;

   function Do_N_Block_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Block_Statement,
        Post => Kind (Do_N_Block_Statement'Result) = I_Code_Block;

   procedure Do_Object_Declaration (N : Node_Id; Block : Irep)
   with Pre => Nkind (N) = N_Object_Declaration
                 and then Kind (Block) = I_Code_Block;

   procedure Do_Object_Declaration_Full (N : Node_Id; Block : Irep)
   with Pre => Nkind (N) = N_Object_Declaration
                 and then Kind (Block) = I_Code_Block;

   procedure Do_Pragma (N : Node_Id; Block : Irep)
   with Pre => Nkind (N) = N_Pragma
     and then Kind (Block) = I_Code_Block; -- FIXME: what about decls?

   function Do_Operator_Simple (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Op,
        Post => Kind (Do_Operator_Simple'Result) in Class_Expr;

   function Do_Operator_Mod
     (LHS : Irep;
      Op_Kind : Irep_Kind;
      RHS : Irep; Ret_Type : Irep)
     return Irep
     with Pre => Op_Kind in I_Op_Add | I_Op_Mul,
     Post => (Kind (Do_Operator_Mod'Result) in Class_Expr
                and then Get_Type (Do_Operator_Mod'Result) = Ret_Type);

   function Do_Operator_Mod (N : Node_Id) return Irep
     with Pre => N_Op (Nkind (N)) in N_Op_Add | N_Op_Multiply;

   function Do_Operator_Sub_Mod
     (LHS : Irep;
      RHS : Irep; Ret_Type : Irep)
     return Irep
     with Post => (Kind (Do_Operator_Sub_Mod'Result) in Class_Expr and then
                     Get_Type (Do_Operator_Sub_Mod'Result) = Ret_Type);

   function Do_Operator_Sub_Mod (N : Node_Id)
                                 return Irep
     with Pre => N_Op (Nkind (N)) = N_Op_Subtract;

   function Do_Operator_General (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Op;

   function Do_Op_Not (N : Node_Id) return Irep
   with Pre => Nkind (N) in N_Op;

   function Do_Op_Mod_Not (N : Node_Id; Ret_Type : Irep) return Irep
     with Pre => (Nkind (N) = N_Op_Not and then Kind (Ret_Type) in Class_Type),
     Post => Kind (Do_Op_Mod_Not'Result) in Class_Expr;

   function Do_Unsigned_Op_Not (N : Node_Id) return Irep
     with Pre => (Nkind (N) = N_Op_Not);

   function Do_Op_Minus (N : Node_Id) return Irep
   with Pre => Nkind (N) in N_Op;

   function Do_Op_Expon (N : Node_Id) return Irep
      with Pre => Nkind (N) = N_Op_Expon;

   function Do_Or_Else (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_Or_Else;

   function Do_And_Then (N : Node_Id) return Irep
     with Pre => (Nkind (N) = N_And_Then);

   type Bit_Operand_Constructor is
     access function (Lhs : Irep;
                      Rhs : Irep;
                      Source_Location : Irep;
                      Overflow_Check : Boolean;
                      I_Type : Irep;
                      Range_Check : Boolean)
                     return Irep;

   function Do_Bit_Op (N : Node_Id;
                       Operator : Bit_Operand_Constructor)
                      return Irep
   with Pre => Nkind (N) in N_Op;

   procedure Do_Package_Declaration (N : Node_Id)
   with Pre => Nkind (N) = N_Package_Declaration;

   procedure Do_Package_Specification (N : Node_Id)
   with Pre => Nkind (N) = N_Package_Specification;

   procedure Do_Exception_Declaration (N : Node_Id)
   with Pre => Nkind (N) = N_Exception_Declaration;

   procedure Do_Private_Type_Declaration (N : Node_Id)
   with Pre => Nkind (N) = N_Private_Type_Declaration;

   function Do_Procedure_Call_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Procedure_Call_Statement;

   function Do_Range_In_Case (N : Node_Id; Symbol : Irep) return Irep;

   function Do_Record_Definition (N : Node_Id; Discs : List_Id) return Irep
   with Pre  => Nkind (N) in N_Record_Definition | N_Variant,
        Post => Kind (Do_Record_Definition'Result) = I_Struct_Type;

   function Do_Signed_Integer_Definition (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Signed_Integer_Type_Definition,
        Post => Kind (Do_Signed_Integer_Definition'Result) =
                  I_Bounded_Signedbv_Type;

   function Do_Floating_Point_Definition (N : Node_Id) return Irep
     with Pre  => (Nkind (N) = N_Floating_Point_Definition
                   and then Is_Type (Defining_Entity (Parent (N)))),
     Post => Kind (Do_Floating_Point_Definition'Result) in
     I_Floatbv_Type | I_Bounded_Floatbv_Type;

   function Do_Simple_Return_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Simple_Return_Statement,
        Post => Kind (Do_Simple_Return_Statement'Result) = I_Code_Return;

   function Do_Raise_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Raise_Statement;

   procedure Do_Subprogram_Declaration (N : Node_Id)
   with Pre => Nkind (N) = N_Subprogram_Declaration;

   procedure Do_Subprogram_Body (N : Node_Id)
   with Pre => Nkind (N) = N_Subprogram_Body;

   procedure Do_Subprogram_Body_Stub (N : Node_Id)
   with Pre => Nkind (N) in N_Subprogram_Body_Stub;

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

   function Do_Type_Conversion (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Type_Conversion,
        Post => Kind (Do_Type_Conversion'Result) in Class_Expr;

   function Do_Type_Definition (N : Node_Id; Discs : List_Id) return Irep;

   procedure Do_Withed_Unit_Spec (N : Node_Id);
   --  Enters the specification of the withed unit, N, into the symbol table

   procedure Do_Withed_Units_Specs is new Sem.Walk_Library_Items
     (Action => Do_Withed_Unit_Spec);
   --  Traverses tree applying the procedure Do_With_Unit_Spec to all nodes
   --  which are specifications of library units withed by the GNAT_Root unit
   --  (that is, the body being compiled).
   --  It starts with the unit Standard and finishes with GNAT_Root

   function Find_Record_Variant (Variant_Part : Node_Id;
                                 Actual_Disc : Node_Id) return Node_Id
   with Pre  => Nkind (Variant_Part) = N_Variant_Part,
        Post => Nkind (Find_Record_Variant'Result) = N_Variant;

   function Get_Fresh_Type_Name (Actual_Type : Irep; Associated_Node : Node_Id)
                                return Irep;

   function Get_Variant_Union_Member_Name (N : Node_Id) return String;

   function Make_Increment
     (Sym : Irep; Sym_Type : Node_Id; Amount : Integer) return Irep;

   function Make_Integer_Constant (Val : Integer; Ty : Node_Id) return Irep;

   function Make_Runtime_Check (Condition : Irep) return Irep
   with Pre  => Kind (Get_Type (Condition)) = I_Bool_Type,
        Post => Kind (Make_Runtime_Check'Result) =
                I_Side_Effect_Expr_Function_Call;

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

   procedure Process_Declaration (N : Node_Id; Block : Irep);
--     with Pre => Nkind (N) in N_Declaration or else
--                 Nkind (N) in N_Number_Declaration or else
--                 Nkind (N) in N_Later_Decl_Item or else
--                 Nkind (N) in N_Pragma or else
--                 Nkind (N) in N_Exception_Declaration or else
--                 Nkind (N) in N_Freeze_Entity;
--  Precondition commented out because full extend of declrations not yet known
   --  Handles both a basic declaration and a declarative item.

   procedure Process_Pragma_Declaration (N : Node_Id)
     with Pre => Nkind (N) in N_Pragma;

   procedure Process_Declarations (L : List_Id; Block : Irep);
   --  Processes the declarations and is used for both a package specification
   --  where only basic declarations are allowed (no subprogram bodies etc.)
   --  and declarative parts where subprogram bodies etc. may be declared.
   --  The Gnat front end will check that only allowed declarations are used
   --  where only basic declarations permitted.
   --  Process_Declarations is a procedure rather than a function like its
   --  sister Process_Statements because the Irep (an I_Code_Block) has to be
   --  extended in package_specifications when it has private declarations.

   procedure Process_Statement (N : Node_Id; Block : Irep)
   with Pre => Kind (Block) = I_Code_Block;
   --  Process statement

   function Process_Statements (L : List_Id) return Irep
   with Post => Kind (Process_Statements'Result) = I_Code_Block;
   --  Process list of statements

   procedure Register_Subprogram_Specification (N : Node_Id)
   with Pre => Nkind (N) in N_Subprogram_Specification;
   --  Insert the subprogram specification into the symbol table

   procedure Register_Type_Declaration (N : Node_Id; E : Entity_Id)
   with Pre => Nkind (N) = N_Full_Type_Declaration;
   --  Common procedure for registering non-anonymous type declarations.
   --  Called by Do_Incomplete_Type_Declaration and Do_Private_Type_Declaration

   procedure Remove_Entity_Substitution (E : Entity_Id);

   function Do_Attribute_Pos_Val (N : Node_Id) return Irep
     with Pre => (Ekind (Etype (N)) in Discrete_Kind
                 and then Nkind (N) = N_Attribute_Reference
                  and then Get_Attribute_Id (Attribute_Name (N)) in
                    Attribute_Val | Attribute_Pos
                 and then List_Length (Expressions (N)) = 1),
     Post => Kind (Do_Attribute_Pos_Val'Result) in Class_Expr;

   function Do_Attribute_Pred_Discrete (N : Node_Id) return Irep
     with Pre => (Ekind (Etype (N)) in Discrete_Kind
                 and then Nkind (N) = N_Attribute_Reference
                  and then Get_Attribute_Id (Attribute_Name (N)) =
                    Attribute_Pred
                 and then List_Length (Expressions (N)) = 1),
     Post => Kind (Do_Attribute_Pred_Discrete'Result) in Class_Expr;

   function Do_Attribute_Succ_Discrete (N : Node_Id) return Irep
     with Pre => (Ekind (Etype (N)) in Discrete_Kind
                 and then Nkind (N) = N_Attribute_Reference
                  and then Get_Attribute_Id (Attribute_Name (N)) =
                    Attribute_Succ
                 and then List_Length (Expressions (N)) = 1),
          Post => Kind (Do_Attribute_Succ_Discrete'Result) in Class_Expr;

   function Do_Access_Function_Definition (N : Node_Id) return Irep
     with Pre => Nkind (N) in N_Access_Function_Definition |
     N_Access_Procedure_Definition;

   function Do_Access_To_Object_Definition (N : Node_Id) return Irep
     with Pre => Nkind (N) = N_Access_To_Object_Definition;

   function Get_No_Return_Check return Irep;

   function Make_Malloc_Function_Call_Expr (Num_Elem : Irep;
                                            Element_Type_Size : Uint;
                                            Source_Loc : Irep)
                                            return Irep is
      Size : constant Irep :=
        Compute_Memory_Op_Size (Num_Elem          => Num_Elem,
                            Element_Type_Size => Element_Type_Size,
                            Source_Loc        => Source_Loc);
      Malloc_Args  : constant Irep := Make_Argument_List;
      Malloc_Name : constant String := "malloc";
      Malloc_Call : constant Irep :=
        Make_Side_Effect_Expr_Function_Call (Arguments       => Malloc_Args,
                                             I_Function      => Symbol_Expr (
                                   Global_Symbol_Table (Intern (Malloc_Name))),
                                             Source_Location => Source_Loc,
                        I_Type          => Make_Pointer_Type (Make_Void_Type));
   begin
      Append_Argument (Malloc_Args, Size);
      return Malloc_Call;
   end Make_Malloc_Function_Call_Expr;

   function Make_Memcpy_Function_Call_Expr (Destination : Irep;
                                            Source : Irep;
                                            Num_Elem : Irep;
                                            Element_Type_Size : Uint;
                                            Source_Loc : Irep)
                                            return Irep is
      Size : constant Irep :=
        Compute_Memory_Op_Size (Num_Elem          => Num_Elem,
                            Element_Type_Size => Element_Type_Size,
                            Source_Loc        => Source_Loc);
      Memcpy_Args  : constant Irep := Make_Argument_List;
      Memcpy_Name : constant String := "memcpy";
      Memcpy_Call : constant Irep :=
        Make_Side_Effect_Expr_Function_Call (Arguments       => Memcpy_Args,
                                             I_Function      => Symbol_Expr (
                                   Global_Symbol_Table (Intern (Memcpy_Name))),
                                             Source_Location => Source_Loc,
                        I_Type          => Make_Pointer_Type (Make_Void_Type));
   begin
      Append_Argument (I     => Memcpy_Args,
                       Value => Destination);
      Append_Argument (I     => Memcpy_Args,
                       Value => Source);
      Append_Argument (Memcpy_Args, Size);
      return Memcpy_Call;
   end Make_Memcpy_Function_Call_Expr;

   procedure Report_Unhandled_Node_Empty (N : Node_Id;
                                          Fun_Name : String;
                                          Message : String) is
   begin
      Put_Line ("----------At: " & Fun_Name & "----------");
      Put_Line ("----------" & Message & "----------");
      pp (Union_Id (N));
   end Report_Unhandled_Node_Empty;

   function Report_Unhandled_Node_Irep (N : Node_Id;
                                        Fun_Name : String;
                                        Message : String) return Irep is
   begin
      Report_Unhandled_Node_Empty (N, Fun_Name, Message);
      return Create_Dummy_Irep;
   end Report_Unhandled_Node_Irep;

   function Report_Unhandled_Node_Kind (N : Node_Id;
                                        Fun_Name : String;
                                        Message : String) return Irep_Kind is
   begin
      Report_Unhandled_Node_Empty (N, Fun_Name, Message);
      return I_Empty;
   end Report_Unhandled_Node_Kind;

   function Report_Unhandled_Node_Type (N : Node_Id;
                                        Fun_Name : String;
                                        Message : String) return Irep
   is begin
      Report_Unhandled_Node_Empty (N, Fun_Name, Message);
      return Make_Nil_Type;
   end Report_Unhandled_Node_Type;

   -----------------------------
   -- Add_Entity_Substitution --
   -----------------------------

   procedure Add_Entity_Substitution (E : Entity_Id; Subst : Irep) is
   begin
      Identifier_Substitution_Map.Insert (E, Subst);
   end Add_Entity_Substitution;

   -----------------------------
   -- Append_Declare_And_Init --
   -----------------------------

   procedure Append_Declare_And_Init
     (Symbol : Irep; Value : Irep; Block : Irep; Source_Loc : Irep)
   is
   begin
      Append_Op (Block, Make_Code_Decl (Symbol => Symbol,
                                        Source_Location => Source_Loc));
      Append_Op (Block, Make_Code_Assign (Lhs => Symbol,
                                          Rhs => Value,
                                          Source_Location => Source_Loc));
   end Append_Declare_And_Init;

   -------------------
   -- Do_Address_Of --
   -------------------

   function Do_Address_Of (N : Node_Id) return Irep is
   begin
      if Ekind (Entity (Prefix (N))) in E_Function | E_Procedure
      then
         declare
            Addressee_Type : constant Irep :=
              Get_Subtype (Do_Type_Reference (Etype (N)));
            Addressee_Id : constant String :=
              Unique_Name (Entity (Prefix (N)));
         begin
            return Make_Address_Of
              (Make_Symbol_Expr (Source_Location => Get_Source_Location (N),
                                 I_Type          => Addressee_Type,
                                 Range_Check     => False,
                                 Identifier      => Addressee_Id));
         end;
      end if;
      if not (Kind (Get_Type (Do_Expression (Prefix (N)))) in Class_Type) then
         return Report_Unhandled_Node_Irep (N, "Do_Address_Of",
                                            "Kind not in class type");
      end if;
      return Make_Address_Of (Do_Expression (Prefix (N)));
   end Do_Address_Of;

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
            return Report_Unhandled_Node_Irep
              (N,
               "Do_Aggregate_Literal",
               "Unhandled aggregate kind: "
                 & Entity_Kind'Image (Ekind (N_Type)));
      end case;
   end Do_Aggregate_Literal;

   ---------------------------------
   -- Do_Aggregate_Literal_Record --
   ---------------------------------

   function Do_Aggregate_Literal_Record (N : Node_Id) return Irep is
      N_Type : constant Entity_Id := Etype (N);
      N_Underlying_Type : constant Node_Id := Etype (N_Type);
   begin
      --  It appears GNAT sorts the aggregate members for us into the order
      --  discriminant (if any), common members, variant members.
      --  However, let's check.
      declare
         Components   : constant Node_Id :=
           Component_List (Type_Definition (Parent (N_Underlying_Type)));
         Variant_Node : constant Node_Id := Variant_Part (Components);

         Component_Iter : Node_Id :=
           First_Component_Or_Discriminant (N_Type);
         Actual_Iter    : Node_Id := First (Component_Associations (N));
         Struct_Expr : constant Irep := Make_Struct_Expr
           (Source_Location => Get_Source_Location (N),
            I_Type => Do_Type_Reference (N_Underlying_Type));
         Non_Discriminant_Components_Seen : Int := 0;
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
                  if Defining_Identifier (Substruct_Component_List) /=
                    Entity (First (Choices (Actual_Iter)))
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

   -----------------------------
   -- Do_Assignment_Statement --
   -----------------------------

   function Do_Assignment_Statement (N : Node_Id) return Irep
   is
   begin
      if Ekind (Etype (Name (N))) in Array_Kind then
         declare
            Lhs_Type : constant Entity_Id := Etype (Name (N));
            --  Since the type of the LHS may be implicit, e.g.
            --  A(1..3):=(1,2,3), where A has 10 elements,
            --  it may be the case that we have not seen the type before.
            --  Hence we should check and declare if unknown.
         begin
            if not Global_Symbol_Table.Contains (Intern
                                                 (Unique_Name (Lhs_Type)))
            then
               Declare_Itype (Lhs_Type);
            end if;
         end;
         return Do_Array_Assignment (N);
      end if;

      declare
         LHS : constant Irep := Do_Expression (Name (N));
         function RHS return Irep;
         function RHS return Irep is
            N_RHS : constant Node_Id := Expression (N);
            Bare_RHS : constant Irep := Do_Expression (N_RHS);
         begin
            return
              (if Do_Range_Check (N_RHS)
               then Make_Range_Assert_Expr
                 (N => N,
                  Value => Bare_RHS,
                  Bounds_Type => Get_Type (LHS))
               else Bare_RHS);
         end RHS;
      begin
         return Make_Code_Assign
           (Lhs => LHS,
            Rhs => Typecast_If_Necessary
              (Expr => RHS,
               New_Type => Get_Type (LHS),
               A_Symbol_Table => Global_Symbol_Table),
            Source_Location => Get_Source_Location (N));
      end;
   end Do_Assignment_Statement;

   ------------------------
   -- Do_Call_Parameters --
   ------------------------

   function Do_Call_Parameters (N : Node_Id) return Irep
   is
      Args : constant Irep := Make_Argument_List;

      --  A formal access parameter cannot be mode out (an Ada rule) and
      --  an actual corresponding to the formal access parameter must be an
      --  access type (again an Ada rule and checked by the front end).
      --  Therefore, the actual parameter will be a goto pointer type and
      --  does not need wrapping into a pointer.  Since the mode of a formal
      --  access parameter cannot be out or in out the Wrap_Argument function
      --  will not wrap the corresponding actual parameter.
      function Wrap_Argument (Base : Irep; Is_Out : Boolean) return Irep is
         (if Is_Out
         then Make_Address_Of (Base)
         else Base);

      procedure Handle_Parameter (Formal : Entity_Id; Actual : Node_Id);

      function Handle_Enum_Symbol_Members (Mem : Irep) return Irep;
      function Handle_Enum_Symbol_Members (Mem : Irep) return Irep is
         Followed_Type_Symbol : constant Irep :=
            Follow_Symbol_Type (Get_Type (Mem), Global_Symbol_Table);
      begin
         if Kind (Followed_Type_Symbol) = I_C_Enum_Type and
           Kind (Mem) = I_Symbol_Expr
         then
            declare
               Val : constant Irep := Global_Symbol_Table
                 (Intern
                    (Get_Identifier
                     (Mem)))
                 .Value;
            begin
               if Val = Ireps.Empty then
                  return Mem;
               end if;
               return
                 (if Kind (Val) = I_Op_Typecast
                  then Get_Op0 (Val) else Val);
            end;
         else
            return Mem;
         end if;
      end Handle_Enum_Symbol_Members;

      ----------------------
      -- Handle_Parameter --
      ----------------------

      procedure Handle_Parameter (Formal : Entity_Id; Actual : Node_Id) is
         Is_Out        : constant Boolean := Out_Present (Parent (Formal));
         Formal_Type : constant Irep :=
           Follow_Symbol_Type (Do_Type_Reference (Etype (Formal)),
                               Global_Symbol_Table);
         Actual_Irep   : Irep;
         Expression    : constant Irep := Do_Expression (Actual);
      begin
         if Is_Out and then
           not (Kind (Get_Type (Expression)) in Class_Type)
         then
            Report_Unhandled_Node_Empty (Actual, "Handle_Parameter",
                                         "Kind of actual not in class type");
            return;
         end if;
         Actual_Irep := Wrap_Argument (
          Typecast_If_Necessary (Handle_Enum_Symbol_Members (Expression),
                          Formal_Type, Global_Symbol_Table), Is_Out);
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
   -- Do_If_Expression --
   ------------------------

   function Do_If_Expression (N : Node_Id) return Irep is
      Expr : Node_Id := First (Expressions (N));
      Cond : constant Irep := Do_Expression (Expr);
      Then_Expr : Irep;
      Else_Expr : Irep;
      Expr_Type : constant Irep := Do_Type_Reference (Etype (N));
   begin
      Next (Expr);
      Then_Expr := Do_Expression (Expr);

      Next (Expr);
      Else_Expr := Do_Expression (Expr);

      return Make_If_Expr
        (Cond,
         Else_Expr,
         Then_Expr,
         Get_Source_Location (N),
         Expr_Type);
   end Do_If_Expression;

   ------------------------
   -- Do_Case_Expression --
   ------------------------

   function Do_Case_Expression (N : Node_Id) return Irep is

      --  Appease the style police
      function Make_Case_Test (Alts : List_Id) return Irep;

      Value : constant Irep := Do_Expression (Expression (N));
      Bound_Var : constant Irep :=
        Fresh_Var_Symbol_Expr (Get_Type (Value), "case_binder");

      --------------------
      -- Make_Case_Test --
      --------------------

      function Make_Case_Test (Alts : List_Id) return Irep is
         function Make_Single_Test (Alt : Node_Id) return Irep;
         function Make_Single_Test (Alt : Node_Id) return Irep is
         begin
            if Nkind (Alt) /= N_Range then
               return Make_Op_Eq (Lhs => Bound_Var,
                                  Rhs => Do_Expression (Alt),
                                  I_Type => Make_Bool_Type,
                                  Source_Location =>
                                    Get_Source_Location (Alt));
            else
               return Do_Range_In_Case (Alt, Bound_Var);
            end if;
         end Make_Single_Test;
         First_Alt_Test : constant Irep := Make_Single_Test (First (Alts));
         This_Alt : Node_Id := First (Alts);
      begin
         Next (This_Alt);
         if not Present (This_Alt) then
            return First_Alt_Test;
         end if;
         declare
            Big_Or : constant Irep := Make_Op_Or
              (Source_Location => (Get_Source_Location (N)),
               I_Type => CProver_Bool_T);
         begin
            Append_Op (Big_Or, First_Alt_Test);
            while Present (This_Alt) loop
               Append_Op (Big_Or, Make_Single_Test (This_Alt));
               Next (This_Alt);
            end loop;
            return Big_Or;
         end;
      end Make_Case_Test;

      function Make_Case_If_Expression (Alternatives : Node_Id) return Irep;
      function Make_Case_If_Expression (Alternatives : Node_Id) return Irep
      is
         This_Alt : constant Node_Id := Alternatives;
         Next_Alt : Node_Id := Alternatives;
         This_Expr : constant Irep := Do_Expression (Expression (This_Alt));
      begin
         Next (Next_Alt);
         if not Present (Next_Alt) then
            return This_Expr;
         else
            if not (Kind (Get_Type (This_Expr)) in Class_Type)
            then
               return Report_Unhandled_Node_Irep (N, "Do_Case_Expression",
               "Case kind not in class expr or alt expr not in class type");
            end if;
            return Make_If_Expr
              (Cond => Make_Case_Test (Discrete_Choices (This_Alt)),
               True_Case => This_Expr,
               False_Case => Make_Case_If_Expression (Next_Alt),
               I_Type => Get_Type (This_Expr),
               Source_Location => Get_Source_Location (This_Expr));
         end if;
      end Make_Case_If_Expression;

   begin
      return Make_Let_Binding_Expr
        (Symbol => Bound_Var,
         Value => Value,
         Where => Make_Case_If_Expression (First (Alternatives (N))),
         Source_Location => Get_Source_Location (N));
   end Do_Case_Expression;

   -------------------------
   -- Do_Compilation_Unit --
   -------------------------

   function Do_Compilation_Unit (N : Node_Id; Unit_Is_Subprogram : out Boolean)
     return Symbol
   is
      U           : constant Node_Id := Unit (N);
      Unit_Name : constant Symbol_Id :=
        Intern (Unique_Name (Unique_Defining_Entity (U)));
      Unit_Symbol : Symbol;
   begin
      --  Insert specifications of all withed units including the
      --  specification of the given compilation unit into the symbol table.
      Do_Withed_Units_Specs;

      case Nkind (U) is
         when N_Subprogram_Body =>
            --  The specification of the subprogram body has already
            --  been inserted into the symbol table by the call to
            --  Do_Withed_Unit_Specs.
            pragma Assert (Global_Symbol_Table.Contains (Unit_Name));
            Unit_Symbol := Global_Symbol_Table (Unit_Name);

            --  Now compile the body of the subprogram
            Unit_Symbol.Value := Do_Subprogram_Or_Block (U);

            --  and update the symbol table entry for this subprogram.
            Global_Symbol_Table.Replace (Unit_Name, Unit_Symbol);
            Unit_Is_Subprogram := True;

         when N_Package_Body =>
            declare
               Dummy : constant Irep := Do_Subprogram_Or_Block (U);
               pragma Unreferenced (Dummy);
            begin
            --  The specification of the package body has already
            --  been inserted into the symbol table by the call to
            --  Do_Withed_Unit_Specs.
               pragma Assert (Global_Symbol_Table.Contains (Unit_Name));
               Unit_Symbol := Global_Symbol_Table (Unit_Name);
               Unit_Is_Subprogram := False;
            end;
         when N_Subprogram_Declaration | N_Package_Declaration =>
            --  Package and subprogram declarations are processed
            --  when they appear in a with statement.
            --  It might be possible to construct a package declaration
            --  which has some features that are imported but not
            --  directly used (which requires the declaration to be withed),
            --  but I can't think of one without delving deeply, and I
            --  think such uses would be unusual (TJJ 21/11/2019)
            null;
         when N_Generic_Subprogram_Declaration
           | N_Generic_Package_Declaration =>
            --  Generic subprograms and packages
            --  don't need to specially handled
            --  because their instantiations appear as nodes
            --  in the AST
            null;
         when others =>
            Report_Unhandled_Node_Empty (N, "Do_Compilation_Unit",
                                         "Generic units are unsupported");
      end case;

      return Unit_Symbol;
   end Do_Compilation_Unit;

   -----------------
   -- Do_Constant --
   -----------------

   function Do_Constant (N : Node_Id) return Irep is
      Constant_Type : constant Irep := Do_Type_Reference (Etype (N));
      Int_Val : constant Uint := Intval (N);
      Source_Loc : constant Irep := Get_Source_Location (N);
      Is_Address : constant Boolean :=
        Unique_Name (Etype (N)) = "system__address";
      Is_Integer : constant Boolean := Etype (N) = Stand.Universal_Integer;
      Is_BV      : constant Boolean :=
        Kind (Constant_Type) in Class_Bitvector_Type;

      Const_Irep : constant Irep :=
        (if Is_Integer or Is_Address then
              Make_Constant_Expr
           (Source_Location => Source_Loc,
            I_Type          => Constant_Type,
            Range_Check     => False,
            Value           => UI_Image (Int_Val, Decimal))
         elsif Is_BV then
            Make_Constant_Expr
           (Source_Location => Source_Loc,
            I_Type          => Constant_Type,
            Range_Check     => False,
            Value           =>
              Convert_Uint_To_Hex (Int_Val, Pos (Get_Width (Constant_Type))))
         else
            Make_Constant_Expr (Source_Location => Source_Loc,
                                I_Type          => Constant_Type,
                                Range_Check     => False,
                                Value           => "0"));
   begin
      if not (Is_Integer or Is_BV or Is_Address) then
         Report_Unhandled_Node_Empty (N, "Do_Constant",
                                      "Unsupported constant type");
      end if;

      return (if Is_Address then
                 Make_Op_Typecast
                (Op0             => Const_Irep,
                 Source_Location => Get_Source_Location (N),
                 I_Type          => Make_Pointer_Type
                   (I_Subtype => Make_Unsignedbv_Type (8),
                    Width     => Pointer_Type_Width),
                 Range_Check     => False)
              else
                 Const_Irep);
   end Do_Constant;

   ---------------------------
   -- Do_Character_Constant --
   ---------------------------

   function Do_Character_Constant (N : Node_Id) return Irep is
      Resolved_Type : constant Irep := Do_Type_Reference (Etype (N));
      Character_Size : constant Int := UI_To_Int (Esize (Etype (N)));
      Value : constant String := Convert_Uint_To_Hex
        (Char_Literal_Value (N), Pos (Character_Size));
   begin
      return Make_Constant_Expr
        (I_Type => Resolved_Type,
         Source_Location => Get_Source_Location (N),
         Value => Value);
   end Do_Character_Constant;

   ------------------------
   -- Do_String_Constant --
   ------------------------

   function Do_String_Constant (N : Node_Id) return Irep is
     --  Element_Type_Ent : constant Entity_Id := Get_Array_Component_Type (N);
     --  Element_Type  : constant Irep := Do_Type_Reference (Element_Type_Ent);
      StrLen           : constant Integer :=
                                   Integer (String_Length (Strval (N)));
      --  String_Length_Expr : constant Irep := New_Irep (I_Constant_Expr);
   begin
      --  FIXME: The size of this signedbv should probably not be a hardcoded
      --         magic number...(e.g. 32 on a 32bit system) this should be set
      --         programatically some how.
      --  FIXME: Need to set a proper type, something like this:
      --  Set_Type (String_Length_Expr,
      --    Make_Signedbv_Type (Ireps.Empty, 64));
      --  Set_Value (String_Length_Expr,
      --             Convert_Uint_To_Hex (UI_From_Int (Int (String_Length
      --               (Strval (N)))), 64));

      --  FIXME: We really need some sort of array type here, such as:
      --  Make_Array_Type (
      --     I_Subtype => Element_Type,
      --     Size => String_Length_Expr));
      String_To_Name_Buffer (Strval (N));
      return Make_String_Constant_Expr
        (Source_Location => Get_Source_Location (N),
         I_Type => Make_String_Type,
         Value => Name_Buffer (1 .. StrLen));
   end Do_String_Constant;

   ----------------------
   -- Do_Real_Constant --
   ----------------------

   function Do_Real_Constant (N : Node_Id) return Irep is
      Real_Constant_Type : constant Irep := Do_Type_Reference (Etype (N));
      Bit_Width : constant Nat := UI_To_Int (Esize (Etype (N)));
      function Value return String;
      function Value return String is
      begin
         case Bit_Width is
            when 32 =>
               return Convert_Ureal_To_Hex_32bits_IEEE (Realval (N));
            when 64 =>
               return Convert_Ureal_To_Hex_64bits_IEEE (Realval (N));
            when others =>
               Report_Unhandled_Node_Empty
                 (N,
                  "Do_Real_Constant",
                  "Can only handle 32/64 bit floats at the moment, but "
                    & Nat'Image (Bit_Width) & " was requested");
               return "00000000000000000000000000000000";
         end case;
      end Value;
   begin
      return Make_Constant_Expr
        (I_Type => Real_Constant_Type,
         Value => Value,
         Source_Location => Get_Source_Location (N));
   end Do_Real_Constant;

   ----------------------------
   -- Do_Defining_Identifier --
   ----------------------------

   function Do_Defining_Identifier (E : Entity_Id) return Irep is
      Result_Type  : constant Irep := Do_Type_Reference (Etype (E));

      Is_Out_Param : constant Boolean :=
        Ekind (E) in E_In_Out_Parameter | E_Out_Parameter;

      Symbol_Type  : constant Irep :=
        (if Is_Out_Param
           then Make_Pointer_Type (Result_Type)
           else Result_Type);

      Sym          : constant Irep := Make_Symbol_Expr
           (Source_Location => Get_Source_Location (E),
            Identifier => Unique_Name (E),
            I_Type => Symbol_Type);
   begin
      if Is_Out_Param then
         return Make_Dereference_Expr
           (I_Type => Result_Type,
            Object => Sym,
            Source_Location => Get_Source_Location (E));
      else
         return Sym;
      end if;
   end Do_Defining_Identifier;

   --------------------
   -- Do_Dereference --
   --------------------

   function Do_Dereference (N : Node_Id) return Irep is
      (Make_Dereference_Expr
        (I_Type => Do_Type_Reference (Etype (N)),
         Object => Do_Expression (Prefix (N)),
         Source_Location => Get_Source_Location (N)));

   --------------------------------
   -- Do_Derived_Type_Definition --
   --------------------------------

   function Do_Derived_Type_Definition (N : Node_Id) return Irep is
      Subtype_Irep : constant Irep :=
        Do_Subtype_Indication (Subtype_Indication (N));
   begin
      if Present (Record_Extension_Part (N)) then
         return Report_Unhandled_Node_Type (N, "Do_Derived_Type_Definition",
                                            "record extension unsupported");
      end if;
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
         return Report_Unhandled_Node_Type (N, "Do_Derived_Type_Definition",
                                   "derived type definition unsupported here");
      end if;

      return Subtype_Irep;
   end Do_Derived_Type_Definition;

   -------------------------------
   -- Do_Enumeration_Definition --
   -------------------------------

   function Do_Enumeration_Definition (N : Node_Id) return Irep is
      Enum_Body : constant Irep := Make_C_Enum_Members;
      Enum_Type_Symbol : constant Irep := Make_Symbol_Type
        (Identifier => Unique_Name (Defining_Identifier (Parent (N))));
      First_Member : constant Node_Id := First (Literals (N));
      Member : Node_Id := First_Member;
      Last_Member : Node_Id := First_Member;
   begin
      loop
         declare
            Val_String : constant String :=
              UI_Image (Enumeration_Pos (Member));
            Val_Name : constant String := Unique_Name (Member);
            Base_Name : constant String := Get_Name_String (Chars (Member));
            Member_Size : constant Int := UI_To_Int (Esize (Etype (Member)));
            Element : constant Irep := Make_C_Enum_Member
              (Basename => Base_Name,
               Value => Val_String,
               Identifier => Val_Name);
            Member_Symbol_Init : constant Irep := Make_Constant_Expr
              (I_Type => Make_Signedbv_Type (Integer (Member_Size)),
               Value => Convert_Uint_To_Hex
                 (Enumeration_Pos (Member), Member_Size),
               Source_Location => Get_Source_Location (Member));
            Typecast_Expr : constant Irep := Make_Op_Typecast
              (Op0 => Member_Symbol_Init,
               I_Type => Enum_Type_Symbol,
               Source_Location => Get_Source_Location (Member));
         begin
            Append_Member (Enum_Body, Element);
            New_Enum_Member_Symbol_Entry (Member_Name    => Intern (Val_Name),
                                     Base_Name      => Intern (Base_Name),
                                     Enum_Type      => Enum_Type_Symbol,
                                     Value_Expr     => Typecast_Expr,
                                     A_Symbol_Table => Global_Symbol_Table);
         end;
         Last_Member := Member;
         Next (Member);
         exit when not Present (Member);
      end loop;

      declare
         function Make_Char_Symbol (N : Node_Id) return Irep is
           (Make_Symbol_Expr
              (Source_Location => Get_Source_Location (N),
               I_Type          => Enum_Type_Symbol,
               Range_Check     => False,
               Identifier      => Unique_Name (N)))
            with Pre => Nkind (N) = N_Defining_Character_Literal;

         Lower_Bound : constant Irep :=
           (case Nkind (First_Member) is
               when N_Defining_Identifier =>
                  Do_Defining_Identifier (First_Member),
               when N_Defining_Character_Literal =>
                  Make_Char_Symbol (First_Member),
               when others =>
                  Report_Unhandled_Node_Irep
              (N        => First_Member,
               Fun_Name => "Do_Enumeration_Definition",
               Message  => "Incorrect Enumeration Literal"));

         Upper_Bound : constant Irep :=
           (case Nkind (Last_Member) is
               when N_Defining_Identifier =>
                  Do_Defining_Identifier (Last_Member),
               when N_Defining_Character_Literal =>
                  Make_Char_Symbol (Last_Member),
               when others =>
                  Report_Unhandled_Node_Irep
              (N        => Last_Member,
               Fun_Name => "Do_Enumeration_Definition",
               Message  => "Incorrect Enumeration Literal"));
      begin
         return Make_C_Enum_Type
           (I_Subtype =>
              Make_Bounded_Signedbv_Type
                (Width       => 32, -- FIXME why 32?
                 Lower_Bound => Store_Symbol_Bound
                   (Bound_Type_Symbol (Lower_Bound)),
                 Upper_Bound => Store_Symbol_Bound
                   (Bound_Type_Symbol (Upper_Bound))),
            I_Body => Enum_Body);
      end;
   end Do_Enumeration_Definition;

   function Do_Attribute_Pos_Val (N : Node_Id) return Irep is
      --  Expressions function returns a list of arguments
      Arguments : constant List_Id := Expressions (N);
      --  We check in the precondition that there's only one
      Argument : constant Node_Id := First (Arguments);
   begin
      return Do_Expression (Argument);
   end Do_Attribute_Pos_Val;

   function Do_Attribute_Pred_Discrete (N : Node_Id) return Irep is
      Arguments : constant List_Id := Expressions (N);
      Argument : constant Node_Id := First (Arguments);
      Arg_Expr : constant Irep := Do_Expression (Argument);
      Result_Type : constant Irep := Get_Type (Arg_Expr);
      Source_Loc : constant Irep := Get_Source_Location (N);
      --  Need to increment by one discrete step
      --  Using index constant
      One : constant Irep :=
           Build_Index_Constant (Value      => 1,
                                 Source_Loc => Source_Loc);
   begin
      return Make_Op_Sub (Rhs             => One,
                          Lhs             => Arg_Expr,
                          Source_Location => Source_Loc,
                          Overflow_Check  => False,
                          I_Type          => Result_Type);
   end Do_Attribute_Pred_Discrete;

   function Do_Attribute_Succ_Discrete (N : Node_Id) return Irep is
      Arguments : constant List_Id := Expressions (N);
      Argument : constant Node_Id := First (Arguments);
      Arg_Expr : constant Irep := Do_Expression (Argument);
      Result_Type : constant Irep := Get_Type (Arg_Expr);
      Source_Loc : constant Irep := Get_Source_Location (N);
      One : constant Irep :=
           Build_Index_Constant (Value      => 1,
                                 Source_Loc => Source_Loc);
   begin
      return Make_Op_Add (Rhs             => One,
                          Lhs             => Arg_Expr,
                          Source_Location => Source_Loc,
                          Overflow_Check  => False,
                          I_Type          => Result_Type);
   end Do_Attribute_Succ_Discrete;

   -------------------
   -- Do_Expression --
   -------------------

   function Create_Dummy_Irep return Irep is (CProver_Nil);

   function Do_Expression (N : Node_Id) return Irep is
   begin
      Declare_Itype (Etype (N));
      case Nkind (N) is
         when N_Identifier |
            N_Expanded_Name          => return Do_Identifier (N);
         when N_Selected_Component   => return Do_Selected_Component (N);
         when N_Op                   => return Do_Operator_General (N);
         when N_Integer_Literal      => return Do_Constant (N);
         when N_String_Literal       => return Do_String_Constant (N);
         when N_Character_Literal    => return Do_Character_Constant (N);
         when N_Type_Conversion      => return Do_Type_Conversion (N);
         when N_Function_Call        => return Do_Function_Call (N);
         when N_Or_Else              => return Do_Or_Else (N);
         when N_Attribute_Reference  =>
            case Get_Attribute_Id (Attribute_Name (N)) is
               when Attribute_Access => return Do_Address_Of (N);
               when Attribute_Address =>
                  --  Use the ASVAT.Address_Model to create the address.
                  return ASVAT.Address_Model.Do_ASVAT_Address_Of (N);
               when Attribute_Length => return Do_Array_Length (N);
               when Attribute_Range  =>
                  return Report_Unhandled_Node_Irep (N, "Do_Expression",
                                                     "Range attribute");
               when Attribute_First  =>
                  if Nkind (Etype (Prefix (N))) = N_Defining_Identifier
                    and then
                      Get_Name_String (Chars (Etype (Etype (Prefix (N)))))
                    = "string"
                  then
                     return Report_Unhandled_Node_Irep (N, "Do_Expression",
                                                "First of string unsupported");
                  else
                     return Do_Array_First (N);
                  end if;
               when Attribute_Last   =>
                  if Nkind (Etype (Prefix (N))) = N_Defining_Identifier
                    and then
                      Get_Name_String (Chars (Etype (Etype (Prefix (N)))))
                    = "string"
                  then
                     return Report_Unhandled_Node_Irep (N, "Do_Expression",
                                                 "Last of string unsupported");
                  else
                     return Do_Array_Last (N);
                  end if;
               when Attribute_Val =>
                  return Do_Attribute_Pos_Val (N);
               when Attribute_Pos =>
                  return Do_Attribute_Pos_Val (N);
               when Attribute_Pred =>
                  return Do_Attribute_Pred_Discrete (N);
               when Attribute_Succ =>
                  return Do_Attribute_Succ_Discrete (N);
               when Attribute_Size =>
                  --  S'Size and X'Size are optimised into a simple literal
                  --  by the gnat frontend when the size of the subtype or
                  --  object is known by the frontend.
                  --  In such cases this branch will not be entered.
                  --  S'Size where S is a scalar subtype is nearly always
                  --  known at compile time, it is implementation dependent
                  --  for indefinite subtypes.
                  --  The Ada RM only specifies the value of S'Size, when
                  --  S is a packed record and S is a formal parameter of
                  --  Unchecked conversion.
                  --  The gnat frontend function RM_Size should be used to
                  --  obtain the value of S'Size.  It has the value 0 if
                  --  the size of the subtype is not known to the frontend.
                  --
                  --  To obtain X'Size the frontend function Esize is used.
                  --  The function can be applied to the object or its subtype.
                  --  It seems that when applied to the object it returns 0 if
                  --  the object does not have an attribute definition clause
                  --  specifying its size.
                  --  In such cases gnat2goto uses the default size of the
                  --  object obtained by applying Esize to its subtype.
                  --  Unfortunately, this may also return 0 if the size of
                  --  the subtype is not known by the frontend.
                  declare
                     Constant_Type : constant Irep :=
                       Do_Type_Reference (Stand.Universal_Integer);
                     The_Entity : constant Node_Id := Entity (Prefix (N));
                     The_Size  : Uint;
                  begin
                     if Is_Object (The_Entity) then
                        declare
                           Object_Size : constant Uint := Esize (The_Entity);
                           Default_Obj_Size : constant Uint :=
                             Esize (Etype (The_Entity));
                           The_Size_To_Use : constant Uint :=
                             (if Integer (UI_To_Int (Object_Size)) /= 0 then
                                 Object_Size
                              else
                                 Default_Obj_Size
                             );
                        begin
                           The_Size := The_Size_To_Use;
                        end;
                     elsif Is_Type (The_Entity) then
                        --  Since the attribute is applied to a subtype,
                        --  S'Size, RM_Size should be used.
                        The_Size := RM_Size (The_Entity);

                        if not Is_Definite_Subtype (The_Entity) then
                           Report_Unhandled_Node_Empty
                             (The_Entity,
                              "Do_Expression",
                              "Size attribute applied to indefinite type " &
                                "is implementation defined");
                        end if;
                     end if;

                     return Make_Constant_Expr
                       (Value =>
                          UI_Image (Input  => The_Size, Format => Decimal),
                        I_Type => Constant_Type,
                        Source_Location => Get_Source_Location (N));
                  end;
               when others           =>
                  return Report_Unhandled_Node_Irep
                    (N, "Do_Expression",
                     Attribute_Id'Image
                       (Get_Attribute_Id (Attribute_Name (N))) &
                    " unsupported");
            end case;
         when N_Explicit_Dereference => return Do_Dereference (N);
         when N_Case_Expression      => return Do_Case_Expression (N);
         when N_Aggregate            => return Do_Aggregate_Literal (N);
         when N_Indexed_Component    => return Do_Indexed_Component (N);
         when N_Slice                => return Do_Slice (N);
         when N_In                   =>  return Do_In (N);
         when N_Real_Literal => return Do_Real_Constant (N);
         when N_If_Expression => return Do_If_Expression (N);
         when N_And_Then => return Do_And_Then (N);
         when N_Qualified_Expression => return Do_Qualified_Expression (N);
         when N_Quantified_Expression =>
            return Report_Unhandled_Node_Irep (N, "Do_Expression",
                                               "Quantified");
         when N_Null =>
            return Do_Null_Expression (N);
         when others                 =>
            return Report_Unhandled_Node_Irep (N, "Do_Expression",
                                               "Unknown expression kind");
      end case;
   end Do_Expression;

   function Do_In (N : Node_Id) return Irep is
      function Get_Range (N : Node_Id) return Node_Id;
      function Get_Range (N : Node_Id) return Node_Id is
         Underlying_Type : Entity_Id := N;
      begin
         while not (Nkind (Underlying_Type) = N_Defining_Identifier) loop
            Underlying_Type := Etype (N);
         end loop;
         if No (Scalar_Range (Underlying_Type)) then
            Report_Unhandled_Node_Empty (N, "Get_Range",
                                         "Does not contain range");
            return N;
         end if;
         return Scalar_Range (Underlying_Type);
      end Get_Range;

      Left_Op : constant Irep :=
        Cast_Enum (Do_Expression (Left_Opnd (N)), Global_Symbol_Table);
      Range_Node : constant Node_Id := Get_Range (Right_Opnd (N));
      Low_Right : constant Irep :=
        Cast_Enum (Do_Expression (Low_Bound (Range_Node)),
                   Global_Symbol_Table);
      High_Right : constant Irep :=
        Cast_Enum (Do_Expression (High_Bound (Range_Node)),
                   Global_Symbol_Table);
      Geq_Low : constant Irep := Make_Op_Geq (Rhs             => Low_Right,
                                              Lhs             => Left_Op,
                                              Source_Location =>
                                                Get_Source_Location (N),
                                              I_Type => Make_Bool_Type);
      Leq_High : constant Irep := Make_Op_Leq (Rhs             => High_Right,
                                               Lhs             => Left_Op,
                                               Source_Location =>
                                                 Get_Source_Location (N),
                                               I_Type => Make_Bool_Type);
      Range_Irep : constant Irep := Make_Op_And
        (Source_Location => Get_Source_Location (N),
         I_Type => CProver_Bool_T);
   begin
      Append_Op (Range_Irep, Geq_Low);
      Append_Op (Range_Irep, Leq_High);
      return Range_Irep;
   end Do_In;

   -------------------
   --  Do_And_Then  --
   -------------------

   function Do_And_Then (N : Node_Id) return Irep is
      L : constant Node_Id := Left_Opnd (N);
      R : constant Node_Id := Right_Opnd (N);
      Expr : constant Irep := Make_Op_And
        (I_Type => CProver_Bool_T,
         Source_Location => Get_Source_Location (N));
   begin
      Append_Op (Expr, Do_Expression (L));
      Append_Op (Expr, Do_Expression (R));
      return Expr;
   end Do_And_Then;

   ------------------
   --  Do_Or_Else  --
   ------------------

   function Do_Or_Else (N : Node_Id) return Irep is
      L : constant Node_Id := Left_Opnd (N);
      R : constant Node_Id := Right_Opnd (N);
      Expr : constant Irep := Make_Op_Or
        (I_Type => CProver_Bool_T,
         Source_Location => Get_Source_Location (N));
   begin
      Append_Op (Expr, Do_Expression (L));
      Append_Op (Expr, Do_Expression (R));
      return Expr;
   end Do_Or_Else;

   ------------------------------
   -- Do_Full_Type_Declaration --
   ------------------------------

   procedure Do_Full_Type_Declaration (N : Node_Id) is
      New_Type : constant Irep :=
        Do_Type_Definition (Type_Definition (N),
                            Discriminant_Specifications (N));
      E        : constant Entity_Id := Defining_Identifier (N);
   begin
      if not (Kind (New_Type) in Class_Type)
      then
         Report_Unhandled_Node_Empty (N, "Do_Full_Type_Declaration",
                      "Identifier not in class type. Type definition failed.");
         return;
      end if;
      if not Is_Type (E)
      then
         Report_Unhandled_Node_Empty (N, "Do_Full_Type_Declaration",
                                      "identifier not a type");
         return;
      end if;

      --  If this is the full_type_declaration of a previous
      --  private_type_declaration or incomplete_type_declaration then
      --  either it will have a previous private declaration or an
      --  Incomplete_View of the declaration will be present and the
      --  full_type_declaration will have been registered
      --  (via `Register_Type_Declaration') when its
      --  private or incomplete_type_declaration was processed.
      --  If it has no private declaration or the Incomplete_View is not
      --  present or it is a derived_type_definition
      --  then the full_type_declaration has to be registered
      if not (Has_Private_Declaration (E)
              or else Present (Incomplete_View (N)))
        or else Nkind (Type_Definition (N)) = N_Derived_Type_Definition
      then
         Do_Type_Declaration (New_Type, E);

         --  Declare the implicit initial subtype too
         if Etype (E) /= E then
            Do_Type_Declaration (New_Type, Etype (E));
         end if;
      end if;

      if not (Global_Symbol_Table.Contains (Intern (Unique_Name (E))))
      then
         Report_Unhandled_Node_Empty (N, "Do_Full_Type_Declaration",
                                 "type not in symbol table after declaration");
      end if;
   end Do_Full_Type_Declaration;

   ----------------------
   -- Make_Assume_Expr --
   ----------------------

   function Make_Assume_Expr (N : Node_Id; Assumption : Irep) return Irep is
      Source_Location : constant Irep := Get_Source_Location (N);
      Assume_Params : constant Irep := Make_Parameter_List;
      Assume_Type : constant Irep := Make_Code_Type
        (Parameters => Assume_Params,
         Ellipsis => False,
         Return_Type => CProver_Void_T,
         Inlined => False,
         Knr => False);
      Assume_Function_Name : constant String := "__CPROVER_assume";
      Sym_Assume : constant Irep := Make_Symbol_Expr (
                                     Source_Location => Source_Location,
                                     I_Type          => Assume_Type,
                                     Range_Check     => False,
                                     Identifier      => Assume_Function_Name);
      Assume_Args  : constant Irep := Make_Argument_List;
      SEE_Fun_Call : constant Irep := Make_Side_Effect_Expr_Function_Call
        (Source_Location => Source_Location,
         I_Function => Sym_Assume,
         Arguments => Assume_Args,
         I_Type => CProver_Void_T);
   begin
      Append_Parameter
        (Assume_Params,
         Make_Code_Parameter
           (Source_Location => Source_Location,
            Default_Value => Ireps.Empty,
            --  arbitrary, should be replaced with a CProver_Int_T
            I_Type => Make_Signedbv_Type (32),
            Base_Name => Assume_Function_Name,
            This => False,
            Identifier => Assume_Function_Name & "::assumption"));
      Append_Argument (Assume_Args, Assumption);
      return SEE_Fun_Call;
   end Make_Assume_Expr;

   -----------------------------
   -- Do_Qualified_Expression --
   -----------------------------

   function Do_Qualified_Expression (N : Node_Id) return Irep is
      Value : constant Irep := Do_Expression (Expression (N));
      Type_Of_Val : constant Irep :=
         Follow_Symbol_Type (
          Do_Type_Reference (Etype (N)),
          Global_Symbol_Table);
      Typecast_Expr : constant Irep :=
        Typecast_If_Necessary (Expr           => Value,
                               New_Type       => Type_Of_Val,
                               A_Symbol_Table => Global_Symbol_Table);
   begin
      --  TODO: Range expressions for non-bounded types are outside
      --        the scope of this PR
      case Kind (Type_Of_Val) is
         when I_Bounded_Signedbv_Type
            | I_Bounded_Floatbv_Type
            | I_Bounded_Unsignedbv_Type
            | I_Unsignedbv_Type
            | I_Signedbv_Type
            | I_Bounded_Mod_Type =>
            return Make_Range_Assert_Expr (N, Typecast_Expr, Type_Of_Val);
         when others =>
            return Typecast_Expr;
      end case;
   end Do_Qualified_Expression;

   -----------------------------
   -- Do_Nondet_Function_Call --
   -----------------------------

   function Do_Nondet_Function_Call (N : Node_Id) return Irep is
      Func_Str     : constant String := Unique_Name (Entity (Name (N)));
      Func_Name    : constant Symbol_Id := Intern (Func_Str);
      Source_Location   : constant Irep := Get_Source_Location (N);
   begin
      if Global_Symbol_Table.Contains (Func_Name) then
         --  ??? why not get this from the entity

         --  Two implementation options here:
         --  1. Trickery: Use LET expression to create a tmp variable and
         --     assign nondet with ranges (side effect with function call
         --     to assume). However, cannot use I_Code_Assume directly,
         --     because cbmc considers this a statement.
         --  2. Future: Just place I_Nondet_Expr, set type, let cbmc
         --     place the assume on ranges.
         --  Due to lack of insight into cbmc, we implement 1.
         declare
            Func_Symbol : constant Symbol := Global_Symbol_Table (Func_Name);

            Type_Irep    : constant Irep :=
              Get_Return_Type (Func_Symbol.SymType);
            Sym_Nondet   : constant Irep :=
              Fresh_Var_Symbol_Expr (Type_Irep, Func_Str);
            Followed_Type : constant Irep :=
              Follow_Symbol_Type (Type_Irep, Global_Symbol_Table);
            Nondet_Expr  : constant Irep := Make_Side_Effect_Expr_Nondet
              (Source_Location => Source_Location,
               I_Type => Type_Irep);

            function Assume_And_Yield_Lhs return Irep;
            function Assume_And_Yield_Lhs return Irep is
            begin
               if Kind (Followed_Type) in
                 I_Bounded_Signedbv_Type | I_Bounded_Floatbv_Type
               then
                  return Make_Assume_Expr
                    (N,
                     Make_Range_Expression
                       (Sym_Nondet,
                        Get_Bound (N, Followed_Type, Bound_Low),
                        Get_Bound (N, Followed_Type, Bound_High)));
               else
                  return Make_Assume_Expr
                    (N,
                     Make_Constant_Expr
                       (Source_Location => Source_Location,
                        I_Type          => CProver_Bool_T,
                        Range_Check     => False,
                        Value           => "true"));
               end if;
            end Assume_And_Yield_Lhs;
            Assume_And_Yield : constant Irep := Make_Op_Comma
              (Lhs => Assume_And_Yield_Lhs,
               Rhs => Sym_Nondet,
               Source_Location => Source_Location);
         begin
            return Make_Let_Binding_Expr
              (Symbol          => Sym_Nondet,
               Value           => Nondet_Expr,
               Where           => Assume_And_Yield,
               Source_Location => Source_Location,
               I_Type          => Type_Irep);
         end;
      else -- How did that happen (GNAT should reject)?
         return Report_Unhandled_Node_Irep (N, "Do_Nondet_Function_Call",
                                            "func name not in symbol table");
      end if;
   end Do_Nondet_Function_Call;

   ----------------------
   -- Do_Function_Call --
   ----------------------

   function Do_Function_Call (N : Node_Id) return Irep
   is
      Func_Ent     : Entity_Id;
      Func_Name    : Symbol_Id;
      Func_Symbol  : Symbol;

   begin
      --  It seems as though an N_Explicit_Drereference is placed in the tree
      --  even when the function call is an implicit dereference.
      --  Hence, implicit dereferences do not have to be seperately handled,
      --  they are handled as explicit dereferences.
      if Nkind (Name (N)) = N_Explicit_Dereference then
         declare
            Fun_Type : constant Irep :=
              Get_Subtype (Do_Type_Reference (Etype (Prefix (Name (N)))));
            Return_Type : constant Irep := Get_Return_Type (Fun_Type);
            Deref_Function : constant Irep :=
              Make_Dereference_Expr
              (Object          => Do_Identifier (Prefix (Name (N))),
               Source_Location => Get_Source_Location (N),
               I_Type          => Fun_Type,
               Range_Check     => False);
         begin
            return Make_Side_Effect_Expr_Function_Call
              (Arguments       => Do_Call_Parameters (N),
               I_Function      => Deref_Function,
               Source_Location => Get_Source_Location (N),
               I_Type          => Return_Type,
               Range_Check     => False);
         end;
      end if;

      if not (Nkind (Name (N)) in N_Has_Entity)
        and then Nkind (Name (N)) /= N_Aspect_Specification
        and then Nkind (Name (N)) /= N_Attribute_Definition_Clause
        and then Nkind (Name (N)) /= N_Freeze_Entity
        and then Nkind (Name (N)) /= N_Freeze_Generic_Entity
      then
         return Report_Unhandled_Node_Irep (N, "Do_Function_Call",
                                            "Wrong name nkind");
      end if;
      Func_Ent := Entity (Name (N));
      Func_Name := Intern (Unique_Name (Func_Ent));

      --  TODO: in general, the Ada program must be able to
      --  use cbm's built-in functions, like "__cprover_assume".
      --  However, there are several problems:
      --   1. without no function bodies, GNAT rejects the program
      --   2. GNAT turns identifiers to lower case, thus "__CPROVER_assume"
      --      et.al. won't be recognized by cbmc anymore.
      --   3. Identifiers cannot start with underline in Ada.
      --   4. cbmc will ignore the Ada type ranges when assigning nondets,
      --      whereas we would expect that "nondet_natural return Natural"
      --      will not introduce negative numbers.

      --  For now, we only handle "nondet" prefixes here.

      if Nkind (Func_Ent) /= N_Defining_Identifier then
         return Report_Unhandled_Node_Irep (Func_Ent, "Do_Function_Call",
                                    "function entity not defining identifier");
      end if;
      if Name_Has_Prefix (N, "nondet") or else
        Has_GNAT2goto_Annotation (Func_Ent, "nondet")
      then
         return Do_Nondet_Function_Call (N);
      else
         if Global_Symbol_Table.Contains (Func_Name) then
            Func_Symbol  := Global_Symbol_Table (Func_Name);
            --  ??? why not get this from the entity

            return Make_Side_Effect_Expr_Function_Call
              (Source_Location => Get_Source_Location (N),
               I_Function => Symbol_Expr (Func_Symbol),
               Arguments => Do_Call_Parameters (N),
               I_Type => Get_Return_Type (Func_Symbol.SymType));
         else
            --  This can happen for RTS functions (body not parsed by us)
            --  TODO: handle RTS functions in a sane way
            return Report_Unhandled_Node_Irep (N, "Do_Function_Call",
                                              "func name not in symbol table");
         end if;
      end if;
   end Do_Function_Call;

   ---------------------------------------
   -- Do_Handled_Sequence_Of_Statements --
   ---------------------------------------

   function Do_Handled_Sequence_Of_Statements (N : Node_Id) return Irep is
      Stmts : constant List_Id := Statements (N);
      Stmnt : Node_Id := First (Stmts);
      Reps : constant Irep := Make_Code_Block
        (Source_Location => Get_Source_Location (N));
   begin
      while Present (Stmnt) loop
         if Nkind (Stmnt) = N_Object_Declaration then
            Process_Declaration (Stmnt, Reps);
         else
            Process_Statement (Stmnt, Reps);
         end if;
         Next (Stmnt);
      end loop;
      return Reps;
   end Do_Handled_Sequence_Of_Statements;

   -------------------
   -- Do_Identifier --
   -------------------

   function Do_Identifier (N : Node_Id) return Irep is
      E : constant Entity_Id := Entity (N);
      Subst_Cursor : constant Identifier_Maps.Cursor :=
        Identifier_Substitution_Map.Find (E);
      Dec_Node : constant Node_Id := Declaration_Node (E);
   begin
      --  An identifier which is a constant declared with a static expression
      --  can be replaced by the expression.  This provides CBMC with the value
      --  of the expression and allows better checking.
      --  This is important for constants that are declared at package level as
      --  CBMC would otherwise not have their value.
      --  Note:  This can only be done with static constants otherwise the
      --  value of the expression could be different each time a subprogram
      --  is called.
      if  Nkind (Dec_Node) = N_Object_Declaration
        and then Constant_Present (Dec_Node)
      then
         if Has_Init_Expression (Dec_Node)
           and then Is_Static_Expression (Expression (Dec_Node))
         then
            --  It is a constant with a static expression
            return Do_Expression (Expression (Dec_Node));
         elsif Present (Full_View (Defining_Identifier (Dec_Node))) then
            --  It is a deferred constant - the completion is given by
            --  the Full_View.
            declare
               FV_Dec_Node : constant Node_Id :=
                 Declaration_Node (Full_View (Defining_Identifier (Dec_Node)));
            begin

               if Has_Init_Expression (FV_Dec_Node)
                 and then Is_Static_Expression (Expression (FV_Dec_Node))
               then
                  --  It is a deferred constant with a completion initialised
                  --  by a static expression.

                  return Do_Expression (Expression (FV_Dec_Node));
               end if;
            end;
         end if;
      end if;

      --  If an identifier is the name of a number_declaration, the identifier
      --  is replaced by its static initialisation expression.
      if Nkind (Dec_Node) = N_Number_Declaration then
         return Do_Expression (Expression (Dec_Node));
      end if;

      if Identifier_Maps.Has_Element (Subst_Cursor) then
         --  Indicates instead of literally referring to the given
         --  name, we should return some replacement irep. Currently
         --  this is used for discriminants during record init.
         return Identifier_Maps.Element (Subst_Cursor);
      else
         if Nkind (E) in N_Has_Etype and then (Is_Type (Etype (E))) then
            return Do_Defining_Identifier (E);
         end if;
         return Report_Unhandled_Node_Irep
           (N,
            "Do_Identifier",
            "Etype not a type");
      end if;
   end Do_Identifier;

   -----------------------
   -- Do_Exit_Statement --
   -----------------------

   function Do_Exit_Statement (N : Node_Id) return Irep is
      Jump_Irep : Irep;
   begin
      if Present (Name (N)) then
         Jump_Irep := Make_Code_Goto
           (Source_Location => Get_Source_Location (N),
            Destination     =>
              Get_Name_String (Chars (Name (N))) & "_exit");
      else
         Jump_Irep := Make_Code_Break (Source_Location =>
                                         Get_Source_Location (N));
      end if;
      if Present (Condition (N)) then
         return Make_Code_Ifthenelse
           (Source_Location => Get_Source_Location (N),
            Cond => Do_Expression (Condition (N)),
            Then_Case => Jump_Irep,
            Else_Case => Ireps.Empty);
      else
         return Jump_Irep;
      end if;
   end Do_Exit_Statement;

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
         (Make_Code_Ifthenelse
           (Cond => Do_Expression (Condition (N)),
            Then_Case => Process_Statements (Then_Statements (N)),
            Else_Case => Ireps.Empty,
            Source_Location => Get_Source_Location (N)));

      --  Local variables

      Ret : constant Irep := Do_If_Block (N);

   --  Start of processing for Do_If_Statement

   begin
      Do_Elsifs (First (Elsif_Parts (N)), Else_Statements (N), Ret);
      return Ret;
   end Do_If_Statement;

   ------------------------------------
   -- Do_Incomplete_Type_Declaration --
   ------------------------------------

   procedure Do_Incomplete_Type_Declaration (N : Node_Id) is
      Entity : constant Entity_Id := Defining_Identifier (N);
      --  Only complete types should be inserted in the symbol table.
      --  If an incomplete type declaration is inserted it will prevent
      --  the full declaration being entered into the symbol talble.
      --
      --  The full view of an incomplete_type_declaration is obtained
      --  by calling the Full_View function.  As the compiler has completed
      --  semantic analysis before invoking the gnat to goto translation
      --  all incomplete_type_declarations should have a full view.
      Full_View_Entity : constant Entity_Id := Full_View (Entity);
   begin
      pragma Assert (Is_Incomplete_Type (Entity));
      --   If the incomplete_type_declaration is completed by a
      --   private_type_declaration, the private_type_declaration
      --   has to be processed to obtain the full view of the type.
      if not Is_Private_Type (Full_View_Entity) then
         pragma Assert (Nkind (Declaration_Node (Full_View_Entity)) =
                          N_Full_Type_Declaration);
         --  The full_type_declaration corresponding to the
         --  incomplete_type_declaration is Full_View_Entity
         --  register the full view in the symbol_table.
         Register_Type_Declaration
           (Declaration_Node (Full_View_Entity), Full_View_Entity);
      else
         Do_Private_Type_Declaration
           (Declaration_Node (Full_View_Entity));
      end if;
   end Do_Incomplete_Type_Declaration;

   -----------------------------------------
   -- Do_Index_Or_Discriminant_Constraint --
   -----------------------------------------

   --  For now, don't encode the constraint in the Irep form; we'll generate
   --  appropriate checks in the front-end, rather than delegating to CBMC
   --  as for range checks.
   function Do_Index_Or_Discriminant_Constraint
     (N : Node_Id; Underlying : Irep) return Irep
   is (Underlying);

   -----------------------------------------
   --          Do_Range_In_Case           --
   -----------------------------------------

   --  Handle the case of a range expression in a case statement alternative
   --  expression. Generalised to work for both case expressions and case
   --  statements.
   function Do_Range_In_Case (N : Node_Id; Symbol : Irep) return Irep is
      Result : constant Irep := Make_Op_And
        (Source_Location => Get_Source_Location (N),
         I_Type => CProver_Bool_T);
      Lower_Bound : constant Irep := Do_Expression
                                       (Low_Bound (N));
      Upper_Bound : constant Irep := Do_Expression
                                       (High_Bound (N));
      Geq_Lower : constant Irep := Make_Op_Geq (Rhs => Lower_Bound,
                                                Lhs => Symbol,
                                                Source_Location =>
                                                  Get_Source_Location (N),
                                                I_Type => Make_Bool_Type);
      Leq_Upper : constant Irep := Make_Op_Leq (Rhs => Upper_Bound,
                                                Lhs => Symbol,
                                                Source_Location =>
                                                  Get_Source_Location (N),
                                                I_Type => Make_Bool_Type);
   begin
      Append_Op (Result, Geq_Lower);
      Append_Op (Result, Leq_Upper);
      return Result;
   end Do_Range_In_Case;

   -----------------------
   -- Do_Case_Statement --
   -----------------------

   function Do_Case_Statement (N : Node_Id) return Irep is
      Value : constant Irep := Do_Expression (Expression (N));

      function Make_Case_Test (Alts : List_Id) return Irep;

      --  Auxiliary function to create a single test case
      --  to emplace in a condition from a list of alternative
      --  values.
      function Make_Case_Test (Alts : List_Id) return Irep is
         function Make_Single_Test (Alt : Node_Id) return Irep;
         function Make_Single_Test (Alt : Node_Id) return Irep is
         begin
            if Nkind (Alt) /= N_Range then
               return Make_Op_Eq (Lhs => Value,
                                  Rhs => Do_Expression (Alt),
                                  I_Type => Make_Bool_Type,
                                  Source_Location =>
                                    Get_Source_Location (Alt));
            else
               return Do_Range_In_Case (Alt, Value);
            end if;
         end Make_Single_Test;
         First_Alt_Test : constant Irep := Make_Single_Test (First (Alts));
         This_Alt : Node_Id := First (Alts);
      begin
         Next (This_Alt);
         if not Present (This_Alt) then
            return First_Alt_Test;
         end if;
         declare
            Big_Or : constant Irep := Make_Op_Or
              (Source_Location => Get_Source_Location (This_Alt),
               I_Type => CProver_Bool_T);
         begin
            Append_Op (Big_Or, First_Alt_Test);
            while Present (This_Alt) loop
               Append_Op (Big_Or, Make_Single_Test (This_Alt));
               Next (This_Alt);
            end loop;
            return Big_Or;
         end;
      end Make_Case_Test;

      function Make_Case_If_Then_Else_Statements (Alternatives : Node_Id)
                                                 return Irep;
      function Make_Case_If_Then_Else_Statements (Alternatives : Node_Id)
                                                 return Irep
      is
         This_Alternative : constant Node_Id := Alternatives;
         Remaining_Alternatives : Node_Id := This_Alternative;
         This_Block : constant Irep := Process_Statements
           (Statements (This_Alternative));
      begin
         Next (Remaining_Alternatives);
         --  check if this is the last case in the chain
         if not Present (Remaining_Alternatives) then
            --  no condition, because at least one case must be hit
            --  and if its not any of the previous ones it must be this one
            return This_Block;
         else
            return Make_Code_Ifthenelse
              (Cond => Make_Case_Test (Discrete_Choices (This_Alternative)),
               Then_Case => This_Block,
               Else_Case => Make_Case_If_Then_Else_Statements
                 (Remaining_Alternatives),
               Source_Location => Get_Source_Location (This_Alternative));
         end if;
      end Make_Case_If_Then_Else_Statements;
   begin
      --  there must at least be one alternative
      pragma Assert (List_Length (Alternatives (N)) >= 1);
      return Ret : constant Irep :=
        Make_Code_Block (Get_Source_Location (N)) do
         Append_Op
           (Ret,
            Make_Case_If_Then_Else_Statements (First (Alternatives (N))));
      end return;
   end Do_Case_Statement;

   -----------------------
   -- Do_Loop_Statement --
   -----------------------

   function Do_Loop_Statement (N : Node_Id) return Irep is
      Iter_Scheme  : constant Node_Id := Iteration_Scheme (N);
      Body_Block   : constant Irep := Process_Statements (Statements (N));
      Loop_Irep : Irep;
      Loop_Wrapper : constant Irep := Make_Code_Block
        (Get_Source_Location (N));
   begin
      if not Present (Iter_Scheme) then
         Loop_Irep := Make_Code_While
           (Loop_Body => Body_Block,
            Cond => CProver_True,
            Source_Location => Get_Source_Location (N));
      else
         if Present (Condition (Iter_Scheme)) then
            --  WHILE loop
            declare
               Cond : constant Irep := Do_Expression (Condition (Iter_Scheme));
            begin
               Loop_Irep := Make_Code_While
                 (Loop_Body => Body_Block,
                  Cond => Cond,
                  Source_Location => Get_Source_Location (N));
            end;
         else
            --  FOR loop.
            --   Ada 1995: loop_parameter_specification
            --   Ada 2012: +iterator_specification
            if Present (Loop_Parameter_Specification (Iter_Scheme)) then
               declare
                  Spec : constant Node_Id :=
                    Loop_Parameter_Specification (Iter_Scheme);
                  Loopvar_Name : constant String :=
                    Unique_Name (Defining_Identifier (Spec));

                  function Get_Range (Spec : Node_Id)
                     return Node_Id;

                  function Get_Range (Spec : Node_Id)
                     return Node_Id
                  is
                     Dsd : Node_Id := Discrete_Subtype_Definition (Spec);
                  begin
                     if Nkind (Dsd) = N_Subtype_Indication then
                        Dsd := Range_Expression (Constraint (Dsd));
                     end if;

                     return Dsd;
                  end Get_Range;

                  Dsd : Node_Id;

                  Type_Loopvar : constant Irep := Do_Type_Reference
                    (Etype (Etype (Defining_Identifier (Spec))));

                  Sym_Loopvar : constant Irep :=
                    Make_Symbol_Expr
                      (Source_Location =>
                         Get_Source_Location (Defining_Identifier (Spec)),
                       I_Type          => Type_Loopvar,
                       Identifier      => Loopvar_Name);

                  Init : Irep;
                  Cond : Irep;
                  Post : Irep;

                  Bound_Low : Irep;
                  Bound_High : Irep;
                  Pre_Dsd : Node_Id := Discrete_Subtype_Definition (Spec);
               begin
                  if Nkind (Pre_Dsd) = N_Subtype_Indication then
                     Pre_Dsd := Range_Expression (Constraint (Pre_Dsd));
                  end if;
                  if Nkind (Pre_Dsd) /= N_Signed_Integer_Type_Definition
                    and Nkind (Pre_Dsd) /= N_Range
                    and  Nkind (Pre_Dsd) /= N_Real_Range_Specification
                    and not (Present (Scalar_Range (Etype (Pre_Dsd))))
                  then
                     Report_Unhandled_Node_Empty (Pre_Dsd,
                                                  "Do_While_Statement",
                                                  "Wrong Nkind spec");
                     return Loop_Wrapper;
                  end if;
                  if Nkind (Pre_Dsd) = N_Identifier or
                    Nkind (Pre_Dsd) = N_Expanded_Name
                  then
                     Dsd := Scalar_Range (Etype (Pre_Dsd));
                  else
                     Dsd := Get_Range (Spec);
                  end if;
                  if not (Present (Low_Bound (Dsd))) then
                     Report_Unhandled_Node_Empty (Dsd,
                                                  "Do_While_Statement",
                                                  "No range in subtype");
                     return Loop_Wrapper;
                  end if;
                  Bound_Low := Do_Expression (Low_Bound (Dsd));
                  Bound_High := Do_Expression (High_Bound (Dsd));
                  --  Loop var decl
                  Append_Op (Loop_Wrapper, Make_Code_Decl
                             (Symbol          => Sym_Loopvar,
                              Source_Location => Get_Source_Location
                                (Defining_Identifier (Spec))));

                  --  TODO: needs generalization to support enums
                  if Reverse_Present (Spec) then
                     Init := Make_Code_Assign
                       (Lhs => Sym_Loopvar,
                        Rhs => Typecast_If_Necessary
                          (Bound_High,
                           Get_Type (Sym_Loopvar),
                           Global_Symbol_Table),
                        Source_Location => Get_Source_Location (Spec));
                     Cond := Make_Op_Geq
                       (Rhs             => Bound_Low,
                        Lhs             => Sym_Loopvar,
                        Source_Location => Get_Source_Location (Spec),
                        Overflow_Check  => False,
                        I_Type          => Make_Bool_Type,
                        Range_Check     => False);
                     Post := Make_Increment
                       (Sym_Loopvar, Etype (Low_Bound (Dsd)), -1);
                  else
                     Init := Make_Code_Assign
                       (Lhs => Sym_Loopvar,
                        Rhs => Typecast_If_Necessary
                          (Bound_Low,
                           Get_Type (Sym_Loopvar),
                           Global_Symbol_Table),
                        Source_Location => Get_Source_Location (Spec));
                     Cond := Make_Op_Leq
                       (Rhs             => Bound_High,
                        Lhs             => Sym_Loopvar,
                        Source_Location => Get_Source_Location (Spec),
                        Overflow_Check  => False,
                        I_Type          => Make_Bool_Type,
                        Range_Check     => False);
                     Post := Make_Increment
                       (Sym_Loopvar, Etype (Low_Bound (Dsd)), 1);
                  end if;
                  Set_Source_Location (Post, Get_Source_Location (Spec));
                  Loop_Irep := Make_Code_For
                    (Loop_Body => Body_Block,
                     Cond => Cond,
                     Init => Init,
                     Iter => Post,
                     Source_Location => Get_Source_Location (N));
               end;
            else
               if not Present (Iterator_Specification (Iter_Scheme)) then
                  Report_Unhandled_Node_Empty (N, "Do_While_Statement",
                                           "Scheme specification not present");
                  return Loop_Wrapper;

               end if;
               Report_Unhandled_Node_Empty (N, "Do_While_Statement",
                                            "Loop iterators not implemented");
               return Loop_Wrapper;
            end if;
         end if;
      end if;

      Set_Loop_Body (Loop_Irep, Body_Block);

      Append_Op (Loop_Wrapper, Loop_Irep);

      --  if GNAT has created the loop identifier, we do not
      --  need a label because the user cannot reference it
      if not Has_Created_Identifier (N) then
         Append_Op (Loop_Wrapper,
                    Make_Code_Label
                      (Code            => Make_Code_Skip
                         (Source_Location =>
                            Get_Source_Location (Identifier (N))),
                       Source_Location => Get_Source_Location (Identifier (N)),
                       Label           => Get_Name_String
                         (Chars (Identifier (N))) & "_exit"));
      end if;
      return Loop_Wrapper;
   end Do_Loop_Statement;

   -----------------------------------------
   --  Do_N_Block_Statement (nested declares)
   -----------------------------------------

   function Do_N_Block_Statement (N : Node_Id) return Irep is
   begin
      return Do_Subprogram_Or_Block (N);
   end Do_N_Block_Statement;

   ---------------
   -- Do_Pragma --
   ---------------

   procedure Do_Pragma (N : Node_Id; Block : Irep) is

      --------------------------------
      -- Do_Pragma_Assert_or_Assume --
      --------------------------------

      --  Handle pragmas that result in a simple assert or assume statement in
      --  the resulting goto program
      procedure Do_Pragma_Assert_or_Assume
        (N_Orig : Node_Id; Block : Irep);
      --  Handle pragmas that suppress some checks by explicitly ignoring them
      procedure Do_Pragma_Suppress
        (N_Orig : Node_Id);
      procedure Do_Pragma_Refine
        (N_Orig : Node_Id);

      procedure Do_Pragma_Assert_or_Assume
        (N_Orig : Node_Id; Block : Irep)
      is
         Which : constant Pragma_Id := Get_Pragma_Id (N_Orig);

         --  I've tried pretty printing but this way seems to be easier and
         --  more accurate basically, just go to the source file, scan for
         --  opening paren to find the start of the condition, then scan for
         --  a closing paren or a comma to find the end of the condition,
         --  ignoring comments
         function Get_Assert_Condition_As_Ada_Source_Code return String;
         function Get_Assert_Condition_As_Ada_Source_Code return String is
            Source_File : Ada.Text_IO.File_Type;
            package SU renames Ada.Strings.Unbounded;
            Condition_Text_Buffer : SU.Unbounded_String;

            --  Skip over whitespace and comments then return the next
            --  character that isn't either of those
            function Get_Next_Relevant_Char return Character;
            function Get_Next_Relevant_Char return Character is
               Char : Character;
               use Ada.Characters.Handling; --  Is_Space
               --  We want to convert all consecutive whitespace
               --  and/or comments into a single space,
               --  but only if there's at least one of them
               Have_Written_Space : Boolean := False;
            begin
               loop
                  --  I don't bother checking for EOF in here because we know
                  --  that the source file is valid Ada (otherwise it wouldn't
                  --  have passed the parsing stage in the frontend), so this
                  --  will terminate before hitting EOF
                  Get (Source_File, Char);
                  if Char = '-' then
                     --  check if this is the start of a comment
                     declare
                        Next_Char : Character;
                        Is_End_Of_Line : Boolean;
                     begin
                        Look_Ahead (Source_File, Next_Char, Is_End_Of_Line);
                        if not Is_End_Of_Line and then Next_Char = '-' then
                           Skip_Line (Source_File);
                        else
                           return Char;
                        end if;
                     end;
                  elsif Is_Space (Char) then
                     --  just skip this
                     null;
                  else
                     --  not a comment, newline or whitespace character
                     return Char;
                  end if;
                  if not Have_Written_Space then
                     --  insert a whitespace character if we skipped over at
                     --  least one (or a comment, same difference)
                     SU.Append (Condition_Text_Buffer, ' ');
                     Have_Written_Space := True;
                  end if;
               end loop;
            end Get_Next_Relevant_Char;

            procedure Collect_Condition_Text;
            procedure Collect_Condition_Text is
               Parentheses_Depth : Integer := 0;
            begin
               --  find start of expression
               while Get_Next_Relevant_Char /= '(' loop
                  null;
               end loop;
               Parentheses_Depth := 1;
               --  record expression until end, which for a Pragma Assert is
               --  either the last ')' if it's the form `Pragma Assert (Cond)`
               --  or the top level ',' if it's the form
               --  `Pragma Assert (Cond, Message)`
               --  (fortunately it is guaranteed to come before the message)
               loop
                  declare
                     Next_Char : constant Character := Get_Next_Relevant_Char;
                  begin
                     case Next_Char is
                        when '(' =>
                           Parentheses_Depth := Parentheses_Depth + 1;
                        when ')' =>
                           if Parentheses_Depth = 1 then
                              exit;
                           end if;
                           Parentheses_Depth := Parentheses_Depth - 1;
                        when ',' =>
                           if Parentheses_Depth = 1 then
                              exit;
                           end if;
                        when '=' =>
                           if Parentheses_Depth = 1 then
                              --  Check for =>, so we can strip the
                              --  optional "Check =>" prefix if it's there
                              declare
                                 Maybe_Gt_Char : Character;
                                 Is_End_Of_Line : Boolean;
                              begin
                                 Look_Ahead
                                   (Source_File,
                                    Maybe_Gt_Char,
                                    Is_End_Of_Line);
                                 if not Is_End_Of_Line
                                   and then Maybe_Gt_Char = '>'
                                 then
                                    Get (Source_File, Maybe_Gt_Char);
                                    Condition_Text_Buffer :=
                                      SU.Null_Unbounded_String;
                                    goto Skip_Append;
                                 end if;
                              end;
                           end if;
                        when others =>
                           null;
                     end case;
                     SU.Append (Condition_Text_Buffer, Next_Char);
                     <<Skip_Append>>
                  end;
               end loop;
            end Collect_Condition_Text;

            use Ada.Strings.Fixed;
            use Sinput;
            Assertion_Sloc : constant Source_Ptr := Sloc (N);
            Start_Line : constant Positive_Count :=
              Positive_Count (Get_Logical_Line_Number (Assertion_Sloc));
            Start_Column : constant Positive_Count :=
              Positive_Count (Get_Column_Number (Assertion_Sloc));

            Source_File_Name : constant String :=
              Get_Name_String
                (Full_File_Name
                   (Get_Source_File_Index
                      (Assertion_Sloc)));
         begin
            Open
              (File => Source_File,
               Mode => In_File,
               Name => Source_File_Name);
            Set_Line (Source_File, Start_Line);
            Set_Col (Source_File, Start_Column);
            Collect_Condition_Text;
            Close (Source_File);
            return Ada.Strings.Fixed.Trim
              (SU.To_String (Condition_Text_Buffer), Ada.Strings.Both);
         end Get_Assert_Condition_As_Ada_Source_Code;

         function Make_Assert_Comment return Irep;
         function Make_Assert_Comment return Irep
         is
            Source_Loc : constant Irep := Get_Source_Location (N);
            Context_Name : constant String := Get_Context_Name (N);
            Comment_Prefix : constant String := "assertion ";
            Comment : constant String :=
              Get_Assert_Condition_As_Ada_Source_Code;
         begin
            Set_Property_Class (Source_Loc, "assertion");
            Set_Function (Source_Loc, Context_Name);
            Set_Comment (Source_Loc, Comment_Prefix & Comment);
            return Source_Loc;
         end Make_Assert_Comment;

         function Make_Assert_Or_Assume (Condition : Irep) return Irep;
         function Make_Assert_Or_Assume (Condition : Irep) return Irep is
         begin
            if Which = Pragma_Assume then
               return Make_Code_Assume
                 (Assumption => Condition,
                  Source_Location => Get_Source_Location (N));
            else
               return Make_Code_Assert
                 (Assertion => Condition,
                  Source_Location => Make_Assert_Comment);
            end if;
         end Make_Assert_Or_Assume;

         Check : Irep := Ireps.Empty;

         ----------------
         -- Handle_Arg --
         ----------------

         procedure Handle_Arg
           (Arg_Pos : Positive; Arg_Name : Name_Id; Expr : Node_Id);

         --  Handle_Arg is called in a loop, Arg_Pos stores the loop iterations
         --  Arg_Name is the name of the parameter
         --  Expr is the Expression associated with the parameter
         procedure Handle_Arg
           (Arg_Pos : Positive; Arg_Name : Name_Id; Expr : Node_Id) is
         begin

            if Arg_Name = Name_Check
              or else (Arg_Name = No_Name and then Arg_Pos = 1)
            then
               Check := Do_Expression (Expr);
            elsif Arg_Name = Name_Message
              or else (Arg_Name = No_Name and then Arg_Pos = 2)
            then
               null; -- ignore, since assert irep has no msg
            else
               Report_Unhandled_Node_Empty (N, "Do_Pragma_Assert_or_Assume",
                                            "Unknown arg name");
            end if;
         end Handle_Arg;

         procedure Iterate_Args is new
           --  Iteration is a generic function which takes Handle_Arg as a
           --  call-back.
           Iterate_Pragma_Parameters (Handle_Arg => Handle_Arg);

      begin
         --  We iterate over the pragma parameters calling Handle_Arg on each
         --  parameter
         Iterate_Args (N_Orig);
         if Check = Ireps.Empty then
            Report_Unhandled_Node_Empty (N, "Do_Pragma_Assert_or_Assume",
                                         "Unassigned arg name");
         end if;

         Append_Op
           (Block, Make_Assert_Or_Assume
              (Condition => Check));
      end Do_Pragma_Assert_or_Assume;

      procedure Do_Pragma_Suppress
        (N_Orig : Node_Id)
      is
         --  To be set by iterator:
         Suppress_Scope : Name_Id;

         ----------------
         -- Handle_Arg --
         ----------------

         procedure Handle_Arg
           (Arg_Pos : Positive; Arg_Name : Name_Id; Expr : Node_Id);

         procedure Handle_Arg
           (Arg_Pos : Positive; Arg_Name : Name_Id; Expr : Node_Id) is
         begin

            --  Suppress pragma only takes one argument with no name
            --  The expression stores the scope in Chars
            if Arg_Name = No_Name and then
              Arg_Pos = 1 and then
              Nkind (Expr) = N_Identifier
            then
               Suppress_Scope := Chars (Expr);
            else
               Report_Unhandled_Node_Empty (N, "Do_Pragma_Suppress",
                                            "Unknown arg name");
            end if;
         end Handle_Arg;

         procedure Iterate_Args is new
           Iterate_Pragma_Parameters (Handle_Arg => Handle_Arg);

      begin
         Iterate_Args (N_Orig);
         if Suppress_Scope = Name_All_Checks
         then
            null; -- Intentionally ignoring this request to suppress all checks
         else
            null; -- Intentionally ignoring supressing some checks
         end if;

      end Do_Pragma_Suppress;

      procedure Do_Pragma_Refine
        (N_Orig : Node_Id)
      is
         --  To be set by iterator:
         Components : List_Id;
         Component_Expression : Node_Id;
         CEE : List_Id;
         Scope_Size : Nat;

         ----------------
         -- Handle_Arg --
         ----------------

         procedure Handle_Arg
           (Arg_Pos : Positive; Arg_Name : Name_Id; Expr : Node_Id);

         procedure Handle_Arg
           (Arg_Pos : Positive; Arg_Name : Name_Id; Expr : Node_Id) is
         begin

            if Arg_Name = No_Name and then
              Arg_Pos = 1 and then
            --  Refine pragma takes parameters whose arguments are aggregates.
            --  The following code demonstrates how to access the refine
            --  arguments.
              Nkind (Expr) = N_Aggregate
            then
               Components := Component_Associations (Expr);
               Component_Expression := Expression (First (Components));
               CEE := Expressions (Component_Expression);
               Scope_Size := Scope_Size + List_Length (CEE);
            else
               Report_Unhandled_Node_Empty (N, "Do_Pragma_Refine",
                                            "Unknown arg name");
            end if;
         end Handle_Arg;

         procedure Iterate_Args is new
           Iterate_Pragma_Parameters (Handle_Arg => Handle_Arg);

      begin
         Iterate_Args (N_Orig);
         if Scope_Size >= 1
         then
            Report_Unhandled_Node_Empty (N, "Do_Pragma Refine",
                                      "Refinement known but unsupported.");
         end if;

      end Do_Pragma_Refine;

      N_Orig : Node_Id;

   begin
      if not Present (Original_Node (N)) then
         Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                      "Original node not present");
      end if;
      N_Orig := Original_Node (N);
      case Pragma_Name (N_Orig) is
         when Name_Assert |
              Name_Assume |
              Name_Assert_And_Cut |
            --  Assert and introduce a cut point: the prover can safely forget
            --  evaluations of local variables and only assume the asserted
            --  condition. This could be used in symex (making it concolic)
            --  but is only an optimization.
            Name_Loop_Invariant =>
            --  Equivalent to assert but also introduces a cut point wrt. the
            --  variables local to the loop.
            Do_Pragma_Assert_or_Assume (N_Orig, Block);
         when Name_Precondition =>
            Do_Pragma_Assert_or_Assume (N_Orig, Block);
         when Name_Postcondition =>
            --  Postcondition will eventually also be translated into
            --  assertions but they should hold elsewhere from where they are
            --  defined and they refer to 'Result variables
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unsupported pragma: Postcondition");
         when Name_Refined_State |
              Name_Refined_Global |
              Name_Refined_Depends =>
            --  We are not supporting refinement at this point
            --  Using it would (probably) require modification to CBMC
            Do_Pragma_Refine (N_Orig);
         when Name_Suppress =>
            --  Suppressing is effectively also ignored (elaborated as example)
            Do_Pragma_Suppress (N_Orig);
         when Name_SPARK_Mode =>
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unsupported pragma: SPARK Mode");
         when Name_Global =>
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unsupported pragma: Global");
         when Name_Variant =>
            --  Could as well be ignored but is another verification condition
            --  that should be checked
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unsupported pragma: Variant");
         when Name_Asynchronous =>
            --  Allows a remote subprogram call to return prior to completion
            --  of the execution of the corresponding remote subprogram body.
            --  It changes the semantics wrt to thread interleavings.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unsupported pragma: Asynchronous");
         when Name_Atomic |
              Name_Atomic_Components =>
            --  For an atomic object all reads and updates of the object as a
            --  whole are indivisible. It changes the semantics wrt to thread
            --  interleavings.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unsupported pragma: Atomic");
         when Name_Volatile |
              Name_Volatile_Components =>
            --  For a volatile object all reads and updates of the object as a
            --  whole are performed directly to memory. In sequential execution
            --  they may be modified by the environment. Effectively, they need
            --  to be modelled as non-deterministic input in every state. It
            --  changes the semantics wrt to thread interleavings.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unsupported pragma: Volatile");
         when Name_Attach_Handler =>
            --  The expression in the Attach_Handler pragma as evaluated at
            --  object creation time specifies an interrupt. As part of the
            --  initialization of that object, if the Attach_Handler pragma is
            --  specified, the handler procedure is attached to the specified
            --  interrupt. A check is made that the corresponding interrupt is
            --  not reserved. We do not support that check yet.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unsupported pragma: Attach Handler");
         when Name_Import =>
            --  Used to import an entity defined in a foreign language into an
            --  Ada program, thus allowing a foreign-language subprogram to
            --  be called from Ada, or a foreign-language variable to be
            --  accessed from Ada. This would (probably) require gnat2goto to
            --  understand the foreign code, which we do not at the moment.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unsupported pragma: Import");
         when Name_Convention =>
            --  Used to specify that an Ada entity should use the conventions
            --  of another language. It is intended primarily for types and
            --  callback subprograms. Same reason for not supporting as above.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unsupported pragma: Convention");
         when Name_Elaborate =>
            --  Specifies that the body of the named library unit is elaborated
            --  before the current library_item. We will support packages.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unsupported pragma: Elaborate");
         when Name_Elaborate_All =>
            --  Specifies that each library_item that is needed by the named
            --  library unit declaration is elaborated before the current
            --  library_item. Same reason for future support as above.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unsupported pragma: Elaborate All");
         when Name_Elaborate_Body =>
            --  Specifies that the body of the library unit is elaborated
            --  immediately after its declaration. Same reason for future
            --  support as above.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unsupported pragma: Elaborate Body");
         when Name_Preelaborate =>
            --  If a library unit is preelaborated, then its declaration, if
            --  any, and body, if any, are elaborated prior to all
            --  non-preelaborated library_item s of the partition. Same reason
            --  for future support as above.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                 "Known but unsupported pragma: Preelaborate");
         when Name_Locking_Policy =>
            --  Specifies whether or not protected objects have priorities, and
            --  the relationships between these priorities and task priorities.
            --  This may change thread interleaving.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unsupported pragma: Locking Policy");
         when Name_Normalize_Scalars =>
            --  Ensures that an otherwise uninitialized scalar object is set to
            --  a predictable value, but out of range if possible. This
            --  obviously changes the behaviour.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                      "Unsupported pragma: Normalize Scalars");
         when Name_Queuing_Policy =>
            --  Governs the order in which tasks are queued for entry
            --  service, and the order in which different entry queues are
            --  considered for service. This may change the behaviour.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unsupported pragma: Queuing Policy");
         when Name_Remote_Types =>
            --  Defines types intended for use in communication between active
            --  partitions. Concurrency may be supported in the future.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unsupported pragma: Remote Types");
         when Name_Restrictions =>
            --  Expresses the user's intent to abide by certain restrictions.
            --  This could probably be implemented as an assertion eventually.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unsupported pragma: Restrictions");
         when Name_Shared_Passive =>
            --  Used for managing global data shared between active partitions.
            --  Concurrency may be supported in the future.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unsupported pragma: Shared Passive");
         when Name_Task_Dispatching_Policy =>
            --  Specifies the details of task dispatching that are not covered
            --  by the basic task dispatching model. Concurrency may be
            --  supported in the future.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                "Unsupported pragma: Task Dispatching Policy");
         when Name_Unreferenced =>
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                "Unsupported pragma: Unreferenced");
         when Name_All_Calls_Remote |
              Name_Remote_Call_Interface =>
            --  Library unit pragma; used by the distributed systems annex
            --  Interface for remote function calls between active partitions
            --  Should not alter the semantics, but we want to know about it.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                  "Known but unsupported pragma: Remote Call");
         when Name_Interrupt_Handler =>
            --  If the pragma appears in a protected_definition, then the
            --  corresponding procedure can be attached dynamically, as a
            --  handler, to interrupts. We want to detect interrupts early.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                            "Known but unsupported pragma: Interrupt Handler");
         when Name_Controlled =>
            --  Used to prevent any automatic reclamation of storage (garbage
            --  collection) for the objects created by allocators of a given
            --  access type. Resource allocation problem must be detected.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                   "Known but unsupported pragma: Controlled");
         when Name_Export =>
            --  Used to export an Ada entity to a foreign language, thus
            --  allowing an Ada subprogram to be called from a foreign
            --  language, or an Ada object to be accessed from a foreign
            --  language. Need to be detected.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                       "Known but unsupported pragma: Export");
         when Name_Annotate |
            --  Ignore here. Rather look for those when we process a node.
              Name_Assertion_Policy |
            --  Control the pragma Assert according to the policy identifier
            --  which can be Check, Ignore, or implementation-defined.
            --  Ignore means that assertions are ignored at run-time -> Ignored
              Name_Compile_Time_Warning |
            --  Used to issue a compile time warning from the compiler
            --  front-end.  The warning will be issued by the front-end but has
            --  no affect on the AST.  It can be ignored safely by gnat2goto.
              Name_Discard_Names |
            --  Used to request a reduction in storage used for the names of
            --  certain entities. -> Ignored
              Name_Inline |
            --  Indicates that inline expansion is desired for all calls to
            --  that entity. -> Ignored
              Name_Inspection_Point |
            --  Identifies a set of objects each of whose values is to be
            --  available at the point(s) during program execution
            --  corresponding to the position of the pragma in the compilation
            --  unit. -> Ignored
              Name_Linker_Options |
            --  Used to specify the system linker parameters needed when a
            --  given compilation unit is included in a partition. We want to
            --  know that code manipulates the linking. The
            --  goto functions produced by gnat2goto are linked by symtab2gb.
            --  Currently there very few options for this linker and none that
            --  apply to most linkers.  Currently  the pragma can ignored,
            --  but in the future, if symtab2gb was to take more options
            --  this pragma could be reinstated.
              Name_List |
            --  Takes one of the identifiers On or Off as the single
            --  argument. It specifies that listing of the compilation is to be
            --  continued or suspended until a List pragma with the opposite
            --  argument is given within the same compilation. -> Ignored
              Name_Page |
            --  Specifies that the program text which follows the pragma should
            --  start on a new page (if the compiler is currently producing a
            --  listing). -> Ignored
              Name_Optimize |
            --  Gives advice to the implementation as to whether time or space
            --  is the primary optimization criterion. -> Ignored
              Name_Pack |
            --  Specifies that storage minimization should be the main
            --  criterion when selecting the representation of a composite
            --  type. -> Ignored
              Name_Pure |
            --  Used to declare that a library unit is pure: does not contain
            --  declaration of any variable or named access type. -> Ignored
              Name_Reviewable |
            --  Directs the implementation to provide information to facilitate
            --  analysis and review of a program's object code. -> Ignored
              Name_Storage_Size |
            --  Specifies the amount of storage to be reserved for the
            --  execution of a task. -> Ignored
              Name_Unsuppress |
            --  enables or disables a set of compiler warnings based on
            --  template
            --  these come from the frontend, so they're not really relevant to
            --  what we're doing here -> Ignored
              Name_Warnings =>
            --  Voids the supressing request. -> Ignored
            null;
         when others =>
            declare
               Name_Diagnostic : constant String :=
                 (if  Pragma_Name (N) /= No_Name
                  then Get_Name_String (Pragma_Name (N))
                  else "<No Name>");
            begin
               Report_Unhandled_Node_Empty
                 (N,
                  "Do_Pragma",
                  "Unknown pragma name: "
                    & Name_Diagnostic);
            end;
      end case;
   end Do_Pragma;

   ---------------------------
   -- Do_Object_Declaration --
   ---------------------------

   procedure Do_Object_Declaration (N : Node_Id; Block : Irep) is
      Obj_Id : constant Symbol_Id :=
        Intern (Unique_Name (Defining_Identifier (N)));
   begin
      --  First check for object declarations which are not constants
      if not Constant_Present (N) then
         --  Not any sort of constant.
         --  Process non-constant object_declaration.
         Do_Object_Declaration_Full (N, Block);
      elsif --  Check that this isn't a completion of a deferred constant.
         not Global_Symbol_Table.Contains (Obj_Id)
      then
         --  The declaration is of constant which may be deferred.
         declare
            Entity : constant Entity_Id := Defining_Identifier (N);
            --  The full view of a deferred constant is obtained
            --  by calling the Full_View function.  As the gnat front-end
            --  has completed semantic analysis before invoking the
            --  gnat to goto translation all object_declarations that are
            --  deferred constants should have a full view unless the
            --  declaration has the pragma Import applied.
            Full_View_Entity : constant Entity_Id := Full_View (Entity);

         begin
            if not Has_Init_Expression (N) and then
              Present (Full_View_Entity)
            then
               --  The constant declaration has no initialisation expression
               --  so it is a deferred constant declaration with a completion.
               --  The completion must be a full constant declaration given
               --  by the full view of the entity.
               --  Process the declaration node of the full view and
               --  register it in the symbol table so that it is not
               --  processed again when the completion is encountered in
               --  the tree.
               New_Valueless_Object_Symbol_Entry (Intern (Unique_Name
                                          (Defining_Identifier (N))),
                                          Global_Symbol_Table);
               --  Adds a dummy entry to the symbol table to register that a
               --  constant has already been processed.

               Do_Object_Declaration_Full
                 (Declaration_Node (Full_View_Entity), Block);
            else
               --  The constant declaration is not deferred or has the
               --  pragma Import applied and its value is defined externally.
               Do_Object_Declaration_Full (N, Block);
            end if;
         end;
      end if;

      pragma Assert (Global_Symbol_Table.Contains (Obj_Id));
   end Do_Object_Declaration;

   --------------------------------------------
   -- Do_Object_Declaration_Full_Declaration --
   --------------------------------------------

   procedure Do_Object_Declaration_Full
     (N : Node_Id; Block : Irep) is
      Defined : constant Entity_Id := Defining_Identifier (N);
      Id   : constant Irep := Do_Defining_Identifier (Defined);
      Decl : constant Irep := Make_Code_Decl
        (Symbol => Id,
         Source_Location => Get_Source_Location (N));
      Init_Expr : Irep := Ireps.Empty;

      Obj_Id : constant Symbol_Id := Intern (Unique_Name (Defined));
      Obj_Type : constant Irep := Get_Type (Id);

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
            Report_Unhandled_Node_Empty (N,
                                         "Do_Object_Declaration",
                                         "Record definition of wrong nkind");
            return False;
         end if;
         Component_Iter :=
           First (Component_Items (Component_List (Record_Def)));
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
         return Has_Defaulted_Discriminants (E)
           or else Has_Defaulted_Components (E)
           or else Ekind (E) = E_Array_Subtype
           or else (Ekind (E) = E_Private_Type
                    and then Present (Full_View (E))
                    and then Ekind (Full_View (E)) = E_Array_Subtype);
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
                     Component_Type : constant Entity_Id :=
                       Etype (Defining_Identifier (Component_Iter));
                  begin
                     if Ekind (Component_Type) in Aggregate_Kind then
                        New_Expr :=
                          Make_Default_Initialiser (Component_Type,
                                                    Types.Empty);
                     else
                        New_Expr := Make_Side_Effect_Expr_Nondet
                          (I_Type => Do_Type_Reference (Component_Type),
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
           (Source_Location => Get_Source_Location (N),
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
                        Report_Unhandled_Node_Empty (N,
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
               Report_Unhandled_Node_Empty (N,
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
            return Make_Array_Default_Initialiser (E);
         elsif Ekind (E) in Record_Kind then
            return Make_Record_Default_Initialiser (E, DCs);
         elsif Ekind (E) in Private_Kind and then Present (Full_View (E))
           and then Ekind (Full_View (E)) in Array_Kind
         then
            return Make_Array_Default_Initialiser (Full_View (E));
         else
            return Report_Unhandled_Node_Irep (E, "Make_Default_Initialiser",
                                                 "Unknown Ekind");
         end if;
      end Make_Default_Initialiser;

      procedure Update_Value (Key : Symbol_Id; Element : in out Symbol);
      procedure Update_Value (Key : Symbol_Id; Element : in out Symbol) is
      begin
         pragma Assert (Unintern (Key) = Unintern (Obj_Id));
         Element.Value := Init_Expr;
      end Update_Value;

      --  Begin processing for Do_Object_Declaration_Full_Declaration
   begin
      Append_Op (Block, Decl);

      if Has_Init_Expression (N) or Present (Expression (N)) then
         Init_Expr := Do_Expression (Expression (N));
      elsif Needs_Default_Initialisation (Etype (Defined)) or
        (Present (Object_Definition (N)) and then
         Nkind (Object_Definition (N)) = N_Subtype_Indication)
      then
         declare
            Defn : constant Node_Id := Object_Definition (N);
            Discriminant_Constraint : constant Node_Id :=
              (if Nkind (Defn) = N_Subtype_Indication
                 then Constraint (Defn) else Types.Empty);
         begin
            Init_Expr :=
              Make_Default_Initialiser (Etype (Defined),
                                        Discriminant_Constraint);
         end;
      end if;

      pragma Assert (Get_Identifier (Id) = Unintern (Obj_Id));
      if not Global_Symbol_Table.Contains (Obj_Id)
      then
         New_Object_Symbol_Entry (Object_Name       => Obj_Id,
                                  Object_Type       => Obj_Type,
                                  Object_Init_Value => Init_Expr,
                                  A_Symbol_Table    => Global_Symbol_Table);
      elsif Init_Expr /= Ireps.Empty then
         Global_Symbol_Table.Update_Element
           (Position => Global_Symbol_Table.Find (Obj_Id),
            Process  => Update_Value'Access);
      end if;

      if Init_Expr /= Ireps.Empty then
         Append_Op (Block, Make_Code_Assign (Lhs => Id,
                Rhs => Typecast_If_Necessary (Init_Expr, Get_Type (Id),
                                              Global_Symbol_Table),
                                             Source_Location =>
                                               Get_Source_Location (N)));
      end if;
   end Do_Object_Declaration_Full;

   -------------------------
   --     Do_Op_Not       --
   -------------------------

   function Do_Op_Not (N : Node_Id) return Irep is
      Value : constant Irep := Do_Expression (Right_Opnd (N));
   begin
      return Make_Op_Not (Value, Get_Source_Location (N), Make_Bool_Type);
   end Do_Op_Not;

   function Do_Op_Mod_Not (N : Node_Id; Ret_Type : Irep) return Irep is
      Followed_Type : constant Irep :=
              Follow_Symbol_Type (Ret_Type, Global_Symbol_Table);
      Value : constant Irep := Do_Expression (Right_Opnd (N));
      Source_Loc : constant Irep := Get_Source_Location (N);
      --  In case the not-operator (not X) is called on a modular-type
      --  (mod Y) variable the result should be: (Y-1)-X
      Mod_Max_String : constant String :=
        Get_Ada_Mod_Max (Followed_Type);
      Mod_Max : constant Irep :=
        Make_Constant_Expr (Source_Location => Source_Loc,
                            I_Type          => Ret_Type,
                            Range_Check     => False,
                            Value           => Mod_Max_String);
      One : constant Irep :=
        Make_Constant_Expr (Source_Location => Source_Loc,
                            I_Type          => Ret_Type,
                            Range_Check     => False,
                            Value           => "1");
      Mod_Max_Value : constant Irep :=
        Make_Op_Sub (Rhs             => One,
                     Lhs             => Mod_Max,
                     Source_Location => Source_Loc,
                     Overflow_Check  => False,
                     I_Type          => Ret_Type);
   begin
      pragma Assert (Kind (Followed_Type) = I_Ada_Mod_Type);
      return Make_Op_Sub (Rhs             => Value,
                          Lhs             => Mod_Max_Value,
                          Source_Location => Source_Loc,
                          Overflow_Check  => False,
                          I_Type          => Ret_Type);
   end Do_Op_Mod_Not;

   function Do_Unsigned_Op_Not (N : Node_Id) return Irep is
      Negated_Value : constant Irep := Do_Expression (Right_Opnd (N));
      Followed_Type : constant Irep := Follow_Symbol_Type
        (Do_Type_Reference (Etype (N)),
         Global_Symbol_Table);
   begin
      pragma Assert (Kind (Followed_Type) = I_Unsignedbv_Type);
      return Make_Op_Bitnot
        (Op0 => Negated_Value,
         Source_Location => Get_Source_Location (N),
         I_Type => Followed_Type);
   end Do_Unsigned_Op_Not;
   -------------------------
   --     Do_Op_Minus    --
   -------------------------

   function Do_Op_Minus (N : Node_Id) return Irep is
      Original_Value : constant Irep := Do_Expression (Right_Opnd (N));
      Original_Value_Type : constant Irep := Do_Type_Reference (Etype (N));
      Unchecked_Result : constant Irep :=
        Make_Op_Neg (Original_Value,
                     Get_Source_Location (N),
                     Original_Value_Type);
   begin
      return (if Do_Overflow_Check (N)
              then Make_Overflow_Assert_Expr (N     => N,
                                              Value => Unchecked_Result)
              else Unchecked_Result);
   end Do_Op_Minus;

   -------------------------
   --      Do_Bit_Op      --
   -------------------------

   --  We identified that the constructor for operator `or` and operator
   --  `and` were pretty much the same, with the only difference being
   --  the constructor being called. So to avoid needless duplication,
   --  we simplified it to a single function that does the same thing,
   --  and just calls the appropriate constructor via a function pointer.
   --
   --  This produces the following code in pseudocode (let A and B be True):
   --    A or B
   --  --
   --    int intA = (int) A;       // 1
   --    int intB = (int) B;       // 1
   --    int intC = A bitop B      // bitop can be `or` or `and`
   --    int R = (boolean) intC;
   --    return R;
   function Do_Bit_Op (N : Node_Id;
                       Operator : Bit_Operand_Constructor)
                       return Irep is
      Source_Loc : constant Irep := Get_Source_Location (N);
      LHS_Value : constant Irep := Do_Expression (Left_Opnd (N));
      RHS_Value : constant Irep := Do_Expression (Right_Opnd (N));

      Ret_Type : constant Irep := Do_Type_Reference (Etype (N));
      Followed_Type : constant Irep :=
        Follow_Symbol_Type (Ret_Type, Global_Symbol_Table);

   begin
      if Kind (Followed_Type) = I_Ada_Mod_Type then
         declare
            Mod_Max_String : constant String :=
              Get_Ada_Mod_Max (Followed_Type);
            Mod_Max : constant Irep :=
              Make_Constant_Expr (Source_Location => Source_Loc,
                                  I_Type          => Ret_Type,
                                  Range_Check     => False,
                                  Value           => Mod_Max_String);
            Full_Result : constant Irep :=
              Operator (Lhs => LHS_Value,
                        Rhs => RHS_Value,
                        Source_Location => Source_Loc,
                        Overflow_Check => False,
                        Range_Check => False,
                        I_Type => Ret_Type);
         begin
            if Nkind (N) = N_Op_And then
               return Full_Result;
            else
               return Make_Op_Mod (Rhs               => Mod_Max,
                                   Lhs               => Full_Result,
                                   Div_By_Zero_Check => False,
                                   Source_Location   => Source_Loc,
                                   Overflow_Check    => False,
                                   I_Type            => Ret_Type);
            end if;
         end;
      else
         declare
            Cast_LHS_To_Integer : constant Irep :=
              Make_Op_Typecast (Op0 => LHS_Value,
                                Source_Location => Source_Loc,
                                I_Type => Int32_T);
            Cast_RHS_To_Integer : constant Irep :=
              Make_Op_Typecast (Op0 => RHS_Value,
                                Source_Location => Source_Loc,
                                I_Type => Int32_T);
            R : constant Irep := Operator (Lhs => Cast_LHS_To_Integer,
                                           Rhs => Cast_RHS_To_Integer,
                                           Source_Location => Source_Loc,
                                           Overflow_Check => False,
                                           Range_Check => False,
                                           I_Type =>
                                             Get_Type (Cast_LHS_To_Integer));
         begin
            return Make_Op_Typecast (Op0 => R,
                                     Source_Location => Source_Loc,
                                     I_Type => Make_Bool_Type);
         end;
      end if;
   end Do_Bit_Op;

   function Do_Op_Abs (N : Node_Id) return Irep
     with Pre => (Nkind (N) = N_Op_Abs);

   function Do_Op_Abs (N : Node_Id) return Irep is
      Operand : constant Irep := Do_Expression (Right_Opnd (N));
   begin
      return Make_Op_Abs
        (Op0 => Operand,
         Source_Location => Get_Source_Location (N),
         I_Type => Do_Type_Reference (Etype (N)));
   end Do_Op_Abs;

   function Make_Mod_Expon_Function
     (Mod_Type : Irep;
      Exponent_Type : Irep;
      Source_Location : Irep)
   return Irep;

--  'fast' exponentiation, using the fact that
--  if e is divisible by two, then
--  b^e == b^(2 * (e/2)) == (b*b)^(e/2)
--  and otherwise if e == 2p + 1
--  then b^e = b * b^(2p)
--  so we only have to do log2(e) multiplications
--  rather than e
--
--  generates a function like this:
--  modN exp_by_squaring(modN base, unsigned exponent) {
--    modN exp_result = 1;
--    while(exponent > 0) {
--      if(exponent % 2 == 0) {
--        base *= base;
--        exponent /= 2;
--      } else {
--        exp_result *= exp_base;
--        --exponent;
--      }
--    }
--    return exp_result;
--  }
   function Make_Mod_Expon_Function
     (Mod_Type : Irep;
      Exponent_Type : Irep;
      Source_Location : Irep)
   return Irep is
      Function_Name : constant String := Fresh_Var_Name ("mod_op_expon");
      Function_Body : constant Irep := Make_Code_Block (Source_Location);
      Function_Params : constant Irep := Make_Parameter_List;
      Base_Arg : constant Irep := Create_Fun_Parameter
        (Fun_Name => Function_Name,
         Param_Name => "base",
         Param_Type => Mod_Type,
         Param_List => Function_Params,
         A_Symbol_Table => Global_Symbol_Table,
         Source_Location => Source_Location);
      Exponent_Arg : constant Irep := Create_Fun_Parameter
        (Fun_Name => Function_Name,
         Param_Name => "exponent",
         Param_Type => Exponent_Type,
         Param_List => Function_Params,
         A_Symbol_Table => Global_Symbol_Table,
         Source_Location => Source_Location);

      Function_Type : constant Irep := Make_Code_Type
        (Parameters => Function_Params,
         Ellipsis => False,
         Return_Type => Mod_Type,
         Inlined => False,
         Knr => False);

      Base_Sym : constant Irep := Param_Symbol (Base_Arg);
      Exponent_Sym : constant Irep := Param_Symbol (Exponent_Arg);

      Expon_Result : constant Irep := Fresh_Var_Symbol_Expr
        (Ty => Mod_Type,
         Infix => "mod_expon_result");
      Declare_Expon_Result : constant Irep := Make_Code_Decl
        (Symbol => Expon_Result,
         Source_Location => Source_Location);
      Expon_Divisible_By_Two : constant Irep := Make_Op_Eq
        (Lhs => Make_Op_Mod
           (Lhs => Exponent_Sym,
            Rhs => Integer_Constant_To_Expr
              (Value => Uint_2,
               Expr_Type => Exponent_Type,
               Source_Location => Source_Location),
            Source_Location => Source_Location,
            Div_By_Zero_Check => False,
            I_Type => Exponent_Type),
         Rhs => Integer_Constant_To_Expr
           (Value => Uint_0,
            Expr_Type => Exponent_Type,
            Source_Location => Source_Location),
         I_Type => Make_Bool_Type,
         Source_Location => Source_Location);
      Expon_Greater_Zero : constant Irep := Make_Op_Gt
        (Lhs => Exponent_Sym,
         Rhs => Integer_Constant_To_Expr
           (Value => Uint_0,
            Expr_Type => Exponent_Type,
            Source_Location => Source_Location),
         Source_Location => Source_Location,
         I_Type => Make_Bool_Type);
      Set_Expon_Result_To_One : constant Irep := Make_Code_Assign
        (Lhs => Expon_Result,
         Rhs => Integer_Constant_To_Expr
           (Value => Uint_1,
            Expr_Type => Mod_Type,
            Source_Location => Source_Location),
         Source_Location => Source_Location);
      Square_Base : constant Irep := Make_Code_Assign
        (Lhs => Base_Sym,
         Rhs => Do_Operator_Mod
           (LHS => Base_Sym,
            Op_Kind => I_Op_Mul,
            RHS => Base_Sym,
            Ret_Type => Mod_Type),
         Source_Location => Source_Location);
      Halve_Exponent : constant Irep := Make_Code_Assign
        (Lhs => Exponent_Sym,
         Rhs => Make_Op_Div
           (Lhs => Exponent_Sym,
            Rhs => Integer_Constant_To_Expr
              (Value => Uint_2,
               Expr_Type => Exponent_Type,
               Source_Location => Source_Location),
            I_Type => Exponent_Type,
            Source_Location => Source_Location,
            Div_By_Zero_Check => False),
         Source_Location => Source_Location);
      Multiply_Result_With_Base : constant Irep := Make_Code_Assign
        (Lhs => Expon_Result,
         Rhs => Do_Operator_Mod
           (LHS => Expon_Result,
            Op_Kind => I_Op_Mul,
            RHS => Base_Sym,
            Ret_Type => Mod_Type),
         Source_Location => Source_Location);
      Decrement_Exponent : constant Irep := Make_Code_Assign
        (Lhs => Exponent_Sym,
         Rhs => Make_Op_Sub
           (Lhs => Exponent_Sym,
            Rhs => Integer_Constant_To_Expr
              (Value => Uint_1,
               Expr_Type => Exponent_Type,
               Source_Location => Source_Location),
            I_Type => Exponent_Type,
            Source_Location => Source_Location),
         Source_Location => Source_Location);
      Multiply_Loop_Body : constant Irep := Make_Code_Block
        (Source_Location => Source_Location);
      If_Even_Body : constant Irep := Make_Code_Block
        (Source_Location => Source_Location);
      If_Odd_Body : constant Irep := Make_Code_Block
        (Source_Location => Source_Location);
      Multiply_Loop : constant Irep := Make_Code_While
        (Loop_Body => Multiply_Loop_Body,
         Cond => Expon_Greater_Zero,
         Source_Location => Source_Location);
      If_Even_Or_Odd_Exponent : constant Irep := Make_Code_Ifthenelse
        (Cond => Expon_Divisible_By_Two,
         Then_Case => If_Even_Body,
         Else_Case => If_Odd_Body,
         Source_Location => Source_Location);
   begin
      Append_Op (Function_Body, Declare_Expon_Result);
      Append_Op (Function_Body, Set_Expon_Result_To_One);
      Append_Op (Function_Body, Multiply_Loop);
      Append_Op (Function_Body, Make_Code_Return
        (Return_Value => Expon_Result,
         Source_Location => Source_Location));

      Append_Op (Multiply_Loop_Body, If_Even_Or_Odd_Exponent);

      Append_Op (If_Even_Body, Square_Base);
      Append_Op (If_Even_Body, Halve_Exponent);

      Append_Op (If_Odd_Body, Multiply_Result_With_Base);
      Append_Op (If_Odd_Body, Decrement_Exponent);

      return Symbol_Expr (New_Function_Symbol_Entry
        (Name => Function_Name,
         Symbol_Type => Function_Type,
         Value => Function_Body,
         A_Symbol_Table => Global_Symbol_Table));
   end Make_Mod_Expon_Function;

   function Do_Op_Expon (N : Node_Id) return Irep is
      LHS : constant Irep := Do_Expression (Left_Opnd (N));
      RHS : constant Irep := Do_Expression (Right_Opnd (N));
      LHS_Resolved_Type : constant Irep := Follow_Symbol_Type
        (Get_Type (LHS), Global_Symbol_Table);
      RHS_Resolved_Type : constant Irep :=
        Follow_Symbol_Type (Get_Type (RHS), Global_Symbol_Table);
   begin
      if Kind (LHS_Resolved_Type) = I_Ada_Mod_Type then
         declare
            Expon_Function : constant Irep := Make_Mod_Expon_Function
              (LHS_Resolved_Type, RHS_Resolved_Type, Get_Source_Location (N));
         begin
            return Make_Simple_Side_Effect_Expr_Function_Call
              (Arguments => (LHS, RHS),
               Function_Expr => Expon_Function,
               Source_Location => Get_Source_Location (N));
         end;
      end if;
      return Report_Unhandled_Node_Irep (N, "Do_Op_Expon",
        "Exponentiation unhandled for non mod types at the moment");
   end Do_Op_Expon;

   -------------------------
   -- Do_Operator_General --
   -------------------------

   function Do_Operator_General (N : Node_Id) return Irep is
   begin
      if Nkind (N) = N_Op_Abs then
         return Do_Op_Abs (N);
      elsif Nkind (N) = N_Op_Concat then
         return Report_Unhandled_Node_Irep (N, "Do_Operator_General",
                                            "Concat unsupported");
      elsif Nkind (N) = N_Op_Not then
         declare
            Ret_Type : constant Irep := Do_Type_Reference (Etype (N));
            Followed_Type : constant Irep :=
              Follow_Symbol_Type (Ret_Type, Global_Symbol_Table);
         begin
            case Kind (Followed_Type) is
               when I_Bool_Type => return Do_Op_Not (N);
               when I_Ada_Mod_Type => return Do_Op_Mod_Not (N, Ret_Type);
               when I_Unsignedbv_Type =>
                  return Do_Unsigned_Op_Not (N);
               when others =>
                  return Report_Unhandled_Node_Irep (N,
                                                     "Do_Operator_General",
                                                    "Mod of unsupported type");
            end case;
         end;
      elsif Nkind (N) = N_Op_Minus then
         return Do_Op_Minus (N);
      elsif Nkind (N) = N_Op_Or then
         return Do_Bit_Op (N, Make_Op_Bitor'Access);
      elsif Nkind (N) = N_Op_And then
         return Do_Bit_Op (N, Make_Op_Bitand'Access);
      elsif Nkind (N) = N_Op_Xor then
         return Do_Bit_Op (N, Make_Op_Bitxor'Access);
      elsif Nkind (N) = N_Op_Expon then
         return Do_Op_Expon (N);
      else
         if Nkind (N) /= N_And_Then
           and then Nkind (N) /= N_In
           and then Nkind (N) /= N_Not_In
           and then Nkind (N) /= N_Or_Else
           and then not (Nkind (N) in N_Binary_Op)
         then
            return Report_Unhandled_Node_Irep (N, "Do_Operator_General",
                                               "Wrong node kind");
         end if;
         return Do_Operator_Simple (N);
      end if;
   end Do_Operator_General;

   ------------------------
   -- Do_Operator_Simple --
   ------------------------

   function Do_Operator_Simple (N : Node_Id) return Irep is
      Ret_Type : constant Irep := Do_Type_Reference (Etype (N));
      Followed_Type : constant Irep :=
        Follow_Symbol_Type (Ret_Type, Global_Symbol_Table);

   begin
      if Kind (Followed_Type) = I_Ada_Mod_Type
      then
         case N_Op (Nkind (N)) is
            when N_Op_Add | N_Op_Multiply =>
               return Do_Operator_Mod (N);
            when N_Op_Subtract =>
               return Do_Operator_Sub_Mod (N);
            when others =>
               null; --  proceed as with non-mod types
         end case;
      end if;
      declare
         type Make_Binary_Operation_T is
           access function
             (Rhs : Irep;
              Lhs : Irep;
              Source_Location : Irep;
              Overflow_Check : Boolean := False;
              I_Type : Irep;
              Range_Check : Boolean := False)
            return Irep;

         --  Small helper to set additional parameter Div_By_Zero_Check
         --  to true, so the interface of Div, Rem and Mod fits
         --  with Make_Binary_Operation_T
         generic
           with function Make
             (Rhs : Irep;
              Lhs : Irep;
              Div_By_Zero_Check : Boolean;
              Source_Location : Irep;
              Overflow_Check : Boolean := False;
              I_Type : Irep := Ireps.Empty;
              Range_Check : Boolean := False)
           return Irep;
         function Make_With_Div_By_Zero_Check
           (Rhs : Irep;
            Lhs : Irep;
            Source_Location : Irep;
            Overflow_Check : Boolean := False;
            I_Type : Irep;
            Range_Check : Boolean := False)
           return Irep;

         function Make_With_Div_By_Zero_Check
           (Rhs : Irep;
            Lhs : Irep;
            Source_Location : Irep;
            Overflow_Check : Boolean := False;
            I_Type : Irep;
            Range_Check : Boolean := False)
           return Irep is
            (Make
              (Rhs => Rhs,
               Lhs => Lhs,
               Div_By_Zero_Check => Do_Division_Check (N),
               Source_Location => Source_Location,
               Overflow_Check => Overflow_Check,
               I_Type => I_Type,
               Range_Check => Range_Check));

         function Make_Div_Operation is new
           Make_With_Div_By_Zero_Check (Make => Make_Op_Div);
         function Make_Rem_Operation is new
           Make_With_Div_By_Zero_Check (Make => Make_Op_Rem);
         function Make_Mod_Operation is new
           Make_With_Div_By_Zero_Check (Make => Make_Op_Mod);

         function Make_Unsupported_Op
           (Rhs : Irep;
              Lhs : Irep;
              Source_Location : Irep;
              Overflow_Check : Boolean := False;
              I_Type : Irep;
              Range_Check : Boolean := False)
           return Irep;

         function Make_Unsupported_Op
           (Rhs : Irep;
            Lhs : Irep;
            Source_Location : Irep;
            Overflow_Check : Boolean := False;
            I_Type : Irep;
            Range_Check : Boolean := False)
          return Irep is
          (Report_Unhandled_Node_Irep
             (N,
              "Do_Operator_Simple",
              "Unsupported operand"));

         Make_Binary_Operation : constant Make_Binary_Operation_T :=
           (case N_Op (Nkind (N)) is
              when N_Op_Divide => Make_Div_Operation'Access,
              when N_Op_Add => Make_Op_Add'Access,
              when N_Op_Subtract => Make_Op_Sub'Access,
              when N_Op_Multiply => Make_Op_Mul'Access,
              when N_Op_Rem => Make_Rem_Operation'Access,
              when N_Op_Mod => Make_Mod_Operation'Access,
              when N_Op_Eq => Make_Op_Eq'Access,
              when N_Op_Ne => Make_Op_Notequal'Access,
              when N_Op_Ge => Make_Op_Geq'Access,
              when N_Op_Gt => Make_Op_Gt'Access,
              when N_Op_Le => Make_Op_Leq'Access,
              when N_Op_Lt => Make_Op_Lt'Access,
              when others => Make_Unsupported_Op'Access);

         LHS : constant Irep := Cast_Enum (Do_Expression (Left_Opnd (N)),
                                           Global_Symbol_Table);
         RHS : constant Irep := Cast_Enum (Do_Expression (Right_Opnd (N)),
                                           Global_Symbol_Table);

         --  Start of processing for Do_Operator_Simple

         Unchecked_Result : constant Irep := Make_Binary_Operation
              (Lhs => LHS,
               Rhs => Typecast_If_Necessary
                 (RHS, Get_Type (LHS), Global_Symbol_Table),
               I_Type => Ret_Type,
               Overflow_Check => Do_Overflow_Check (N),
               Source_Location => Get_Source_Location (N));

         Maybe_Overflow_Check : Irep := Unchecked_Result;
         Maybe_Division_Check : Irep := Unchecked_Result;

      begin
         if Do_Overflow_Check (N) then
            Maybe_Overflow_Check := Make_Overflow_Assert_Expr
              (N     => N,
               Value => Unchecked_Result);
         end if;

         if Nkind (N) in N_Op_Divide | N_Op_Mod | N_Op_Rem
           and then Do_Division_Check (N)
         then
            Maybe_Division_Check := Make_Div_Zero_Assert_Expr
              (N       => N,
               Value   => Maybe_Overflow_Check,
               Divisor => Get_Rhs (Unchecked_Result));
         else
            Maybe_Division_Check := Maybe_Overflow_Check;
         end if;

         return Maybe_Division_Check;
      end;
   end Do_Operator_Simple;

   --  In case the type of operands in modular we attach a I_Op_Mod to the
   --  result.
   --  Note that we do not set the overflow check it should not be necessary
   --  for modular types.
   function Do_Operator_Mod (LHS : Irep; Op_Kind : Irep_Kind;
                                    RHS : Irep; Ret_Type : Irep)
                                    return Irep is
      Followed_Ret_Type : constant Irep :=
        Follow_Symbol_Type (Ret_Type, Global_Symbol_Table);

      --  Multiplication can result in intermediate results larger than what
      --  could be stored in Ret_Type: we cast to a wider type
      Large_Enough_Type : constant Irep :=
        Maybe_Double_Type_Width (Followed_Ret_Type);

      Lhs_Cast : constant Irep :=
        Typecast_If_Necessary (LHS, Large_Enough_Type, Global_Symbol_Table);
      Rhs_Cast : constant Irep :=
        Typecast_If_Necessary (RHS, Large_Enough_Type, Global_Symbol_Table);

      --  XXX this should be from the parent node, not from the LHS
      Source_Loc : constant Irep := Get_Source_Location (LHS);

      --  Impossible because of the precondition
      Impossible : exception;
      Full_Result : constant Irep :=
        (case Op_Kind is
           when I_Op_Add => Make_Op_Add
            (Lhs => Lhs_Cast,
             Rhs => Rhs_Cast,
             Source_Location => Source_Loc,
             I_Type => Large_Enough_Type),
           when I_Op_Mul => Make_Op_Mul
            (Lhs => Lhs_Cast,
             Rhs => Rhs_Cast,
             Source_Location => Source_Loc,
             I_Type => Large_Enough_Type),
           when others => raise Impossible);

      Mod_Max_String : constant String :=
        Get_Ada_Mod_Max (Followed_Ret_Type);

      --  Extract the modulus from Ret_Type
      Mod_Max : constant Irep :=
        Make_Constant_Expr (Source_Location => Source_Loc,
                            I_Type          => Large_Enough_Type,
                            Range_Check     => False,
                            Value           => Mod_Max_String);
      Mod_Ret : constant Irep := Make_Op_Mod
        (Lhs => Full_Result,
         Rhs => Mod_Max,
         Source_Location => Source_Loc,
         Div_By_Zero_Check => False,
         I_Type => Large_Enough_Type);
   begin
      return Make_Op_Typecast
        (Op0 => Mod_Ret,
         Source_Location => Source_Loc,
         I_Type => Ret_Type);
   end Do_Operator_Mod;

   function Do_Operator_Mod (N : Node_Id) return Irep is
      Impossible_Exception : exception;
      Op_Kind : constant Irep_Kind :=
        (case N_Op (Nkind (N)) is
           when N_Op_Add => I_Op_Add,
           when N_Op_Multiply => I_Op_Mul,
           when others => raise Impossible_Exception
                          with "this case is excluded by the precondition");
      Lhs : constant Irep := Do_Expression (Left_Opnd (N));
      Rhs : constant Irep := Do_Expression (Right_Opnd (N));
      Expr_Type : constant Irep := Do_Type_Reference (Etype (N));
   begin
      return Do_Operator_Mod (Lhs, Op_Kind, Rhs, Expr_Type);
   end Do_Operator_Mod;

   --  Modular minus gets special treatment, effectively x - y =>
   --  x + (Mod_Max (T) - y)
   --  this expression never over/under-flows so no type widening is necessary
   function Do_Operator_Sub_Mod (LHS : Irep; RHS : Irep; Ret_Type : Irep)
                                 return Irep is
      Followed_Ret_Type : constant Irep :=
        Follow_Symbol_Type (Ret_Type, Global_Symbol_Table);

      Mod_Max_String : constant String :=
        Get_Ada_Mod_Max (Followed_Ret_Type);
      Source_Loc : constant Irep := Get_Source_Location (LHS);

      --  Extract the modulus from Ret_Type
      Mod_Max : constant Irep :=
        Make_Constant_Expr (Source_Location => Source_Loc,
                            I_Type          => Ret_Type,
                            Range_Check     => False,
                            Value           => Mod_Max_String);
      Mod_Rhs : constant Irep :=
        Make_Op_Sub (Rhs             => RHS,
                     Lhs             => Mod_Max,
                     Source_Location => Source_Loc,
                     Overflow_Check  => False,
                     I_Type          => Ret_Type);

   begin
      return
        Make_Op_Add (Rhs             => Mod_Rhs,
                     Lhs             => LHS,
                     Source_Location => Source_Loc,
                     Overflow_Check  => False,
                     I_Type          => Ret_Type);
   end Do_Operator_Sub_Mod;

   function Do_Operator_Sub_Mod (N : Node_Id) return Irep is
      Lhs : constant Irep := Do_Expression (Left_Opnd (N));
      Rhs : constant Irep := Do_Expression (Right_Opnd (N));
      Expr_Type : constant Irep := Do_Type_Reference (Etype (N));
   begin
      return Do_Operator_Sub_Mod (Lhs, Rhs, Expr_Type);
   end Do_Operator_Sub_Mod;

   ----------------------------
   -- Do_Package_Declaration --
   ----------------------------

   procedure Do_Package_Declaration (N : Node_Id) is
   begin
      Do_Package_Specification (Specification (N));
   end Do_Package_Declaration;

   ----------------------------
   -- Do_Package_Specification --
   ----------------------------

   procedure Do_Package_Specification (N : Node_Id) is
      Package_Decs : constant Irep := Make_Code_Block
        (Source_Location => Get_Source_Location (N));
      Package_Name : Symbol_Id;
      Package_Symbol : Symbol;
      Def_Unit_Name : Node_Id;
      Entity_Node : Node_Id;

   begin
      Def_Unit_Name := Defining_Unit_Name (N);

      --  Defining_Unit_Name will return a N_Defining_Identifier
      --  for non-child package but a N_Package_Specification when it is a
      --  child package.
      --  To obtain the Entity N_Defining_Identifier is required.
      --  The actual parameter for Unique_Name must be an Entity node.
      if Nkind (Def_Unit_Name) = N_Defining_Identifier then
         Entity_Node := Def_Unit_Name;
      else
         Entity_Node := Defining_Identifier (Def_Unit_Name);
      end if;

      Package_Name := Intern (Unique_Name (Entity_Node));
      Package_Symbol.Name       := Package_Name;
      Package_Symbol.BaseName   := Package_Name;
      Package_Symbol.PrettyName := Package_Name;
      Package_Symbol.SymType    := CProver_Void_T;
      Package_Symbol.Mode       := Intern ("C");
      Package_Symbol.Value      := Make_Nil (Get_Source_Location (N));

      Global_Symbol_Table.Insert (Package_Name, Package_Symbol);

      if Present (Visible_Declarations (N)) then
         Process_Declarations (Visible_Declarations (N), Package_Decs);
      end if;
      if Present (Private_Declarations (N)) then
         Process_Declarations (Private_Declarations (N), Package_Decs);
      end if;
   end Do_Package_Specification;

   procedure Do_Exception_Declaration (N : Node_Id) is
   begin
      null;
      --  Ignored for now
   end Do_Exception_Declaration;

   ---------------------------------
   -- Do_Private_Type_Declaration --
   ---------------------------------

   procedure Do_Private_Type_Declaration (N : Node_Id) is
      Entity : constant Entity_Id := Defining_Identifier (N);
      --  A partial view of a type declaration must not be inserted into
      --  the symbol table.
      --
      --  The full view of a private_type_declaration is obtained
      --  by calling the Full_View function.  As the compiler has completed
      --  semantic analysis before invoking the gnat to goto translation
      --  all private_type_declarations should have a full view.
      Full_View_Entity : constant Entity_Id := Full_View (Entity);
   begin
      if Is_Private_Type (Entity) then
         --  At the moment tagged types and abstract types are not supported.
         --  Limited types should be ok as limiting a type only applies
         --  constraints on its use within an Ada program.  The gnat
         --  front-end checks that these constraints are maintained by
         --  the code being analysed.
         if Is_Abstract_Type (Entity) then
            --  Ignore : Support is not necessary to capture the executable
            --  semantics of the program, because abstract state is not part of
            --  the compiled program. Maybe, at some point in the future, we
            --  might want to improve the tooling to use these but it is not
            --  incorrect for us to just ignore them.
            null;
            return;
         elsif Is_Tagged_Type (Entity) then
            Report_Unhandled_Node_Empty (N, "Do_Private_Type_Declaration",
                                      "Tagged type declaration unsupported");
            return;
         end if;
         --  The private_type_declaration is neither tagged or abstract.
         --  The Full_View of the declaratin will have been processed by the
         --  gnat front-end and will be Full_View_Entity.

         --  A private_type_declaration may be the completion of an
         --  incomplete_type_declaration.  The processing of the
         --  incomplete_type_declaration will have inserted (registered) the
         --  full view of the private_type_declartion into the table already.
         --  It is not obvious how to check that the private_type_declaration
         --  is a completion of an incomplete_type_declaration from the tree
         --  but it does not matter because its prior existence in the symbol
         --  will prevent it being re-inserted through a second registration.

         if Nkind (Declaration_Node (Full_View_Entity)) =
           N_Full_Type_Declaration
         then
            --  The full_type_declaration corresponding to the
            --  private_type_declaration is Full_View_Entity
            --  register the full view in the symbol table.
            Register_Type_Declaration
              (Declaration_Node (Full_View_Entity), Full_View_Entity);
         else
            Report_Unhandled_Node_Empty
              (Declaration_Node (Full_View_Entity),
               "Do_Private_Type_Declaration",
               "Full view of private_type_declaration " &
               "Does not yield a full_type_declaration node");
         end if;

      else
         Report_Unhandled_Node_Empty
              (N,
               "Do_Private_Type_Declaration",
               "The node is not a private entity");
      end if;

   end Do_Private_Type_Declaration;

   ---------------------------------
   -- Do_Procedure_Call_Statement --
   ---------------------------------

   function Do_Procedure_Call_Statement (N : Node_Id) return Irep
   is
   begin
      --  It seems as though an N_Explicit_Drereference is placed in the tree
      --  even when the procedure call is an implicit dereference.
      --  Hence, implicit dereferences do not have to be seperately handled,
      --  they are handled as explicit dereferences.
      if Nkind (Name (N)) = N_Explicit_Dereference then
         declare
            Fun_Type : constant Irep :=
              Get_Subtype (Do_Type_Reference (Etype (Prefix (Name (N)))));
            Deref_Function : constant Irep := Make_Dereference_Expr
              (Object          => Do_Identifier (Prefix (Name (N))),
               Source_Location => Get_Source_Location (N),
               I_Type          => Fun_Type,
               Range_Check     => False);
         begin
            return Make_Code_Function_Call
              (Arguments       => Do_Call_Parameters (N),
               I_Function      => Deref_Function,
               Lhs             => CProver_Nil,
               Source_Location => Get_Source_Location (N),
               I_Type          => CProver_Void_T,
               Range_Check     => False);
         end;
      end if;

      if not (Nkind (Name (N)) in N_Has_Entity)
        and then Nkind (Name (N)) /= N_Aspect_Specification
        and then Nkind (Name (N)) /= N_Attribute_Definition_Clause
        and then Nkind (Name (N)) /= N_Freeze_Entity
        and then Nkind (Name (N)) /= N_Freeze_Generic_Entity
      then
         return Report_Unhandled_Node_Irep
           (N,
            "Do_Procedure_Call_Statement",
            "Wrong nkind of name");
      end if;
      declare
         Callee : constant Unbounded_String
           := To_Unbounded_String (Unique_Name (Entity (Name (N))));
         Sym_Id : constant Symbol_Id := Intern (To_String (Callee));
      begin
         if not Global_Symbol_Table.Contains (Sym_Id) then
            return Report_Unhandled_Node_Irep
              (N,
               "Do_Procedure_Call_Statement",
               "sym id not in symbol table");
         end if;
         declare
            --  ??? use Get_Entity_Name from gnat2why to handle entries and
            --  entry families (and most likely extend it for accesses to
            --  subprograms).

            Function_Type : constant Irep := Global_Symbol_Table
              (Sym_Id).SymType;
         begin
            return Make_Code_Function_Call
              (I_Function => Make_Symbol_Expr
                 (Identifier => To_String (Callee),
                  I_Type => Function_Type,
                  Source_Location => Get_Source_Location (N)),
               Arguments => Do_Call_Parameters (N),
               Lhs => CProver_Nil,
               Source_Location => Get_Source_Location (N));
         end;
      end;
   end Do_Procedure_Call_Statement;

   -------------------------
   -- Do_Range_Constraint --
   -------------------------

   function Do_Range_Constraint (N : Node_Id; Underlying : Irep)
                                     return Irep
   is
      Range_Expr : constant Node_Id := Range_Expression (N);
      Resolved_Underlying : constant Irep :=
        Follow_Symbol_Type (Underlying, Global_Symbol_Table);
      --  ??? why not get this from the entity

      function Get_Array_Attr_Bound_Symbol (Bound_Node : Node_Id)
                                            return Bound_Type_Symbol
        with Pre => Get_Attribute_Id (Attribute_Name (Bound_Node))
          in Attribute_First | Attribute_Last;
      function Get_Array_Attr_Bound_Symbol (Bound_Node : Node_Id)
                                            return Bound_Type_Symbol
      is
      begin
         if Get_Attribute_Id (Attribute_Name (Bound_Node)) =  Attribute_First
         then
            return Bound_Type_Symbol (Do_Array_First (Bound_Node));
         else
            return Bound_Type_Symbol (Do_Array_Last (Bound_Node));
         end if;
      end Get_Array_Attr_Bound_Symbol;

      procedure Set_Bound_Value (Bound : Node_Id;
                                 Bound_Value : out Integer;
                                 Ok : out Boolean)
      with Pre => Is_OK_Static_Expression (Bound);
      --  For static expressions, the gnat front end replaces all attribute
      --  references by the lower and upper bounds of the attributed prefix.
      --  If the type of the range is an integer type, it folds the lower and
      --  upper bounds expressions into their intege value. If the range is
      --  an enumeration type it sets the lower and upper bounds to the
      --  enumeration literal identifiers of the bounds.

      procedure Set_Bound_Value (Bound : Node_Id;
                                 Bound_Value : out Integer;
                                 Ok : out Boolean) is
      begin
         Ok := False;
         Bound_Value := 0;
         case Nkind (Bound) is
         when N_Integer_Literal =>
            Bound_Value :=
              Store_Nat_Bound (Bound_Type_Nat (Intval (Bound)));
            Ok := True;
         when N_Identifier =>
            Bound_Value :=
                 Store_Symbol_Bound (Bound_Type_Symbol (
                                     Do_Identifier (Bound)));
            Ok := True;
            when others =>
               null;
         end case;
      end Set_Bound_Value;

      Lower_Bound : constant Node_Id := Low_Bound (Range_Expr);
      Upper_Bound : constant Node_Id := High_Bound (Range_Expr);

      Lower_Bound_Value : Integer;
      Upper_Bound_Value : Integer;

      Ok : Boolean;
   begin
      if not (Kind (Resolved_Underlying) in Class_Bitvector_Type or
              Kind (Resolved_Underlying) = I_C_Enum_Type)
      then
         return Report_Unhandled_Node_Type (Range_Expr,
                                            "Do_Base_Range_Constraint",
                                        "range expression not bitvector type");
      end if;

      if Is_OK_Static_Range (Range_Expr) then
         Set_Bound_Value (Lower_Bound, Lower_Bound_Value, Ok);
         if not Ok then
            return Report_Unhandled_Node_Type
              (Lower_Bound,
               "Do_Base_Range_Constraint",
               "unsupported lower range kind");
         end if;
         Set_Bound_Value (Upper_Bound, Upper_Bound_Value, Ok);
         if not Ok then
            return Report_Unhandled_Node_Type
              (Upper_Bound,
               "Do_Range_Constraint",
               "unsupported upper range kind");
         end if;

      elsif Nkind (Lower_Bound) = N_Attribute_Reference and then
        (Get_Attribute_Id (Attribute_Name (Lower_Bound)) =
           Attribute_First and
             (Get_Attribute_Id (Attribute_Name (Upper_Bound))) =
             Attribute_Last)
      then
         Lower_Bound_Value :=
           Store_Symbol_Bound
             (Get_Array_Attr_Bound_Symbol (Lower_Bound));
         Upper_Bound_Value :=
           Store_Symbol_Bound (Get_Array_Attr_Bound_Symbol (Upper_Bound));
      else
         return Report_Unhandled_Node_Type
           (Lower_Bound,
            "Do_Range_Constraint",
            "only static ranges are supported");
      end if;

      declare
         Width : constant Integer :=
           (if Kind (Resolved_Underlying) = I_C_Enum_Type
             then Get_Width (Get_Subtype (Resolved_Underlying))
             else Get_Width (Resolved_Underlying));
      begin
         return
           (if Kind (Resolved_Underlying) in I_Ada_Mod_Type | I_Unsignedbv_Type
            then Make_Bounded_Unsignedbv_Type
              (Width => Width,
               Lower_Bound => Lower_Bound_Value,
               Upper_Bound => Upper_Bound_Value)
            else Make_Bounded_Signedbv_Type
              (Width => Width,
               Lower_Bound => Lower_Bound_Value,
               Upper_Bound => Upper_Bound_Value));
      end;
   end Do_Range_Constraint;

   --------------------------
   -- Do_Record_Definition --
   --------------------------

   function Do_Record_Definition (N : Node_Id; Discs : List_Id) return Irep is

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
      procedure Do_Variant_Struct (Var : Node_Id; Union_Components : Irep);

      --------------------------
      -- Add_Record_Component --
      --------------------------

      procedure Add_Record_Component (Comp_Name : String;
                                      Comp_Type_Node : Node_Id;
                                      Comp_Node : Node_Id;
                                      Add_To_List : Irep := Components) is
      begin
         Declare_Itype (Comp_Type_Node);
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

      procedure Do_Variant_Struct (Var : Node_Id; Union_Components : Irep) is
         Struct_Type : constant Irep :=
           Do_Record_Definition (Var, List_Id (Types.Empty));
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

   --  Start of processing for Do_Record_Definition

   begin

      --  Create fields for any discriminants:
      --  This order (discriminant, common fields, variant fields)
      --  seems to match GNAT's record-literal ordering (apparently
      --  regardless of source ordering).
      while Present (Disc_Iter) loop
         Add_Record_Component (Unique_Name (Defining_Identifier (Disc_Iter)),
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
               Add_Record_Component_Raw (
                 "_variants", Union_Symbol, Variants_Node);
            end;
         end;
      end if;

      return Make_Struct_Type
        (Tag => "This will be filled in later by Do_Type_Declaration",
         Components => Components);
   end Do_Record_Definition;

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
      Orig_Component : constant Entity_Id :=
        Original_Record_Component (Component);
      Component_Type : constant Irep := Do_Type_Reference (Etype (Component));
      Component_Name : constant String := Unique_Name (Orig_Component);
      Source_Location : constant Irep := Get_Source_Location (N);
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

   ----------------------------------
   -- Do_Signed_Integer_Definition --
   ----------------------------------

   function Do_Signed_Integer_Definition (N : Node_Id) return Irep is
      E : constant Entity_Id := Defining_Entity (Parent (N));
   begin
      --  ??? This sounds like it should be a precondition
      if not Is_Type (E) then
         return Report_Unhandled_Node_Irep
           (N, "Do_Signed_Integer_Definition",
            "Entity id is not a type");
      end if;
      return Make_Bounded_Signedbv_Type
        (Lower_Bound =>
           Store_Nat_Bound (Bound_Type_Nat (Intval (Low_Bound (N)))),
         Upper_Bound =>
           Store_Nat_Bound (Bound_Type_Nat (Intval (High_Bound (N)))),
         Width => Positive (UI_To_Int (Esize (E))));
   end Do_Signed_Integer_Definition;

   ----------------------------------
   -- Do_Floating_Point_Definition --
   ----------------------------------

   function Do_Floating_Point_Definition (N : Node_Id) return Irep is
      E : constant Entity_Id := Defining_Entity (Parent (N));

      --  This determines if ranges were specified or not
      Scalar_Range_Ent : constant Node_Id := Scalar_Range (E);
      Width : constant Integer := Integer (UI_To_Int (Esize (E)));
      Mantissa : constant Integer := Float_Mantissa_Size (Width);
   begin
      if Nkind (Scalar_Range_Ent) = N_Real_Range_Specification then
         --  If user specified range bounds we store them
         declare
            Range_Spec : constant Node_Id := Real_Range_Specification (N);
            Lower_Bound : constant Integer :=
              Store_Real_Bound
              (Bound_Type_Real (Realval (Low_Bound (Range_Spec))));
            Upper_Bound : constant Integer :=
              Store_Real_Bound
              (Bound_Type_Real (Realval (High_Bound (Range_Spec))));
         begin
            return Make_Bounded_Floatbv_Type
              (Width => Width,
               F => Mantissa,
               Lower_Bound => Lower_Bound,
               Upper_Bound => Upper_Bound);
         end;
      else
         return Make_Floatbv_Type
           (Width => Width,
            F => Mantissa);
      end if;
   end Do_Floating_Point_Definition;

   --------------------------------
   -- Do_Simple_Return_Statement --
   --------------------------------

   function Do_Simple_Return_Statement (N : Node_Id) return Irep is
      Return_Value : constant Irep :=
        (if Present (Expression (N))
         then Do_Expression (Expression (N))
         else CProver_Nil);
   begin
      return Make_Code_Return
        (Return_Value => Return_Value,
         Source_Location => Get_Source_Location (N));
   end Do_Simple_Return_Statement;

   function Do_Raise_Statement (N : Node_Id) return Irep is
      Source_Loc : constant Irep := Get_Source_Location (N);

      Func_Params : constant Irep := Make_Parameter_List;
      Func_Type : constant Irep :=
           Make_Code_Type (Parameters  => Func_Params,
                           Ellipsis    => False,
                           Return_Type => CProver_Void_T,
                           Inlined     => False,
                           Knr         => False);
      Function_Name : constant String := "__CPROVER_Ada_Raise_Exception";

      Exception_Id_String : constant Irep :=
        Make_String_Constant_Expr
          (Text       => Unique_Name (Entity (Name (N))),
           Source_Loc => Source_Loc);
      Exception_Comment : constant Irep :=
        (if Present (Expression (N)) then Do_String_Constant (Expression (N))
         else Make_String_Constant_Expr (Text       => "",
                                         Source_Loc => Source_Loc));
      Exception_Call_Arguments : constant Irep := Make_Argument_List;
      Body_Block : constant Irep := Make_Code_Block (Source_Loc);
      Function_Symbol : constant Symbol :=
        New_Function_Symbol_Entry (Name           => Function_Name,
                                   Symbol_Type    => Func_Type,
                                   Value          => Body_Block,
                                   A_Symbol_Table => Global_Symbol_Table);
      Assert_Comment : constant Irep :=
        Make_String_Constant_Expr (Text       => "Ada Exception",
                                   Source_Loc => Source_Loc);
      Assert_False : constant Irep :=
        Make_Assert_Call (Assertion   => Get_Int32_T_Zero,
                          Description => Assert_Comment,
                          Source_Loc  => Source_Loc,
                          A_Symbol_Table => Global_Symbol_Table);
      Assume_False : constant Irep :=
        Make_Assume_Call (Assumption   => Get_Int32_T_Zero,
                          Source_Loc   => Source_Loc,
                          A_Symbol_Table => Global_Symbol_Table);
      Char_Pointer_Type : constant Irep := Make_Pointer_Type (Int8_T);
   begin
      Create_Fun_Parameter (Fun_Name        => Function_Name,
                            Param_Name      => "name",
                            Param_Type      => Char_Pointer_Type,
                            Param_List      => Func_Params,
                            A_Symbol_Table  => Global_Symbol_Table,
                            Source_Location => Source_Loc);
      Create_Fun_Parameter (Fun_Name        => Function_Name,
                            Param_Name      => "comment",
                            Param_Type      => Char_Pointer_Type,
                            Param_List      => Func_Params,
                            A_Symbol_Table  => Global_Symbol_Table,
                            Source_Location => Source_Loc);

      Append_Op (Body_Block, Assert_False);
      Append_Op (Body_Block, Assume_False);

      Append_Argument
        (Exception_Call_Arguments,
         String_To_Char_Pointer (String_Irep    => Exception_Id_String,
                                 A_Symbol_Table => Global_Symbol_Table));
      Append_Argument
        (Exception_Call_Arguments,
         String_To_Char_Pointer (String_Irep    => Exception_Comment,
                                 A_Symbol_Table => Global_Symbol_Table));

      return Make_Code_Function_Call
        (Arguments       => Exception_Call_Arguments,
         I_Function      => Symbol_Expr (Function_Symbol),
         Lhs             => Make_Nil (Source_Loc),
         Source_Location => Source_Loc,
         I_Type          => CProver_Void_T);
   end Do_Raise_Statement;

   ------------------------
   -- Do_Subprogram_Body --
   ------------------------

   procedure Do_Subprogram_Body (N : Node_Id) is
      Proc_Name   : constant Symbol_Id :=
        Intern (Unique_Name (Defining_Entity (N)));

      Proc_Symbol : Symbol;
   begin
      --  Corresponding_Spec is optional for subprograms
      --  but it should always be present for generic subprograms,
      --  so this check should be sufficient
      if Present (Corresponding_Spec (N)) and then
        Ekind (Corresponding_Spec (N))
        in E_Generic_Function | E_Generic_Procedure
      then
         return;
      end if;
      if not Global_Symbol_Table.Contains (Proc_Name) then
         --  A subprogram body does not have to have a separate declaration
         --  so it may not be in the symbol table.
         --  The subprogram specification of the subprogram body is used to
         --  populate the symbol table instead.
         Register_Subprogram_Specification (Specification (N));
      end if;
      --  Todo aspect_specification, i.e. pre/post-conditions
      --  Now the subprogram should registered in the symbol table
      --  whether a separate declaration was provided or not.
      if not Global_Symbol_Table.Contains (Proc_Name) then
         Report_Unhandled_Node_Empty (N, "Do_Subprogram_Body",
                                      "Proc name not in symbol table");
      end if;
      Proc_Symbol := Global_Symbol_Table (Proc_Name);

      --  Compile the subprogram body and update its entry in the symbol table.
      Proc_Symbol.Value := Do_Subprogram_Or_Block (N);
      Global_Symbol_Table.Replace (Proc_Name, Proc_Symbol);
   end Do_Subprogram_Body;

   -----------------------------
   -- Do_Subprogram_Body_Stub --
   -----------------------------

   procedure Do_Subprogram_Body_Stub (N : Node_Id) is
   begin
      --  The Gnat compilation model requires that a file
      --  containing the separate subprogram body is present
      --  otherwise a compilation error is generated.
      --  Therefore, the subunit will always be present when gnat2goto
      --  encounters a Subprogram_Body_Stub.
      Do_Subprogram_Body (Proper_Body (Unit ((Library_Unit (N)))));
   end Do_Subprogram_Body_Stub;

-------------------------------
   -- Do_Subprogram_Declaration --
   -------------------------------

   procedure Do_Subprogram_Declaration (N : Node_Id) is
      E : constant Node_Id := Defining_Unit_Name (Specification (N));
   begin
      pragma Assert (Ekind (E) in Subprogram_Kind);
      Register_Subprogram_Specification (Specification (N));

      if Is_Intrinsic_Subprogram (E)
        and Nkind (Specification (N)) = N_Function_Specification
      then
         Check_For_Intrinsic_Address_Functions :
         declare
            Fun_Sort : constant
              ASVAT.Address_Model.Address_To_Access_Functions :=
                ASVAT.Address_Model.Get_Intrinsic_Address_Function (E);
         begin
            if Fun_Sort = ASVAT.Address_Model.To_Pointer_Function then
               --  Make a body for
               --  function To_Pointer (System.Address) return access T.
               ASVAT.Address_Model.Make_To_Pointer (E);
            elsif Fun_Sort = ASVAT.Address_Model.To_Address_Function then
              --  Make a body for
              --  function To_Pointer (V : T) return Systen.Address.
               ASVAT.Address_Model.Make_To_Address (E);
            else
               --  If the intrinsic funtion is not from an instatiation of
               --  the System.Address_To_Access package nothing is done.
               null;
            end if;
         end Check_For_Intrinsic_Address_Functions;
      end if;
   end Do_Subprogram_Declaration;

   ----------------------------
   -- Do_Subprogram_Or_Block --
   ----------------------------

   function Do_Subprogram_Or_Block (N : Node_Id) return Irep is
      Decls : constant List_Id := Declarations (N);
      HSS   : constant Node_Id := Handled_Statement_Sequence (N);
      Reps : constant Irep := Make_Code_Block
        (Source_Location => Get_Source_Location (N));
      All_Handlers : constant Irep := Make_Code_Block
        (Source_Location => Get_Source_Location (N));
   begin
      if Present (Decls) then
         Process_Declarations (Decls, Reps);
      end if;

      if Present (HSS) then
         Process_Statement (HSS, Reps);
      end if;

      if Present (HSS) and then Present (Exception_Handlers (HSS)) then
         declare
            A_Handler : Node_Id := First (Exception_Handlers (HSS));
         begin
            while Present (A_Handler) loop
               Append_Op (All_Handlers,
                          Process_Statements (Statements (A_Handler)));
               Next (A_Handler);
            end loop;
         end;
         Append_Op (Reps,
                    Make_Code_Ifthenelse
                      (Cond            => Typecast_If_Necessary
                         (Expr           => Get_Int32_T_Zero,
                          New_Type       => Make_Bool_Type,
                          A_Symbol_Table => Global_Symbol_Table),
                       Then_Case       => All_Handlers,
                       Else_Case       => Make_Code_Block
                         (Get_Source_Location (N)),
                       Source_Location => Get_Source_Location (N),
                       I_Type          => Make_Nil_Type,
                       Range_Check     => False));
      end if;

      if Nkind (N) = N_Subprogram_Body and then
        Present (Corresponding_Spec (N)) and then
        No_Return (Corresponding_Spec (N))
      then
         Append_Op (Reps, Get_No_Return_Check);
      end if;

      return Reps;
   end Do_Subprogram_Or_Block;

   --------------------------------
   -- Do_Subprogram_Specification --
   --------------------------------

   function Do_Subprogram_Specification (N : Node_Id) return Irep is
      Param_List : constant Irep := Make_Parameter_List;
      Param_Iter : Node_Id := First (Parameter_Specifications (N));
   begin
      while Present (Param_Iter) loop
         declare
            Param_Sort : constant Node_Id := Parameter_Type (Param_Iter);
         begin
            if not (Nkind (Param_Sort)
                    in N_Has_Etype | N_Access_Definition)
            then
               return Report_Unhandled_Node_Type
                 (N,
                  "Do_Subprogram_Specification",
                  "Param iter is not an access parameter or has no etype");
            end if;
            declare
               Is_Out : constant Boolean := Out_Present (Param_Iter);

               --  A subprogram can have a formal access parameter of the form
               --  procedure P (Ptr_To_ObjectOf_Type_T : access T);
               Is_Access_Param : constant Boolean :=
                 Nkind (Param_Sort) = N_Access_Definition;

               Param_Name : constant String :=
                 Unique_Name (Defining_Identifier (Param_Iter));

               Param_Ada_Type  : constant Node_Id :=
                 (if Is_Access_Param then
                     Etype (Subtype_Mark (Param_Sort))
                  else
                     Etype (Parameter_Type (Param_Iter)));

               Param_Type_Base : constant Irep :=
                 Do_Type_Reference (Param_Ada_Type);

               --  If the formal parameter is mode out or in out,
               --  or is an access parameter, it is made into a pointer
               Param_Type : constant Irep :=
                 (if Is_Out or Is_Access_Param then
                     Make_Pointer_Type (Param_Type_Base)
                  else Param_Type_Base);
               Param_Irep : constant Irep := Make_Code_Parameter
                 (Source_Location => Get_Source_Location (Param_Iter),
                  I_Type => Param_Type,
                  Identifier => Param_Name,
                  Base_Name => Param_Name,
                  This => False,
                  Default_Value => Ireps.Empty);
            begin
               Append_Parameter (Param_List, Param_Irep);
               New_Parameter_Symbol_Entry
                 (Name_Id        => Intern (Param_Name),
                  BaseName       => Param_Name,
                  Symbol_Type    => Param_Type,
                  A_Symbol_Table => Global_Symbol_Table);

               Next (Param_Iter);
            end;
         end;
      end loop;
      return Make_Code_Type
        (Parameters => Param_List,
         Ellipsis => False,
         Return_Type =>
           (if Nkind (N) = N_Function_Specification
              then Do_Type_Reference (Etype (Result_Definition (N)))
              else CProver_Void_T),
         Inlined => False,
         Knr => False);
   end Do_Subprogram_Specification;

   ----------------------------
   -- Do_Subtype_Declaration --
   ----------------------------

   procedure Do_Subtype_Declaration (N : Node_Id) is
      New_Type : constant Irep := Do_Subtype_Indication
        (Subtype_Indication (N));
   begin
      Do_Type_Declaration (New_Type, Defining_Identifier (N));
   end Do_Subtype_Declaration;

   ---------------------------
   -- Do_Subtype_Indication --
   ---------------------------

   function Do_Subtype_Indication (N : Node_Id) return Irep
   is
      Underlying : Irep;
      Constr : Node_Id;
   begin
      case Nkind (N) is
         when N_Subtype_Indication =>
            Underlying := Do_Type_Reference (Etype (Subtype_Mark (N)));
            Constr := Constraint (N);
            if Present (Constr) then
               case Nkind (Constr) is
               when N_Range_Constraint =>
                  return Do_Range_Constraint (Constr, Underlying);
               when N_Index_Or_Discriminant_Constraint =>
                  return
                    Do_Index_Or_Discriminant_Constraint (Constr, Underlying);
               when others =>
                  return
                    Report_Unhandled_Node_Irep (N, "Do_Subtype_Indication",
                                                "Unknown expression kind");
               end case;
            else
               return Underlying;
            end if;
         when N_Identifier |
              N_Expanded_Name =>
            --  subtype indications w/o constraint are given only as identifier
            Underlying := Do_Type_Reference (Etype (N));
            return Underlying;
         when others =>
            return Report_Unhandled_Node_Irep (N, "Do_Subtype_Indication",
                                               "Unknown expression kind");
      end case;
   end Do_Subtype_Indication;

   ------------------------
   -- Do_Type_Conversion --
   ------------------------

   function Do_Type_Conversion (N : Node_Id) return Irep is
      To_Convert : constant Irep := Do_Expression (Expression (N));
      New_Type   : constant Irep := Do_Type_Reference (Etype (N));
      Maybe_Checked_Op : constant Irep :=
        (if Do_Range_Check (Expression (N))
         then Make_Range_Assert_Expr
           (N => N,
            Value => To_Convert,
            Bounds_Type => New_Type)
         else To_Convert);
   begin
      return Make_Op_Typecast
        (Op0 => Maybe_Checked_Op,
         I_Type => New_Type,
         Source_Location => Get_Source_Location (N));
   end Do_Type_Conversion;

   -------------------------
   -- Do_Type_Declaration --
   -------------------------

   procedure Do_Type_Declaration (New_Type_In : Irep; E : Entity_Id) is
      New_Type        : constant Irep := New_Type_In;
      New_Type_Name   : constant String := Unique_Name (E);
      New_Type_Name_Id   : constant Symbol_Id := Intern (New_Type_Name);
      New_Type_Symbol : constant Symbol :=
        Make_Type_Symbol (New_Type_Name_Id,
                          (if New_Type_Name /= "system__address" then
                           --  For all type declarations except the
                           --  private type System.Address we use the
                           --  type declared in the source code.
                              New_Type
                           else
                           --  System.Address is modelled as a pointer
                           --  to a byte (an array of bytes).
                              Make_Pointer_Type
                             (I_Subtype => Make_Unsignedbv_Type (8),
                              Width     => Pointer_Type_Width)));
   begin
      if Kind (New_Type) = I_Struct_Type then
         Set_Tag (New_Type, New_Type_Name);
      end if;
      if not Symbol_Maps.Contains (Global_Symbol_Table, New_Type_Name_Id) then
         Symbol_Maps.Insert (Global_Symbol_Table, New_Type_Name_Id,
                             New_Type_Symbol);
      end if;
   end Do_Type_Declaration;

   ------------------------
   -- Do_Type_Definition --
   ------------------------

   function Do_Type_Definition (N : Node_Id; Discs : List_Id) return Irep is
   begin
      if Discs /= List_Id (Types.Empty)
        and then Nkind (N) /= N_Record_Definition
      then
         return Report_Unhandled_Node_Irep (N, "Do_Type_Definition",
                                            "Wrong Nkind or wrong discs");
      end if;
      case Nkind (N) is
         when N_Record_Definition =>
            return Do_Record_Definition (N, Discs);
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
         when N_Modular_Type_Definition =>
            return Do_Modular_Type_Definition (N);
         when N_Floating_Point_Definition =>
            return Do_Floating_Point_Definition (N);
         when N_Access_Function_Definition =>
            return Do_Access_Function_Definition (N);
         when N_Access_Procedure_Definition =>
            return Do_Access_Function_Definition (N);
         when N_Access_To_Object_Definition =>
            return Do_Access_To_Object_Definition (N);
         when others =>
            return Report_Unhandled_Node_Type (N, "Do_Type_Definition",
                                               "Unknown expression kind");
      end case;
   end Do_Type_Definition;

   -----------------------
   -- Do_Type_Reference --
   -----------------------

   function Do_Type_Reference (E : Entity_Id) return Irep is
      Type_Name : constant String := Unique_Name
        (if Ekind (E) = E_Access_Subtype then Etype (E) else E);
      Type_Id : constant Symbol_Id := Intern (Type_Name);
   begin
      Declare_Itype (E);
      if Global_Symbol_Table.Contains (Type_Id) then
         if Kind (Global_Symbol_Table.Element (Type_Id).SymType) in Class_Type
         then
            return Global_Symbol_Table.Element (Type_Id).SymType;
         else
            return Report_Unhandled_Node_Type (E, "Do_Type_Reference",
                                               "Type of type not a type");
         end if;
      else
         return Make_Symbol_Type (Type_Name);
      end if;
   end Do_Type_Reference;

   -------------------------
   -- Do_Withed_Unit_Spec --
   -------------------------

   procedure Do_Withed_Unit_Spec (N : Node_Id) is
   begin
      if Defining_Entity (N) = Stand.Standard_Standard then
         --  TODO: github issue #252
         --  At the moment Standard is not processed
         null;
      else
         --  Handle all other withed library unit declarations
         case Nkind (N) is
            when N_Subprogram_Body =>
               if Acts_As_Spec (N) then
                  --  The unit is a withed library unit which subprogram body
                  --  that has no separate declaration, or,
                  --  it is the subprogram body of the compilation unit being
                  --  compiled and it has no separate declaration.
                  --  Obtain the subprogram specification from the body
                  --  and insert it into the symbol table.
                  Register_Subprogram_Specification (Specification (N));
               else
                  null;
               end if;
            when N_Subprogram_Declaration =>
               --  The unit is withed library unit that is a subprogram
               --  declaration, or,
               --  it is the declaration of the compilation unit body being
               --  compiled.
               --  Do_Subprogram_Declaration enters the specification of the
               --  subprogram into the symbol table.
               Do_Subprogram_Declaration (N);
            when N_Package_Declaration =>
               Do_Package_Declaration (N);
            when N_Package_Body =>
               null;
            when N_Generic_Subprogram_Declaration
              | N_Generic_Package_Declaration =>
               --  no special handling for generics is required
               --  because their instantiations appear as nodes in the AST
               null;
            when others =>
               Report_Unhandled_Node_Empty
                 (N, "Do_Withed_Unit_Spec",
                  "This type of library_unit is not yet handled");
         end case;

      end if;

   end Do_Withed_Unit_Spec;

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
         Append (Variant_Name,
                 "_" & Get_Name_String (Chars (Constraint_Iter)));
         Next (Constraint_Iter);
      end loop;
      return To_String (Variant_Name);
   end Get_Variant_Union_Member_Name;

   --------------------
   -- Make_Increment --
   --------------------

   function Make_Increment
     (Sym : Irep; Sym_Type : Node_Id; Amount : Integer) return Irep
   is
      Amount_Expr : constant Irep :=
        Make_Integer_Constant (Amount, Sym_Type);
      Plus : constant Irep := Make_Op_Add
        (Lhs => Sym,
         Rhs => Amount_Expr,
         I_Type => Get_Type (Sym),
         Source_Location => Internal_Source_Location);
   begin
      return Make_Side_Effect_Expr_Assign
        (Lhs => Sym,
         Rhs => Plus,
         Source_Location => Internal_Source_Location,
         I_Type => Get_Type (Sym));
   end Make_Increment;

   ---------------------------
   -- Make_Integer_Constant --
   ---------------------------

   function Make_Integer_Constant (Val : Integer; Ty : Node_Id) return Irep is
      Type_Width : constant Int := UI_To_Int (Esize (Ty));
      Val_Binary : constant String := Convert_Int_To_Binary (Val, Type_Width);
   begin
      return Make_Constant_Expr
        (Value => Val_Binary,
         I_Type => Do_Type_Reference (Ty),
         Source_Location => Internal_Source_Location);
   end Make_Integer_Constant;

   ------------------------
   -- Make_Runtime_Check --
   ------------------------

   function Make_Runtime_Check (Condition : Irep) return Irep
   is
   begin

      if Check_Function_Symbol = Ireps.Empty then
         --  Create the check function on demand:
         declare
            Formal_Params : constant Irep := Make_Parameter_List;
            Fn_Type : constant Irep := Make_Code_Type
              (Parameters => Formal_Params,
               Return_Type => CProver_Void_T);
            Fn_Name : constant String := "__ada_runtime_check";
            Formal_Param : constant Irep := Make_Code_Parameter
              (Identifier => Fn_Name & "::arg",
               Base_Name => "arg",
               I_Type => CProver_Bool_T,
               Default_Value => Ireps.Empty,
               This => False,
               Source_Location => Internal_Source_Location);
            Formal_Expr : constant Irep := Make_Symbol_Expr
              (Identifier => Get_Identifier (Formal_Param),
               I_Type => Get_Type (Formal_Param),
               Source_Location => Get_Source_Location (Formal_Param));
            Assertion : constant Irep := Make_Code_Assert
              (Assertion => Formal_Expr,
               Source_Location => Internal_Source_Location);
            Fn_Symbol : constant Symbol := New_Function_Symbol_Entry
              (Name          => Fn_Name,
               Symbol_Type   => Fn_Type,
               Value         => Assertion,
               A_Symbol_Table => Global_Symbol_Table);
         begin
            Append_Parameter (Formal_Params, Formal_Param);
            Check_Function_Symbol := Symbol_Expr (Fn_Symbol);
         end;
      end if;

      declare
         Call_Args : constant Irep := Make_Argument_List;
      begin
         Append_Argument (Call_Args, Condition);
         return Make_Side_Effect_Expr_Function_Call
           (I_Function => Check_Function_Symbol,
            I_Type => CProver_Void_T,
            Arguments => Call_Args,
            Source_Location => Get_Source_Location (Condition));
      end;
   end Make_Runtime_Check;

   -----------------------------
   --  Make_Struct_Component  --
   -----------------------------

   function Make_Struct_Component (Name : String; Ty : Irep) return Irep is
   begin
      return Make_Struct_Union_Component
        (Source_Location => Internal_Source_Location,
         Is_Padding      => False,
         I_Access        => "public",
         I_Type          => Ty,
         Range_Check     => False,
         Anonymous       => False,
         Prettyname      => Name,
         Name            => Name,
         Basename        => Name);
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

   --------------------------
   -- Process_Declaration --
   --------------------------

   procedure Process_Declaration (N : Node_Id; Block : Irep) is
      procedure Handle_Representation_Clause (N : Node_Id);
      procedure Handle_Representation_Clause (N : Node_Id) is
         Attr_Id : constant String := Get_Name_String (Chars (N));
      begin
         --  First check if it is an address clause which gnat2goto does not
         --  currently handle
         if Attr_Id = "address" then
            Report_Unhandled_Node_Empty
              (N, "Process_Declaration",
               "Address representation clauses are not currently supported");
            return;
         elsif Attr_Id = "size" or else Attr_Id = "component_size" then
            declare
               Target_Name : constant Irep := Do_Identifier (Name (N));
               Entity_Esize : constant Uint := Esize (Entity (N));
               Target_Type_Irep : constant Irep :=
                 Follow_Symbol_Type
                   (Get_Type (Target_Name), Global_Symbol_Table);
               Expression_Value : constant Uint := Expr_Value (Expression (N));
            begin
               pragma Assert (Kind (Target_Type_Irep) in Class_Type);
               if Attr_Id = "size" then

                  --  Just check that the front-end already applied this size
                  --  clause, i .e. that the size of type-irep we already had
                  --  equals the entity type this clause is applied to (and the
                  --  size specified in this clause).
                  if Entity_Esize /=
                       UI_From_Int (Int (Get_Width (Target_Type_Irep)))
                    or Entity_Esize /= Expression_Value
                  then
                     Report_Unhandled_Node_Empty
                       (N, "Process_Declaration",
                        "size clause not applied by the front-end");
                  end if;
                  return;
               elsif Attr_Id = "component_size" then
                  if not Is_Array_Type (Entity (N)) then
                     Report_Unhandled_Node_Empty
                       (N, "Process_Declaration",
                        "Component size only supported for array types");
                     return;
                  end if;
                  declare
                     Array_Data : constant Irep :=
                       Get_Data_Component_From_Type (Target_Type_Irep);
                     Target_Subtype : constant Irep :=
                       Follow_Symbol_Type (Get_Subtype (Get_Type (Array_Data)),
                                           Global_Symbol_Table);
                     Target_Subtype_Width : constant Uint :=
                       UI_From_Int (Int (Get_Width (Target_Subtype)));
                  begin
                     if Component_Size (Entity (N)) /= Expression_Value or
                       Target_Subtype_Width /= Expression_Value
                     then
                        Report_Unhandled_Node_Empty
                          (N, "Process_Declaration",
                           "Having component sizes be different from the "
                           & "size of their underlying type "
                           & "is currently not supported");
                     end if;
                  end;
                  return;
               end if;
            end;
         elsif Attr_Id = "alignment" then
            --  ASVAT does not model alignment of objects in memory.
            --  Nothing to be done.
            return;
         end if;

         Report_Unhandled_Node_Empty
           (N, "Process_Declaration",
            "Representation clause unsupported: " & Attr_Id);

      end Handle_Representation_Clause;

   begin
      --  Deal with the declaration

      case Nkind (N) is

         --  basic_declarations  --

         when N_Full_Type_Declaration =>
            Do_Full_Type_Declaration (N);

         when N_Incomplete_Type_Declaration =>
            Do_Incomplete_Type_Declaration (N);

         when N_Private_Type_Declaration =>
            Do_Private_Type_Declaration (N);

         when N_Subtype_Declaration =>
            Do_Subtype_Declaration (N);

         when N_Object_Declaration =>
            Do_Object_Declaration (N, Block);

         when N_Number_Declaration =>
            --  A number declaration is replaced by its static initialisation
            --  expression.
            --  No action is required at the point of it's declaration;
            null;

         when N_Subprogram_Declaration =>
            Do_Subprogram_Declaration (N);

         when N_Abstract_Subprogram_Declaration =>
            --  Ignored : Support is not necessary to capture the executable
            --  semantics of the program.
            null;
         when N_Package_Declaration =>
            Do_Package_Declaration (N);

         when N_Renaming_Declaration =>
            --  renaming declarations are handled by the gnat front-end;
            null;

         when N_Exception_Declaration => Do_Exception_Declaration (N);

         when N_Generic_Declaration =>
            null;

         when N_Generic_Instantiation =>
            null;

            --  basic_declarative_items  --

         when N_Representation_Clause =>
            Handle_Representation_Clause (N);

         when N_Use_Package_Clause =>
            --  do nothing, name resolution is done by frontend
            null;

         when N_Use_Type_Clause =>
            Report_Unhandled_Node_Empty (N, "Process_Declaration",
                                         "Use type clause declaration");

         --  remaining declarative items  --

            --  proper_body  --

         when N_Subprogram_Body =>
            Do_Subprogram_Body (N);

         when N_Package_Body =>
            --  XXX: Need to add instructions for initialisers of subpackage
            if Ekind (Corresponding_Spec (N)) /= E_Generic_Package then
               declare
                  Unused_Node_Result : constant Irep
                    := Do_Subprogram_Or_Block (N);
               begin
                  pragma Unreferenced (Unused_Node_Result);
               end;
            end if;
         when N_Task_Body =>
            Report_Unhandled_Node_Empty (N, "Process_Declaration",
                                         "Task body declaration");

         when N_Protected_Body =>
            Report_Unhandled_Node_Empty (N, "Process_Declaration",
                                         "Protected body declaration");

            --  body_stub  --

         when N_Subprogram_Body_Stub =>
            Do_Subprogram_Body_Stub (N);

         when N_Package_Body_Stub =>
            Report_Unhandled_Node_Empty (N, "Process_Declaration",
                                         "Package body stub declaration");

         when N_Task_Body_Stub =>
            Report_Unhandled_Node_Empty (N, "Process_Declaration",
                                         "Task body stub declaration");

         when N_Protected_Body_Stub =>
            Report_Unhandled_Node_Empty (N, "Process_Declaration",
                                         "Protected body stub declaration");

         --  Pragmas may appear in declarations  --

         when N_Pragma =>
            Process_Pragma_Declaration (N);

            --  Every code lable is implicitly declared in  --
            --  the closest surrounding block               --

         when N_Implicit_Label_Declaration =>
            --  Ignore for now, as I guess an implicit label can't be
            --  referenced.
            --  Yes it can: this is the declaration of the name it appears
            --  the declaritve section but is used on a statement.
            null;

         -- Not sure the nex two should be here --
         when N_Itype_Reference =>
            Do_Itype_Reference (N);

         when N_Freeze_Entity |
              N_Freeze_Generic_Entity =>
            --  Ignore: not relevant to the kind of analysis we are doing or
            --  the phase of compilation and building that we are working on.
            null;

         when N_Null_Statement =>
            if Present (Original_Node (N)) then
               --  is the result of rewritting -> can be ignored
               null;
            else
               Report_Unhandled_Node_Empty (N, "Process_Declaration",
                                            "Unsupported null statement");
            end if;
         when others =>
            Report_Unhandled_Node_Empty (N, "Process_Declaration",
                                         "Unknown declaration kind");

      end case;

   end Process_Declaration;

   procedure Process_Pragma_Declaration (N : Node_Id) is
      procedure Handle_Pragma_Volatile (N : Node_Id);
      procedure Handle_Pragma_Machine_Attribute (N : Node_Id)
        with Pre => Nkind (N) in N_Pragma
        and then Pragma_Name (N) = Name_Machine_Attribute;

      procedure Handle_Pragma_Volatile (N : Node_Id) is
         Argument_Associations : constant List_Id :=
           Pragma_Argument_Associations (N);
         First_Argument_Expression : constant Node_Id :=
           Expression (First (Argument_Associations));
         Expression_Id : constant Symbol_Id :=
           Intern (Unique_Name (Entity (First_Argument_Expression)));

         procedure Set_Volatile (Key : Symbol_Id; Element : in out Symbol);
         procedure Set_Volatile (Key : Symbol_Id; Element : in out Symbol) is
         begin
            pragma Assert (Unintern (Key) = Unintern (Expression_Id));
            Element.IsVolatile := True;
         end Set_Volatile;
      begin
         pragma Assert (Global_Symbol_Table.Contains (Expression_Id));
         Global_Symbol_Table.Update_Element (
                          Position => Global_Symbol_Table.Find (Expression_Id),
                          Process  => Set_Volatile'Access);
      end Handle_Pragma_Volatile;

      procedure Handle_Pragma_Machine_Attribute (N : Node_Id) is
         Argument_Associations : constant List_Id :=
           Pragma_Argument_Associations (N);

         --  first is the identifier to be given the attribute
         First_Argument : constant Node_Id := First (Argument_Associations);

         --  second is the attribute as string
         Second_Argument : constant Node_Id := Next (First_Argument);
         Attr_String_Id : constant String_Id :=
           Strval (Expression (Second_Argument));
         Attr_Length : constant Integer :=
           Integer (String_Length (Attr_String_Id));
      begin
         String_To_Name_Buffer (Attr_String_Id);
         declare
            Attr_String : String
              renames Name_Buffer (1 .. Attr_Length);
         begin
            if Attr_String = "signal" then
            --  CBMC would not acknowledge this one anyway -> Ignored
               null;
            else
               Report_Unhandled_Node_Empty
                 (N, "Process_Pragma_Declaration",
                  "Unsupported pragma: Machine Attribute "
                  & Attr_String);
            end if;
         end;
      end Handle_Pragma_Machine_Attribute;

   begin
      case Pragma_Name (N) is
         when Name_Assert |
              Name_Assume |
              Name_Assert_And_Cut |
            --  Assert and introduce a cut point: the prover can safely forget
            --  evaluations of local variables and only assume the asserted
            --  condition. This could be used in symex (making it concolic)
            --  but is only an optimization.
            Name_Loop_Invariant =>
            --  Equivalent to assert but also introduces a cut point wrt. the
            --  variables local to the loop.
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Assert/Assume");
         when Name_Precondition =>
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Precondition");
         when Name_Postcondition =>
            --  Postcondition will eventually also be translated into
            --  assertions but they should hold elsewhere from where they are
            --  defined and they refer to 'Result variables
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Postcondition");
         when Name_Refined_State |
              Name_Refined_Global |
              Name_Refined_Depends =>
            --  We are not supporting refinement at this point
            --  Using it would (probably) require modification to CBMC
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Refine");
         when Name_Global =>
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Global");
         when Name_Variant =>
            --  Could as well be ignored but is another verification condition
            --  that should be checked
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Variant");
         when Name_Asynchronous =>
            --  Allows a remote subprogram call to return prior to completion
            --  of the execution of the corresponding remote subprogram body.
            --  It changes the semantics wrt to thread interleavings.
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Asynchronous");
         when Name_Atomic |
              Name_Atomic_Components =>
            --  For an atomic object all reads and updates of the object as a
            --  whole are indivisible. It changes the semantics wrt to thread
            --  interleavings.
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Atomic");
         when Name_Volatile |
              Name_Volatile_Components =>
            --  For a volatile object all reads and updates of the object as a
            --  whole are performed directly to memory. In sequential execution
            --  they may be modified by the environment. Effectively, they need
            --  to be modelled as non-deterministic input in every state. It
            --  changes the semantics wrt to thread interleavings.
            Handle_Pragma_Volatile (N);
         when Name_Attach_Handler =>
            --  The expression in the Attach_Handler pragma as evaluated at
            --  object creation time specifies an interrupt. As part of the
            --  initialization of that object, if the Attach_Handler pragma is
            --  specified, the handler procedure is attached to the specified
            --  interrupt. A check is made that the corresponding interrupt is
            --  not reserved. We do not support that check yet.
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Attach Handler");
         when Name_Import =>
            --  Used to import an entity defined in a foreign language into an
            --  Ada program, thus allowing a foreign-language subprogram to
            --  be called from Ada, or a foreign-language variable to be
            --  accessed from Ada. This would (probably) require gnat2goto to
            --  understand the foreign code, which we do not at the moment.
            --  However, if the calling convention is specified as "Intrinsic"
            --  then the subprogram is built into the compiler and gnat2goto
            --  can safely ignore the pragma.
            declare
               --  If the pragma is specified with positional parameter
               --  association, then the calling convention is the first
               --  parameter. Check to see if it is Intrinsic.
               Next_Ass : Node_Id := First (Pragma_Argument_Associations (N));
               Is_Intrinsic : Boolean := Present (Next_Ass) and then
                 Nkind (Expression (Next_Ass)) = N_Identifier and then
                 Get_Name_String (Chars (Expression (Next_Ass))) = "intrinsic";
            begin
               --  If the first parameter is not Intrinsic, check named
               --  parameters for calling convention
               while not Is_Intrinsic and Present (Next_Ass) loop
                  if Chars (Next_Ass) /= No_Name and then
                    Get_Name_String (Chars (Next_Ass)) = "convention"
                  then
                     --  The named parameter is Convention, check to see if it
                     --  is Intrinsic
                     Is_Intrinsic :=
                       Get_Name_String (Chars (Expression (Next_Ass))) =
                       "intrinsic";
                  end if;
                     --  Get the next parameter association
                  Next_Ass := Next (Next_Ass);
               end loop;

               if not Is_Intrinsic then
                  Put_Line (Standard_Error,
                            "Warning: Multi-language analysis unsupported.");
               end if;
            end;

         when Name_Elaborate =>
            --  Specifies that the body of the named library unit is elaborated
            --  before the current library_item. We will support packages.
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Elaborate");
         when Name_Elaborate_All =>
            --  Specifies that each library_item that is needed by the named
            --  library unit declaration is elaborated before the current
            --  library_item. Same reason for future support as above.
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Elaborate All");
         when Name_Locking_Policy =>
            --  Specifies whether or not protected objects have priorities, and
            --  the relationships between these priorities and task priorities.
            --  This may change thread interleaving.
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Locking Policy");
         when Name_Normalize_Scalars =>
            --  Ensures that an otherwise uninitialized scalar object is set to
            --  a predictable value, but out of range if possible. This
            --  obviously changes the behaviour.
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                      "Unsupported pragma: Normalize Scalars");
         when Name_Queuing_Policy =>
            --  Governs the order in which tasks are queued for entry
            --  service, and the order in which different entry queues are
            --  considered for service. This may change the behaviour.
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Queuing Policy");
         when Name_Remote_Types =>
            --  Defines types intended for use in communication between active
            --  partitions. Concurrency may be supported in the future.
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Remote Types");
         when Name_Restrictions =>
            --  Expresses the user's intent to abide by certain restrictions.
            --  This could probably be implemented as an assertion eventually.
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Restrictions");
         when Name_Shared_Passive =>
            --  Used for managing global data shared between active partitions.
            --  Concurrency may be supported in the future.
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Shared Passive");
         when Name_Task_Dispatching_Policy =>
            --  Specifies the details of task dispatching that are not covered
            --  by the basic task dispatching model. Concurrency may be
            --  supported in the future.
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                "Unsupported pragma: Task Dispatching Policy");
         when Name_All_Calls_Remote |
              Name_Remote_Call_Interface =>
            --  Library unit pragma; used by the distributed systems annex
            --  Interface for remote function calls between active partitions
            --  Should not alter the semantics, but we want to know about it.
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                  "Known but unsupported pragma: Remote Call");
         when Name_Interrupt_Handler =>
            --  If the pragma appears in a protected_definition, then the
            --  corresponding procedure can be attached dynamically, as a
            --  handler, to interrupts. We want to detect interrupts early.
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                            "Known but unsupported pragma: Interrupt Handler");
         when Name_Controlled =>
            --  Used to prevent any automatic reclamation of storage (garbage
            --  collection) for the objects created by allocators of a given
            --  access type. Resource allocation problem must be detected.
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                   "Known but unsupported pragma: Controlled");
         when Name_Export =>
            --  Used to export an Ada entity to a foreign language, thus
            --  allowing an Ada subprogram to be called from a foreign
            --  language, or an Ada object to be accessed from a foreign
            --  language. Need to be detected.
            Put_Line (Standard_Error,
                      "Warning: Multi-language analysis unsupported.");
         when Name_Machine_Attribute =>
            Handle_Pragma_Machine_Attribute (N);
         when Name_Check =>
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Check");
         when Name_Effective_Writes =>
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                       "Unsupported pragma: Effective writes");
         when Name_Async_Readers =>
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Async readers");
         when Name_No_Return =>
            --  Can be detected when processing the function body
            null;

         when Name_Suppress_Initialization =>
            --  pragma Suppress_Initialization can be ignored if it is
            --  appied to an array or scalar type which do not have a
            --  default value aspect applied.
            --  If these conditions are not met an unsupported pragma is
            --  reported.
            declare
               Arg : constant Node_Id :=
                 First (Pragma_Argument_Associations (N));
               E   : constant Entity_Id := Entity
                 (if Present (Arg) and then
                  Nkind (Arg) = N_Pragma_Argument_Association
                  then
                     Expression (Arg)
                  else
                     Arg);
            begin
               if not ((Is_Array_Type (E) and then
                          not Present (Default_Aspect_Component_Value (E)))
                        or else
                        (Is_Scalar_Type (E) and then
                             not Present (Default_Aspect_Value (E))))
               then
                  Report_Unhandled_Node_Empty
                    (N, "Process_Pragma_Declaration",
                     "Unsupported pragma: Suppress initialization");
               end if;
            end;
         when Name_Obsolescent =>
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Obsolescent");
         when Name_Initializes =>
            Report_Unhandled_Node_Empty (N, "Process_Pragma_Declaration",
                                         "Unsupported pragma: Initializes");
         when Name_Annotate |
            --  Ignore here. Rather look for those when we process a node.
              Name_Assertion_Policy | Name_Check_Policy |
            --  Control the pragma Assert according to the policy identifier
            --  which can be Check, Ignore, or implementation-defined.
            --  Ignore means that assertions are ignored at run-time -> Ignored
              Name_Compile_Time_Warning |
            --  Used to issue a compile time warning from the compiler
            --  front-end.  The warning will be issued by the front-end but has
            --  no affect on the AST.  It can be ignored safely by gnat2goto.
              Name_Discard_Names |
            --  Used to request a reduction in storage used for the names of
            --  certain entities. -> Ignored
              Name_Inspection_Point |
            --  Identifies a set of objects each of whose values is to be
            --  available at the point(s) during program execution
            --  corresponding to the position of the pragma in the compilation
            --  unit. -> Ignored
              Name_Linker_Options |
            --  Used to specify the system linker parameters needed when a
            --  given compilation unit is included in a partition. We want to
            --  know that code manipulates the linking.Name_Linker_Options =>
            --  Used to specify the system linker parameters needed when a
            --  given compilation unit is included in a partition. The
            --  goto functions produced by gnat2goto are linked by symtab2gb.
            --  Currently there very few options for this linker and none that
            --  apply to most linkers.  Currently  the pragma can ignored,
            --  but in the future, if symtab2gb was to take more options
            --  this pragma could be reinstated.
              Name_List |
            --  Takes one of the identifiers On or Off as the single
            --  argument. It specifies that listing of the compilation is to be
            --  continued or suspended until a List pragma with the opposite
            --  argument is given within the same compilation. -> Ignored
              Name_Page |
            --  Specifies that the program text which follows the pragma should
            --  start on a new page (if the compiler is currently producing a
            --  listing). -> Ignored
              Name_Optimize |
            --  Gives advice to the implementation as to whether time or space
            --  is the primary optimization criterion. -> Ignored
              Name_Pack |
            --  Specifies that storage minimization should be the main
            --  criterion when selecting the representation of a composite
            --  type. -> Ignored
              Name_Pure |
            --  Used to declare that a library unit is pure: does not contain
            --  declaration of any variable or named access type. -> Ignored
              Name_Reviewable |
            --  Directs the implementation to provide information to facilitate
            --  analysis and review of a program's object code. -> Ignored
              Name_Storage_Size |
            --  Specifies the amount of storage to be reserved for the
            --  execution of a task. -> Ignored
              Name_Unsuppress |
            --  Voids the supressing request. -> Ignored
              Name_Convention |
            --  Used to specify that an Ada entity should use the conventions
            --  of another language. It is intended primarily for types and
            --  callback subprograms. -> Ignored
              Name_Inline |
              Name_Inline_Always |
              Name_Inline_Generic |
            --  Indicates that inline expansion is desired for all calls to
            --  that entity. -> Ignored
              Name_Pure_Function |
            --  Optimisation control, can be ignored as it is not actually
            --  checked
              Name_Preelaborate |
            --  If a library unit is preelaborated, then its declaration, if
            --  any, and body, if any, are elaborated prior to all
            --  non-preelaborated library_item s of the partition. -> Ignored
              Name_Suppress |
            --  Suppressing is effectively also ignored (elaborated as example)
              Name_SPARK_Mode |
            --  Ignored for now
              Name_No_Elaboration_Code_All |
            --  Only affects elaboration and linking so can be ignored for now
              Name_Universal_Aliasing |
            --  Optimisation control, should be ignored
              Name_Implementation_Defined |
            --  Only informs the compiler that entities are implementation
            --  defined. -> Ignored
              Name_Preelaborable_Initialization |
            --  Same as the above preelaborations.
              Name_Warnings |
            --  Ignoring pragma warnings means that all warnings are on.
              Name_Abstract_State |
              Name_Ada_05 |
              Name_Ada_2012 |
              Name_Elaborate_Body |
              Name_No_Strict_Aliasing |
              Name_Unreferenced =>
            --  The above are ignored because they are not relevant to the kind
            --  of analysis we are doing or the phase of compilation and
            --  building that we are working on.
            null;
         when others =>
            declare
               Unknown_Pragma_Diagnostic : constant String :=
                 (if Pragma_Name (N) /= No_Name
                  then Get_Name_String (Pragma_Name (N))
                  else "<No Name>");
            begin
               Report_Unhandled_Node_Empty
                 (N,
                  "Process_Pragma_Declaration",
                  "Unknown pragma: " & Unknown_Pragma_Diagnostic);
            end;
      end case;
   end Process_Pragma_Declaration;

   --------------------------
   -- Process_Declarations --
   --------------------------

   procedure Process_Declarations (L : List_Id; Block : Irep) is
      Decl : Node_Id := First (L);
   begin
      while Present (Decl) loop
         Process_Declaration (Decl, Block);
         Next (Decl);
      end loop;

   end Process_Declarations;

   -------------------------
   --  Process_Statement  --
   -------------------------

   procedure Process_Statement (N : Node_Id; Block : Irep) is
   begin
      --  Deal with the statement
      case Nkind (N) is
         -- Simple statements --
         when N_Null_Statement =>
            null;

         when N_Assignment_Statement =>
            Append_Op (Block, Do_Assignment_Statement (N));

         when N_Exit_Statement =>
            Append_Op (Block, Do_Exit_Statement (N));

         when N_Goto_Statement =>
            Report_Unhandled_Node_Empty (N, "Process_Statement",
                                         "Goto statement");

         when N_Procedure_Call_Statement =>
            Append_Op (Block, Do_Procedure_Call_Statement (N));

         when N_Simple_Return_Statement =>
            if No_Return (Return_Applies_To (Return_Statement_Entity (N)))
            then
               Append_Op (Block, Get_No_Return_Check);
            end if;
            Append_Op (Block, Do_Simple_Return_Statement (N));

         when N_Entry_Call_Statement =>
            Report_Unhandled_Node_Empty (N, "Process_Statement",
                                         "Entry call statement");

         when N_Requeue_Statement =>
            Report_Unhandled_Node_Empty (N, "Process_Statement",
                                         "Requeue statement");

         when N_Delay_Statement =>
            Report_Unhandled_Node_Empty (N, "Process_Statement",
                                         "Delay statement");

         when N_Abort_Statement =>
            Report_Unhandled_Node_Empty (N, "Process_Statement",
                                         "Abort statement");

         when N_Raise_Statement =>
            Append_Op (Block, Do_Raise_Statement (N));

         when N_Code_Statement =>
            Report_Unhandled_Node_Empty (N, "Process_Statement",
                                         "Code statement");

         --  Compound statements

         when N_If_Statement =>
            Append_Op (Block, Do_If_Statement (N));

         when N_Case_Statement =>
            Append_Op (Block, Do_Case_Statement (N));

         when N_Loop_Statement =>
            Append_Op (Block, Do_Loop_Statement (N));

         when N_Block_Statement =>
            Append_Op (Block, Do_N_Block_Statement (N));

         when N_Handled_Sequence_Of_Statements =>  -- this seems incorrct
            --  It should be block_statement
            Append_Op (Block, Do_Handled_Sequence_Of_Statements (N));

         when N_Extended_Return_Statement =>
            Report_Unhandled_Node_Empty (N, "Process_Statement",
                                         "Extended return statement");

         when N_Accept_Statement =>
            Report_Unhandled_Node_Empty (N, "Process_Statement",
                                         "Accept statement");

            -- Select statements --

         when N_Selective_Accept =>
            Report_Unhandled_Node_Empty (N, "Process_Statement",
                                         "Selective Accept");

         when N_Timed_Entry_Call =>
            Report_Unhandled_Node_Empty (N, "Process_Statement",
                                         "Timed entry call");

         when N_Conditional_Entry_Call =>
            Report_Unhandled_Node_Empty (N, "Process_Statement",
                                         "Conditional entry call");

         when N_Asynchronous_Select =>
            Report_Unhandled_Node_Empty (N, "Process_Statement",
                                         "Asynchronous select");

         -- Pragmas may placed in sequences of statements --

         when N_Pragma =>
            Do_Pragma (N, Block);

         --  Not sure the nex two should be here -
         --  should they be in declarations? --
--         when N_Itype_Reference =>
--            Do_Itype_Reference (N);

--         when N_Freeze_Entity =>
--            --  Ignore, nothing to generate
--            null;
         when N_Object_Declaration =>
            Do_Object_Declaration (N, Block);

         when others =>
            Report_Unhandled_Node_Empty (N, "Process_Statement",
                                         "Unknown expression kind");

      end case;
   end Process_Statement;

   ------------------------
   -- Process_Statements --
   ------------------------

   function Process_Statements (L : List_Id) return Irep is
      Stmt : Node_Id := First (L);
      Reps : constant Irep := Make_Code_Block
        (Source_Location => Get_Source_Location (Stmt));
      package IO renames Ada.Text_IO;
   begin
      while Present (Stmt) loop
         begin
            Process_Statement (Stmt, Reps);
         exception
            when Error : others =>
               IO.Put_Line (IO.Standard_Error, "<========================>");
               IO.Put_Line (IO.Standard_Error,
                            Ada.Exceptions.Exception_Information
                              (Error));
               if GNAT2GOTO.Options.Dump_Statement_AST_On_Error then
                  Treepr.Print_Node_Subtree (Stmt);
               end if;
               IO.Put_Line (IO.Standard_Error, "<========================>");
         end;
         Next (Stmt);
      end loop;

      return Reps;
   end Process_Statements;

   function Do_Modular_Type_Definition (N : Node_Id) return Irep is
      Mod_Max : constant Uint := Intval (Expression (N));
      --  We start at 1, not 0, because our bitvectors
      --  can't be smaller than 1 bit
      Mod_Max_Binary_Logarithm : Integer := 1;
      Power_Of_Two : Uint := Uint_2;
      Ada_Type_Size : constant Integer :=
        Integer (UI_To_Int (Esize (Defining_Identifier (Parent (N)))));
   begin
      while Power_Of_Two < Mod_Max loop
         Mod_Max_Binary_Logarithm := Mod_Max_Binary_Logarithm + 1;
         Power_Of_Two := Power_Of_Two * 2;
      end loop;
      --  If the max value is 2^w (for w > 0) then we can just
      --  use an unsignedbv of width w
      if Mod_Max = Power_Of_Two and Ada_Type_Size = Mod_Max_Binary_Logarithm
      then
         return Make_Unsignedbv_Type (Width => Mod_Max_Binary_Logarithm);
      end if;

      return Make_Ada_Mod_Type
        (Width => Ada_Type_Size,
         Ada_Mod_Max => Convert_Uint_To_Hex
           (Mod_Max, Pos (Ada_Type_Size)));
   end Do_Modular_Type_Definition;

   ---------------------------------------
   -- Register_Subprogram_Specification --
   ---------------------------------------

   procedure Register_Subprogram_Specification (N : Node_Id) is
      Subprog_Type : constant Irep :=
        Do_Subprogram_Specification (N);
      Subprog_Defining_Unit_Name : constant Node_Id := Defining_Unit_Name (N);
      Subprog_Defining_Entity : constant Node_Id := (
        if Nkind (Subprog_Defining_Unit_Name) = N_Defining_Program_Unit_Name
        then Defining_Identifier (Subprog_Defining_Unit_Name)
        else Subprog_Defining_Unit_Name);
      Subprog_Name : constant Symbol_Id :=
        Intern (Unique_Name (Subprog_Defining_Entity));
   begin
      New_Subprogram_Symbol_Entry (Subprog_Name   => Subprog_Name,
                                   Subprog_Type   => Subprog_Type,
                                   A_Symbol_Table => Global_Symbol_Table);
   end Register_Subprogram_Specification;

   -------------------------------
   -- Register_Type_Declaration --
   -------------------------------

   procedure Register_Type_Declaration (N : Node_Id; E : Entity_Id) is
      New_Type : constant Irep :=
        Do_Type_Definition (Type_Definition (N),
                            Discriminant_Specifications (N));
   begin
      pragma Assert (Kind (New_Type) in Class_Type);
      Do_Type_Declaration (New_Type, E);

      --  Declare the implicit initial subtype too
      if Etype (E) /= E then
         Do_Type_Declaration (New_Type, Etype (E));
      end if;
   end Register_Type_Declaration;

   procedure Remove_Entity_Substitution (E : Entity_Id) is
   begin
      Identifier_Substitution_Map.Delete (E);
   end Remove_Entity_Substitution;

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

   function Do_Access_Function_Definition (N : Node_Id) return Irep
   is
      Return_Type : constant Irep :=
        (if Nkind (N) = N_Access_Procedure_Definition
         then CProver_Void_T
         else Do_Type_Reference (Etype (Result_Definition (N))));
      Parameters : constant Irep := Make_Parameter_List;
      Fun_Name : constant String :=
        Unique_Name (Defining_Identifier (Parent (N)));
      A_Parameter : Node_Id := First (Parameter_Specifications (N));
   begin
      while Present (A_Parameter) loop
         declare
            Param_Name : constant String :=
              Unique_Name (Defining_Identifier (A_Parameter));
            Param_Type : constant Irep :=
              Do_Type_Reference (Etype (Parameter_Type (A_Parameter)));
            Param_Irep : constant Irep :=
              Create_Fun_Parameter (Fun_Name        => Fun_Name,
                                    Param_Name      => Param_Name,
                                    Param_Type      => Param_Type,
                                    Param_List      => Parameters,
                                    A_Symbol_Table  => Global_Symbol_Table,
                                   Source_Location => Get_Source_Location (N));
         begin
            pragma Assert (Kind (Param_Irep) = I_Code_Parameter);
         end;
         Next (A_Parameter);
      end loop;
      return Make_Pointer_Type (Make_Code_Type (Parameters  => Parameters,
                                                Ellipsis    => False,
                                                Return_Type => Return_Type,
                                                Inlined     => False,
                                                Knr         => False));
   end Do_Access_Function_Definition;

   function Do_Access_To_Object_Definition (N : Node_Id) return Irep
   is
      Sub_Indication : constant Node_Id := Subtype_Indication (N);
      Under_Type : constant Node_Id := Etype
        (if Nkind (Sub_Indication) = N_Subtype_Indication
         then Subtype_Mark (Sub_Indication)
         else Sub_Indication);
   begin
      return Make_Pointer_Type (Do_Type_Reference (Under_Type));
   end Do_Access_To_Object_Definition;

   function Get_No_Return_Check return Irep is
      No_Return_Check_Symbol : constant Irep := Symbol_Expr
        (Get_Ada_Check_Symbol
           (Name           => "__CPROVER_Ada_Pragma_No_Return",
            A_Symbol_Table => Global_Symbol_Table,
            Source_Loc     => Internal_Source_Location));
      No_Return_Check_Args : constant Irep := Make_Argument_List;
      No_Return_Check_Call : constant Irep := Make_Code_Function_Call
        (Arguments       => No_Return_Check_Args,
         I_Function      => No_Return_Check_Symbol,
         Lhs             => Make_Nil (Internal_Source_Location),
         Source_Location => Internal_Source_Location,
         I_Type          => Make_Void_Type);
   begin
      Append_Argument (No_Return_Check_Args, Get_Int32_T_Zero);
      return No_Return_Check_Call;
   end Get_No_Return_Check;

   function Do_Null_Expression (N : Node_Id) return Irep is
      Pointer_Type : constant Irep := Do_Type_Reference (Etype (N));
   begin
      return Make_Null_Pointer
        (I_Type => Pointer_Type,
         Source_Location => Get_Source_Location (N));
   end Do_Null_Expression;
end Tree_Walk;
