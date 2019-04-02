with Uname;                 use Uname;
with Namet;                 use Namet;
with Nlists;                use Nlists;
with Sem;
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

package body Tree_Walk is

   procedure Add_Entity_Substitution (E : Entity_Id; Subst : Irep);

   procedure Declare_Itype (Ty : Entity_Id);

   function Do_Address_Of (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Attribute_Reference,
        Post => Kind (Do_Address_Of'Result) = I_Address_Of_Expr;

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

   function Do_Constant (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_Integer_Literal,
        Post => Kind (Do_Constant'Result) = I_Constant_Expr;

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

   function Do_Identifier (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Identifier;

   function Do_If_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_If_Statement,
        Post => Kind (Do_If_Statement'Result) = I_Code_Ifthenelse;

   function Do_Exit_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Exit_Statement,
        Post => Kind (Do_Exit_Statement'Result) in Class_Code;

   function Do_Index_Or_Discriminant_Constraint
     (N : Node_Id; Underlying : Irep) return Irep
   with Pre  => Nkind (N) = N_Index_Or_Discriminant_Constraint;

   function Do_Itype_Array_Subtype (N : Entity_Id) return Irep
   with Pre => Is_Itype (N) and then Ekind (N) = E_Array_Subtype;

   function Do_Itype_String_Literal_Subtype (N : Entity_Id) return Irep
   with Pre => Is_Itype (N) and then Ekind (N) = E_String_Literal_Subtype;

   function Do_Itype_Definition (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_Defining_Identifier;

   function Do_Itype_Integer_Subtype (N : Entity_Id) return Irep
   with Pre => Is_Itype (N) and then Ekind (N) = E_Signed_Integer_Subtype;

   function Do_Itype_Integer_Type (N : Entity_Id) return Irep
   with Pre => Is_Itype (N) and then Ekind (N) = E_Signed_Integer_Type;

   function Do_Itype_Record_Subtype (N : Entity_Id) return Irep
   with Pre => Is_Itype (N) and then Ekind (N) = E_Record_Subtype;

   procedure Do_Itype_Reference (N : Node_Id)
   with Pre => Nkind (N) = N_Itype_Reference;

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

   procedure Do_Pragma (N : Node_Id; Block : Irep)
   with Pre => Nkind (N) = N_Pragma
     and then Kind (Block) = I_Code_Block; -- FIXME: what about decls?

   function Do_Operator_Simple (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Op,
        Post => Kind (Do_Operator_Simple'Result) in Class_Expr;

   function Do_Operator_General (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Op;

   function Do_Op_Not (N : Node_Id) return Irep
   with Pre => Nkind (N) in N_Op;

   function Do_Op_Minus (N : Node_Id) return Irep
   with Pre => Nkind (N) in N_Op;

   function Do_Or_Else (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_Or_Else;

   function Do_And_Then (N : Node_Id) return Irep
     with Pre => (Nkind (N) = N_And_Then);

   type Bit_Operand_Constructor is
     access function (Lhs : Irep;
                      Rhs : Irep;
                      Source_Location : Source_Ptr;
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

   function Do_Procedure_Call_Statement (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Procedure_Call_Statement,
        Post => Kind (Do_Procedure_Call_Statement'Result) =
                  I_Code_Function_Call;

   function Do_Range_Constraint (N : Node_Id; Underlying : Irep) return Irep;

   function Do_Record_Definition (N : Node_Id; Discs : List_Id) return Irep
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

   function Do_Floating_Point_Definition (N : Node_Id) return Irep
     with Pre  => (Nkind (N) = N_Floating_Point_Definition
                   and then Is_Type (Defining_Entity (Parent (N)))),
     Post => Kind (Do_Floating_Point_Definition'Result) in
     I_Floatbv_Type | I_Bounded_Floatbv_Type;

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

   function Make_Type_Symbol (Name : Symbol_Id; Defn : Irep) return Symbol;

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

   type Construct is (Declaration, Statement);

   procedure Warn_Unhandled_Construct (C : Construct; Mess : String);

   procedure Process_Declaration (N : Node_Id; Block : Irep);
--     with Pre => Nkind (N) in N_Declaration or else
--                 Nkind (N) in N_Number_Declaration or else
--                 Nkind (N) in N_Later_Decl_Item or else
--                 Nkind (N) in N_Pragma or else
--                 Nkind (N) in N_Exception_Declaration or else
--                 Nkind (N) in N_Freeze_Entity;
--  Precondition commented out because full extend of declrations not yet known
   --  Handles both a basic declaration and a declarative item.

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

   procedure Remove_Entity_Substitution (E : Entity_Id);

   function Create_Dummy_Irep return Irep;

   function Make_Malloc_Function_Call_Expr (Num_Elem : Irep;
                                            Element_Type_Size : Uint;
                                            Source_Loc : Source_Ptr)
                                            return Irep is
      Size : constant Irep :=
        Compute_Memory_Op_Size (Num_Elem          => Num_Elem,
                            Element_Type_Size => Element_Type_Size,
                            Source_Loc        => Source_Loc);
      Malloc_Args  : constant Irep := New_Irep (I_Argument_List);
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
                                            Source_Loc : Source_Ptr)
                                            return Irep is
      Size : constant Irep :=
        Compute_Memory_Op_Size (Num_Elem          => Num_Elem,
                            Element_Type_Size => Element_Type_Size,
                            Source_Loc        => Source_Loc);
      Memcpy_Args  : constant Irep := New_Irep (I_Argument_List);
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
      Put_Line (Standard_Error, "----------At: " & Fun_Name & "----------");
      Put_Line (Standard_Error, "----------" & Message & "----------");
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
      if Present (Ty) and then Is_Itype (Ty) then
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
      R : constant Irep := New_Irep (I_Address_Of_Expr);
   begin
      if not (Kind (Get_Type (Do_Expression (Prefix (N)))) in Class_Type) then
         Report_Unhandled_Node_Empty (N, "Do_Address_Of",
                                      "Kind not in class type");
         return R;
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
            return Report_Unhandled_Node_Irep (N,
                                               "Do_Aggregate_Literal",
                                               "Unhandled aggregate kind");
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
         Struct_Expr : constant Irep := New_Irep (I_Struct_Expr);
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

         Set_Type (Struct_Expr, Do_Type_Reference (N_Underlying_Type));
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
         return Do_Array_Assignment (N);
      end if;

      declare
         LHS : constant Irep := Do_Expression (Name (N));
         RHS : constant Irep := Do_Expression (Expression (N));
         R : constant Irep := New_Irep (I_Code_Assign);
      begin
         Set_Source_Location (R, Sloc (N));
         Set_Lhs (R, LHS);
         if Do_Range_Check (Expression (N)) then
            declare
               Range_Expr : constant Irep :=
                 Make_Range_Assert_Expr (
                          N          => N,
                          Value      => RHS,
                          Bounds_Type => Get_Type (LHS));
            begin
               Set_Rhs (R,
                        Typecast_If_Necessary (Expr     => Range_Expr,
                                        New_Type => Get_Type (LHS)));
            end;
         else
            Set_Rhs (R, RHS);
         end if;
         return R;
      end;
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
      if not (Kind (Resolved_Underlying) in Class_Bitvector_Type) then
         Report_Unhandled_Node_Empty (Range_Expr, "Do_Base_Range_Constraint",
                                      "range expression not bitvector type");
         return R : constant Irep := New_Irep (I_Bounded_Signedbv_Type);
      end if;
      if Nkind (Low_Bound (Range_Expr)) /= N_Integer_Literal then
         Report_Unhandled_Node_Empty (Range_Expr, "Do_Base_Range_Constraint",
                                     "low bound range expression not literal");
         return R : constant Irep := New_Irep (I_Bounded_Signedbv_Type);
      end if;
      if Nkind (High_Bound (Range_Expr)) /= N_Integer_Literal then
         Report_Unhandled_Node_Empty (Range_Expr, "Do_Base_Range_Constraint",
                                    "high bound range expression not literal");
         return R : constant Irep := New_Irep (I_Bounded_Signedbv_Type);
      end if;
      return R : constant Irep := New_Irep (I_Bounded_Signedbv_Type) do
         Set_Width (R, Get_Width (Resolved_Underlying));
         Set_Lower_Bound (I     => R,
                          Value => Store_Bound (Bound_Type (Intval (
                            Low_Bound (Range_Expr)))));
         Set_Upper_Bound (I     => R,
                          Value => Store_Bound (Bound_Type (Intval (
                            High_Bound (Range_Expr)))));
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
         Actual_Irep   : Irep;

      begin
         if Is_Out and then
           not (Kind (Get_Type (Do_Expression (Actual))) in Class_Type)
         then
            Report_Unhandled_Node_Empty (Actual, "Handle_Parameter",
                                         "Kind of actual not in class type");
            return;
         end if;
         Actual_Irep := Wrap_Argument (Do_Expression (Actual), Is_Out);
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

      return Make_If_Expr (Cond, Else_Expr, Then_Expr, Sloc (N), Expr_Type);
   end Do_If_Expression;

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
            Set_Type (Big_Or, Make_Bool_Type);
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
               if not (Kind (This_Test) in Class_Expr) or else
                 not (Kind (Get_Type (This_Expr)) in Class_Type)
               then
                  Report_Unhandled_Node_Empty (N, "Do_Case_Expression",
                  "Case kind not in class expr or alt expr not in class type");
                  return Ret;
               end if;
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

   function Do_Compilation_Unit (N : Node_Id; Add_Start : out Boolean)
     return Symbol
   is
      U           : constant Node_Id := Unit (N);
      Unit_Symbol : Symbol;
   begin
      --  Insert all all specifications of all withed units including the
      --  specification of the given compilation unit into the symbol table.
      Do_Withed_Units_Specs;

      case Nkind (U) is
         when N_Subprogram_Body =>
            declare
               Unit_Name : constant Symbol_Id :=
                 Intern (Unique_Name (Unique_Defining_Entity (U)));
            begin
               --  The specification of the subprogram body has already
               --  been inserted into the symbol table by the call to
               --  Do_Withed_Unit_Specs.
               pragma Assert (Global_Symbol_Table.Contains (Unit_Name));
               Unit_Symbol := Global_Symbol_Table (Unit_Name);

               --  Now compile the body of the subprogram
               Unit_Symbol.Value := Do_Subprogram_Or_Block (U);

               --  and update the symbol table entry for this subprogram.
               Global_Symbol_Table.Replace (Unit_Name, Unit_Symbol);
               Add_Start := True;
            end;

         when N_Package_Body =>
            declare
               Dummy : constant Irep := Do_Subprogram_Or_Block (U);
               pragma Unreferenced (Dummy);
            begin
               Add_Start := False;
            end;

         when others =>
            Report_Unhandled_Node_Empty (N, "Do_Compilation_Unit",
                                         "Unknown tree node");
      end case;

      return Unit_Symbol;
   end Do_Compilation_Unit;

   -----------------
   -- Do_Constant --
   -----------------

   function Do_Constant (N : Node_Id) return Irep is
      Ret           : constant Irep := New_Irep (I_Constant_Expr);
      Constant_Type : constant Irep := Do_Type_Reference (Etype (N));
      Is_Integer_Literal : constant Boolean :=
        Etype (N) = Stand.Universal_Integer;
      Constant_Resolved_Type : Irep;
      Constant_Width : Integer := 64;
   begin
      -- Dummy value initialisation --
      -- To be removed once the Unsupported reports are removed --
      Set_Source_Location (Ret, Sloc (N));
      Set_Type (Ret, Make_Signedbv_Type (Ireps.Empty, 32));
      Set_Value (Ret, "00000000000000000000000000000000");

      if Is_Integer_Literal then
         Constant_Resolved_Type :=  Constant_Type;
      else
         if not Global_Symbol_Table.Contains (
                                              Intern (Get_Identifier
                                                        (Constant_Type)))
         then
            Report_Unhandled_Node_Empty (N, "Do_Constant",
                                         "Constant Type not in symbol table");
            return Ret;
         end if;
         Constant_Resolved_Type := Follow_Symbol_Type (Constant_Type,
                                                       Global_Symbol_Table);
      end if;

      if Is_Integer_Literal then
         null;
      else
         if Kind (Constant_Resolved_Type) in Class_Bitvector_Type then
            Constant_Width := Get_Width (Constant_Resolved_Type);
         else
            Report_Unhandled_Node_Empty (N, "Do_Constant",
                                  "Constant Type not in Class_Bitvector_Type");
            return Ret;
         end if;
      end if;

      Set_Source_Location (Ret, Sloc (N));
      Set_Type (Ret, Constant_Resolved_Type);
      if not Is_Integer_Literal then
         Set_Value (Ret,
                    Convert_Uint_To_Hex
                      (Intval (N), Pos (Constant_Width)));
      else
         Set_Value (Ret, UI_Image (Input  => Intval (N),
                                   Format => Decimal));
      end if;
      return Ret;
   end Do_Constant;

   ---------------------------
   -- Do_Character_Constant --
   ---------------------------

   function Do_Character_Constant (N : Node_Id) return Irep is
      Ret : constant Irep := New_Irep (I_Constant_Expr);
      Resolved_Type : constant Irep := Do_Type_Reference (Etype (N));
      Character_Size : constant Int := UI_To_Int (Esize (Etype (N)));
   begin
      Set_Type (Ret, Resolved_Type);
      Set_Source_Location (Ret, Sloc (N));
      Set_Value (Ret,
                 Convert_Uint_To_Hex
                   (Char_Literal_Value (N), Pos (Character_Size)));
      return Ret;
   end Do_Character_Constant;

   ------------------------
   -- Do_String_Constant --
   ------------------------

   function Do_String_Constant (N : Node_Id) return Irep is
      Ret              : constant Irep := New_Irep (I_String_Constant_Expr);
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

      Set_Type (Ret, Make_String_Type);
      --  FIXME: We really need some sort of array type here, such as:
      --  Make_Array_Type (
      --     I_Subtype => Element_Type,
      --     Size => String_Length_Expr));
      String_To_Name_Buffer (Strval (N));
      Set_Value (Ret, Name_Buffer (1 .. StrLen));
      Set_Source_Location (Ret, Sloc (N));
      return Ret;
   end Do_String_Constant;

   ----------------------
   -- Do_Real_Constant --
   ----------------------

   function Do_Real_Constant (N : Node_Id) return Irep is
      Ret           : constant Irep := New_Irep (I_Constant_Expr);
      Real_Constant_Type : constant Irep := Do_Type_Reference (Etype (N));
      Bit_Width : constant Nat := UI_To_Int (Esize (Etype (N)));
      Unknown_Float_Width : exception;
   begin
      Set_Source_Location (Ret, Sloc (N));
      Set_Type (Ret, Real_Constant_Type);

      begin
         Set_Value (Ret,
                    (case Bit_Width is
                        when 32 =>
                           Convert_Ureal_To_Hex_32bits_IEEE (Realval (N)),
                        when 64 =>
                           Convert_Ureal_To_Hex_64bits_IEEE (Realval (N)),
                        when others => raise Unknown_Float_Width));

      exception
         when Error : others =>
            Report_Unhandled_Node_Empty (N, "Do_Real_Constant",
                                         Ada.Exceptions.Exception_Name
                                           (Error));
            Set_Value (Ret, "00000000000000000000000000000000");
            return Ret;
      end;

      return Ret;
   end Do_Real_Constant;

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
         Report_Unhandled_Node_Empty (N, "Do_Derived_Type_Definition",
                                      "abstract present not true");
         return New_Irep (I_Bool_Type);
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
            Set_Basename (Element, Base_Name);
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
                       Convert_Uint_To_Hex (Enumeration_Rep (Member),
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

   function Create_Dummy_Irep return Irep is
      ir : constant Irep := New_Irep (I_Constant_Expr);
   begin
      Set_Value (ir, "0");
      return ir;
   end Create_Dummy_Irep;

   function Do_Expression (N : Node_Id) return Irep is
   begin
      Declare_Itype (Etype (N));
      case Nkind (N) is
         when N_Identifier           => return Do_Identifier (N);
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
               when Attribute_Length => return Do_Array_Length (N);
               when Attribute_Range  =>
                  return Report_Unhandled_Node_Irep (N, "Do_Expression",
                                                     "Range attribute");
               when Attribute_First  =>
                  return Report_Unhandled_Node_Irep (N, "Do_Expression",
                                                     "First attribute");
               when Attribute_Last   =>
                  return Report_Unhandled_Node_Irep (N, "Do_Expression",
                                                     "Last attribute");
               when others           =>
                  return Report_Unhandled_Node_Irep (N, "Do_Expression",
                                                     "Unknown attribute");
            end case;
         when N_Explicit_Dereference => return Do_Dereference (N);
         when N_Case_Expression      => return Do_Case_Expression (N);
         when N_Aggregate            => return Do_Aggregate_Literal (N);
         when N_Indexed_Component    => return Do_Indexed_Component (N);
         when N_Slice                => return Do_Slice (N);
         when N_In =>
            return Report_Unhandled_Node_Irep (N, "Do_Expression",
                                               "In");
         when N_Real_Literal => return Do_Real_Constant (N);
         when N_If_Expression => return Do_If_Expression (N);
         when N_And_Then => return Do_And_Then (N);
         when N_Qualified_Expression =>
            return Report_Unhandled_Node_Irep (N, "Do_Expression",
                                               "Qualified");
         when N_Quantified_Expression =>
            return Report_Unhandled_Node_Irep (N, "Do_Expression",
                                               "Quantified");
         when others                 =>
            return Report_Unhandled_Node_Irep (N, "Do_Expression",
                                               "Unknown expression kind");
      end case;
   end Do_Expression;

   -------------------
   --  Do_And_Then  --
   -------------------

   function Do_And_Then (N : Node_Id) return Irep is
      L : constant Node_Id := Left_Opnd (N);
      R : constant Node_Id := Right_Opnd (N);
      Expr : constant Irep := New_Irep (I_Op_And);
   begin
      Append_Op (Expr, Do_Expression (L));
      Append_Op (Expr, Do_Expression (R));
      Set_Type (Expr, Make_Bool_Type);
      Set_Source_Location (Expr, Sloc (N));
      return Expr;
   end Do_And_Then;

   ------------------
   --  Do_Or_Else  --
   ------------------

   function Do_Or_Else (N : Node_Id) return Irep is
      L : constant Node_Id := Left_Opnd (N);
      R : constant Node_Id := Right_Opnd (N);
      Expr : constant Irep := New_Irep (I_Op_Or);
   begin
      Append_Op (Expr, Do_Expression (L));
      Append_Op (Expr, Do_Expression (R));
      Set_Type (Expr, Make_Bool_Type);
      Set_Source_Location (Expr, Sloc (N));
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
      Do_Type_Declaration (New_Type, E);

      --  Declare the implicit initial subtype too
      if Etype (E) /= E then
         Do_Type_Declaration (New_Type, Etype (E));
      end if;
   end Do_Full_Type_Declaration;

   ----------------------
   -- Make_Assume_Expr --
   ----------------------

   function Make_Assume_Expr (N : Node_Id; Assumption : Irep) return Irep is
      Sym_Assume : constant Irep := Make_Symbol_Expr (
                                     Source_Location => Sloc (N),
                                     I_Type          => New_Irep (I_Code_Type),
                                     Range_Check     => False,
                                     Identifier      => "__CPROVER_assume");
      SEE_Fun_Call : constant Irep :=
        New_Irep (I_Side_Effect_Expr_Function_Call);
      Assume_Args  : constant Irep := New_Irep (I_Argument_List);
   begin

      Append_Argument (Assume_Args, Assumption);

      Set_Source_Location (SEE_Fun_Call, Sloc (N));
      Set_Function        (SEE_Fun_Call, Sym_Assume);
      Set_Arguments       (SEE_Fun_Call, Assume_Args);
      Set_Type            (SEE_Fun_Call, Make_Void_Type);
      return SEE_Fun_Call;
   end Make_Assume_Expr;

   -----------------------------
   -- Do_Nondet_Function_Call --
   -----------------------------

   function Do_Nondet_Function_Call (N : Node_Id) return Irep is
      Func_Str     : constant String := Unique_Name (Entity (Name (N)));
      Func_Name    : constant Symbol_Id := Intern (Func_Str);
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
            SE_Call_Expr : constant Irep :=
              Make_Assume_Expr (N, Make_Range_Expression (Sym_Nondet,
                                                       Get_Type (Sym_Nondet)));
            Nondet_Expr  : constant Irep :=
              New_Irep (I_Side_Effect_Expr_Nondet);
            Assume_And_Yield : constant Irep := New_Irep (I_Op_Comma);
         begin
            Set_Source_Location (Sym_Nondet, Sloc (N));
            Set_Type (Nondet_Expr, Type_Irep);
            Set_Source_Location (Nondet_Expr, Sloc (N));

            Set_Lhs (Assume_And_Yield, SE_Call_Expr);
            Set_Rhs (Assume_And_Yield, Sym_Nondet);
            Set_Source_Location (Assume_And_Yield, Sloc (N));
            return Make_Let_Expr
              (Symbol          => Sym_Nondet,
               Value           => Nondet_Expr,
               Where           => Assume_And_Yield,
               Source_Location => Sloc (N),
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
      The_Function : Irep;

   begin
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
         return Report_Unhandled_Node_Irep (N, "Do_Function_Call",
                                    "function entity not defining identifier");
      end if;
      if Name_Has_Prefix (N, "nondet") or else
        Has_GNAT2goto_Annotation (Func_Ent, "nondet")
      then
         return Do_Nondet_Function_Call (N);
      else
         The_Function := New_Irep (I_Symbol_Expr);
         Set_Identifier (The_Function, Unintern (Func_Name));

         if Global_Symbol_Table.Contains (Func_Name) then
            Func_Symbol  := Global_Symbol_Table (Func_Name);
            Set_Type (The_Function, Func_Symbol.SymType);
            --  ??? why not get this from the entity

            return R : constant Irep :=
              New_Irep (I_Side_Effect_Expr_Function_Call)
            do
               Set_Source_Location (R, Sloc (N));
               Set_Function        (R, The_Function);
               Set_Arguments       (R, Do_Call_Parameters (N));
               Set_Type (R, Get_Return_Type (Func_Symbol.SymType));
            end return;
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
   begin
      return Process_Statements (Stmts);
   end Do_Handled_Sequence_Of_Statements;

   -------------------
   -- Do_Identifier --
   -------------------

   function Do_Identifier (N : Node_Id) return Irep is
      E : constant Entity_Id := Entity (N);
      Subst_Cursor : constant Identifier_Maps.Cursor :=
        Identifier_Substitution_Map.Find (E);
   begin
      if Identifier_Maps.Has_Element (Subst_Cursor) then
         --  Indicates instead of literally referring to the given
         --  name, we should return some replacement irep. Currently
         --  this is used for discriminants during record init.
         return Identifier_Maps.Element (Subst_Cursor);
      else
         if Nkind (E) in N_Has_Etype and then (Is_Type (Etype (E))) then
            return Do_Defining_Identifier (E);
         end if;
         Report_Unhandled_Node_Empty (N, "Do_Identifier",
                                         "Etype not a type");
         return R : constant Irep := New_Irep (I_Symbol_Expr);
      end if;
   end Do_Identifier;

   -----------------------
   -- Do_Exit_Statement --
   -----------------------

   function Do_Exit_Statement (N : Node_Id) return Irep is

      -------------
      -- Do_When --
      -------------

      function Do_When (N : Node_Id) return Irep with
        Pre  => Nkind (N) in N_Subexpr,
        Post => Kind (Do_When'Result) in Class_Code;

      function Do_When (N : Node_Id) return Irep is
         Cond_Expr : constant Irep := Do_Expression (N);
         Ret       : constant Irep := New_Irep (I_Code_Ifthenelse);
      begin
         Set_Source_Location (Ret, Sloc (N));
         Set_Cond (Ret, Cond_Expr);
         return Ret;
      end Do_When;

      Jump_Irep : Irep;
   begin
      if Present (Name (N)) then
         Jump_Irep := Make_Code_Goto
           (Source_Location => Sloc (N),
            Destination     =>
              Get_Name_String (Chars (Name (N))) & "_exit");
      else
         Jump_Irep := Make_Code_Break (Source_Location => Sloc (N));
      end if;
      if Present (Condition (N)) then
         return R : constant Irep := Do_When (Condition (N)) do
            Set_Then_Case (R, Jump_Irep);
         end return;
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

   -------------------------------------
   -- Do_Itype_String_Literal_Subtype --
   -------------------------------------

   function Do_Itype_String_Literal_Subtype (N : Node_Id) return Irep is
   begin
      --  Since we don't note the bounds at the irep level, just
      --  call this an alias:
      return R : constant Irep := New_Irep (I_Symbol_Type) do
         Set_Identifier (R, Unique_Name (Etype (N)));
      end return;
   end Do_Itype_String_Literal_Subtype;

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
         when E_String_Literal_Subtype => Do_Itype_String_Literal_Subtype (N),
         when E_Signed_Integer_Subtype => Do_Itype_Integer_Subtype (N),
         when E_Record_Subtype => Do_Itype_Record_Subtype (N),
         when E_Signed_Integer_Type => Do_Itype_Integer_Type (N),
         when E_Floating_Point_Type => Create_Dummy_Irep,
         when E_Anonymous_Access_Type => Make_Pointer_Type
        (Base => Do_Type_Reference (Designated_Type (Etype (N)))),
         when others => Report_Unhandled_Node_Irep (N, "Do_Itype_Definition",
                                                    "Unknown Ekind"));
   end Do_Itype_Definition;

   ------------------------------
   -- Do_Itype_Integer_Subtype --
   ------------------------------

   function Do_Itype_Integer_Subtype (N : Entity_Id) return Irep is
      Lower_Bound : constant Irep :=
        Do_Expression (Low_Bound (Scalar_Range (N)));
      Upper_Bound : constant Irep :=
        Do_Expression (High_Bound (Scalar_Range (N)));

   begin
      if Kind (Upper_Bound) /= I_Constant_Expr or
        Kind (Lower_Bound) /= I_Constant_Expr
      then
         return Report_Unhandled_Node_Irep (N, "Do_Itype_Integer_Subtype",
                                            "Non-literal bound unsupported");
      end if;
      return
        Make_Bounded_Signedbv_Type (
                               Lower_Bound => Store_Bound (Bound_Type (Intval (
                                      Low_Bound (Scalar_Range (N))))),
                               Upper_Bound => Store_Bound (Bound_Type (Intval (
                                      High_Bound (Scalar_Range (N))))),
                               Width       => Positive (UI_To_Int (Esize (N))),
                               I_Subtype   => Ireps.Empty);
   end Do_Itype_Integer_Subtype;

   ------------------------------
   -- Do_Itype_Integer_Type --
   ------------------------------

   function Do_Itype_Integer_Type (N : Entity_Id) return Irep is
     (Make_Bounded_Signedbv_Type (
                               Lower_Bound => Store_Bound (Bound_Type (Intval (
                                      Low_Bound (Scalar_Range (N))))),
                               Upper_Bound => Store_Bound (Bound_Type (Intval (
                                      High_Bound (Scalar_Range (N))))),
                               Width       => Positive (UI_To_Int (Esize (N))),
                               I_Subtype   => Ireps.Empty));

   -----------------------------
   -- Do_Itype_Record_Subtype --
   -----------------------------

   --  Don't need to record discriminant constraints in the irep
   --  representation (yet), so just an alias for its supertype.
   function Do_Itype_Record_Subtype (N : Entity_Id) return Irep is
   begin
      return Do_Type_Reference (Etype (N));
   end Do_Itype_Record_Subtype;

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
               return Report_Unhandled_Node_Irep (N,
                                                "Do_Anonymous_Type_Definition",
                                                  "Unknown typedef");
         end case;

      end Do_Anonymous_Type_Definition;

      New_Type : constant Irep := Do_Anonymous_Type_Definition;
   begin
      Do_Type_Declaration (New_Type, Typedef);
   end Do_Itype_Reference;

   -----------------------
   -- Do_Case_Statement --
   -----------------------

   function Do_Case_Statement (N : Node_Id) return Irep is
      Ret : constant Irep := New_Irep (I_Code_Block);
      Value : constant Irep := Do_Expression (Expression (N));

      --  Auxiliary function to create a single test case
      --  to emplace in a condition from a list of alternative
      --  values.
      function Make_Case_Test (Alts : List_Id) return Irep;
      function Make_Case_Test (Alts : List_Id) return Irep is
         function Make_Single_Test (Alt : Node_Id) return Irep;
         function Make_Single_Test (Alt : Node_Id) return Irep is
            Ret : constant Irep := New_Irep (I_Op_Eq);
            Rhs : constant Irep := Do_Expression (Alt);
         begin
            Set_Lhs (Ret, Value);
            Set_Rhs (Ret, Rhs);
            Set_Type (Ret, Make_Bool_Type);
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
            Set_Type (Big_Or, New_Irep (I_Bool_Type));
            while Present (This_Alt) loop
               Append_Op (Big_Or, Make_Single_Test (This_Alt));
               Next (This_Alt);
            end loop;
            return Big_Or;
         end;
      end Make_Case_Test;

      This_Alt : Node_Id := First (Alternatives (N));
   begin
      pragma Assert (List_Length (Alternatives (N)) >= 1);
      --  Do-while loop because there must be at least one alternative.
      loop
         declare
            This_Stmt : constant Irep :=
             Process_Statements (Statements (This_Alt));
            This_Alt_Copy : constant Node_Id := This_Alt;
            This_Test : Irep;
         begin
            Next (This_Alt);
            if not Present (This_Alt) then
               --  Omit test, this is either `others`
               --  or the last case of complete coverage
               This_Test := This_Stmt;
               Append_Op (Ret, This_Test);
            else
               This_Test := New_Irep (I_Code_Ifthenelse);
               Set_Cond (This_Test,
                         Make_Case_Test
                           (Discrete_Choices (This_Alt_Copy)));
               Set_Then_Case (This_Test, This_Stmt);
               Append_Op (Ret, This_Test);
            end if;
         end;
         exit when not Present (This_Alt);
      end loop;
      return Ret;
   end Do_Case_Statement;

   -----------------------
   -- Do_Loop_Statement --
   -----------------------

   function Do_Loop_Statement (N : Node_Id) return Irep is
      Iter_Scheme  : constant Node_Id := Iteration_Scheme (N);
      Body_Block   : constant Irep := Process_Statements (Statements (N));

      function Do_For_Statement (Param : Irep; Cond : Irep; Post : Irep)
                                 return Irep;

      function Do_While_Statement (Cond : Irep) return Irep;

      ----------------------
      -- Do_For_Statement --
      ----------------------

      function Do_For_Statement (Param : Irep; Cond : Irep; Post : Irep)
                                 return Irep
      is
         Ret : constant Irep := New_Irep (I_Code_For);
      begin
         Set_Source_Location (Ret, Sloc (N));
         Set_Init (Ret, Param);
         Set_Cond (Ret, Cond);
         Set_Iter (Ret, Post);
         --  body block done in caller
         return Ret;
      end Do_For_Statement;

      ------------------------
      -- Do_While_Statement --
      ------------------------

      function Do_While_Statement (Cond : Irep) return Irep is
         Ret : constant Irep := New_Irep (I_Code_While);
      begin
         --  body block done in caller
         Set_Source_Location (Ret, Sloc (N));
         Set_Cond (Ret, Cond);
         return Ret;
      end Do_While_Statement;

      Loop_Irep : Irep;
      Loop_Wrapper : constant Irep := New_Irep (I_Code_Block);
   begin
      if not Present (Iter_Scheme) then
         --  infinite loop
         declare
            Const_True : constant Irep := New_Irep (I_Constant_Expr);
         begin
            Set_Value (Const_True, "true");
            Set_Type (Const_True, Make_Bool_Type);
            Loop_Irep := Do_While_Statement (Const_True);
         end;
      else
         if Present (Condition (Iter_Scheme)) then
            --  WHILE loop
            declare
               Cond : constant Irep := Do_Expression (Condition (Iter_Scheme));
            begin
               Loop_Irep := Do_While_Statement (Cond);
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
                      (Source_Location => Sloc (Defining_Identifier (Spec)),
                       I_Type          => Type_Loopvar,
                       Identifier      => Loopvar_Name);

                  Init : constant Irep := New_Irep (I_Code_Assign);
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
                  then
                     Report_Unhandled_Node_Empty (N, "Do_While_Statement",
                                                  "Wrong Nkind spec");
                     return Loop_Wrapper;
                  end if;
                  Dsd := Get_Range (Spec);
                  Bound_Low := Do_Expression (Low_Bound (Dsd));
                  Bound_High := Do_Expression (High_Bound (Dsd));
                  --  Loop var decl
                  Append_Op (Loop_Wrapper, Make_Code_Decl
                             (Symbol          => Sym_Loopvar,
                              Source_Location => Sloc
                                (Defining_Identifier (Spec))));

                  --  TODO: needs generalization to support enums
                  if Reverse_Present (Spec) then
                     Set_Lhs (Init, Sym_Loopvar);
                     Set_Rhs (Init, Bound_High);
                     Cond := Make_Op_Geq
                       (Rhs             => Bound_Low,
                        Lhs             => Sym_Loopvar,
                        Source_Location => Sloc (Spec),
                        Overflow_Check  => False,
                        I_Type          => Make_Bool_Type,
                        Range_Check     => False);
                     Post := Make_Increment
                       (Sym_Loopvar, Etype (Low_Bound (Dsd)), -1);
                  else
                     Set_Lhs (Init, Sym_Loopvar);
                     Set_Rhs (Init, Bound_Low);
                     Cond := Make_Op_Leq
                       (Rhs             => Bound_High,
                        Lhs             => Sym_Loopvar,
                        Source_Location => Sloc (Spec),
                        Overflow_Check  => False,
                        I_Type          => Make_Bool_Type,
                        Range_Check     => False);
                     Post := Make_Increment
                       (Sym_Loopvar, Etype (Low_Bound (Dsd)), 1);
                  end if;
                  Set_Source_Location (Init, Sloc (Spec));
                  Set_Source_Location (Post, Sloc (Spec));
                  Loop_Irep := Do_For_Statement (Init, Cond, Post);
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
                      (Code            => New_Irep (I_Code_Skip),
                       Source_Location => Sloc (Identifier (N)),
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
         A_Irep : constant Irep := New_Irep
           (if Which in Pragma_Assume
            then I_Code_Assume else I_Code_Assert);

         --  To be set by iterator:
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

         if Which in Pragma_Assert | Pragma_Loop_Invariant then
            Set_Assertion (A_Irep, Check);
         else
            Set_Assumption (A_Irep, Check);
         end if;
         Set_Source_Location (A_Irep, Sloc (N));
         Append_Op (Block, A_Irep);
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
         when Name_Linker_Options =>
            --  Used to specify the system linker parameters needed when a
            --  given compilation unit is included in a partition. We want to
            --  know that code manipulates the linking.
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                               "Known but unsupported pragma: Linker Options");
         when Name_Annotate |
            --  Ignore here. Rather look for those when we process a node.
              Name_Assertion_Policy |
            --  Control the pragma Assert according to the policy identifier
            --  which can be Check, Ignore, or implementation-defined.
            --  Ignore means that assertions are ignored at run-time -> Ignored
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
              Name_Unsuppress =>
            --  Voids the supressing request. -> Ignored
            null;
         when others =>
            Report_Unhandled_Node_Empty (N, "Do_Pragma",
                                         "Unknown pragma name");
      end case;
   end Do_Pragma;

   ---------------------------
   -- Do_Object_Declaration --
   ---------------------------

   procedure Do_Object_Declaration (N : Node_Id; Block : Irep) is
      Defined : constant Entity_Id := Defining_Identifier (N);
      Id   : constant Irep := Do_Defining_Identifier (Defined);
      Decl : constant Irep := New_Irep (I_Code_Decl);
      Init_Expr : Irep := Ireps.Empty;

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
            Report_Unhandled_Node_Empty (Do_Object_Declaration.N,
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
           or else Ekind (E) = E_Array_Subtype;
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
                           Source_Location => Sloc (E));
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
         Ret : constant Irep := New_Irep (I_Struct_Expr);

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

                  New_Expr := Do_Expression (Disc_Actual);
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

         Set_Type (Ret, Do_Type_Reference (E));
         return Ret;

      end Make_Record_Default_Initialiser;

      function Make_Default_Initialiser (E : Entity_Id;
                                         DCs : Node_Id) return Irep is
      begin
         if Ekind (E) in Array_Kind then
            return Make_Array_Default_Initialiser (E);
         elsif Ekind (E) in Record_Kind then
            return Make_Record_Default_Initialiser (E, DCs);
         else
            return Report_Unhandled_Node_Irep (N, "Make_Default_Initialiser",
                                                 "Unknown Ekind");
         end if;
      end Make_Default_Initialiser;

      --  Begin processing for Do_Object_Declaration

   begin
      Set_Source_Location (Decl, (Sloc (N)));
      Set_Symbol (Decl, Id);
      Append_Op (Block, Decl);

      if Has_Init_Expression (N) then
         Init_Expr := Do_Expression (Expression (N));
      elsif Needs_Default_Initialisation (Etype (Defined)) then
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

      if Init_Expr /= Ireps.Empty then
         Append_Op (Block, Make_Code_Assign (Lhs => Id,
                                             Rhs => Init_Expr,
                                             Source_Location => Sloc (N)));
      end if;
   end Do_Object_Declaration;

   -------------------------
   --     Do_Op_Not       --
   -------------------------

   function Do_Op_Not (N : Node_Id) return Irep is
      Boolean_Value : constant Irep := Do_Expression (Right_Opnd (N));
   begin
      return Make_Op_Not (Boolean_Value, Sloc (N), Make_Bool_Type);
   end Do_Op_Not;

   -------------------------
   --     Do_Op_Minus    --
   -------------------------

   function Do_Op_Minus (N : Node_Id) return Irep is
      Original_Value : constant Irep := Do_Expression (Right_Opnd (N));
      Original_Value_Type : constant Irep := Do_Type_Reference (Etype (N));
   begin
      return Make_Op_Neg (Original_Value, Sloc (N), Original_Value_Type);
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
      LHS_Bool_Value : constant Irep := Do_Expression (Left_Opnd (N));
      RHS_Bool_Value : constant Irep := Do_Expression (Right_Opnd (N));
      Cast_LHS_To_Integer : constant Irep :=
        Make_Op_Typecast (Op0 => LHS_Bool_Value,
                          Source_Location => Sloc (N),
                          I_Type => Make_Signedbv_Type (Make_Nil_Type, 32));
      Cast_RHS_To_Integer : constant Irep :=
        Make_Op_Typecast (Op0 => RHS_Bool_Value,
                          Source_Location => Sloc (N),
                          I_Type => Make_Signedbv_Type (Make_Nil_Type, 32));
      R : constant Irep := Operator (Lhs => Cast_LHS_To_Integer,
                                     Rhs => Cast_RHS_To_Integer,
                                     Source_Location => Sloc (N),
                                     Overflow_Check => False,
                                     Range_Check => False,
                                     I_Type =>
                                       Get_Type (Cast_LHS_To_Integer));
   begin
      return Make_Op_Typecast (Op0 => R,
                               Source_Location => Sloc (N),
                               I_Type => Make_Bool_Type);
   end Do_Bit_Op;

   function Do_Op_Abs (N : Node_Id) return Irep
     with Pre => (Nkind (N) = N_Op_Abs);

   function Do_Op_Abs (N : Node_Id) return Irep is
      Operand : constant Irep := Do_Expression (Right_Opnd (N));
   begin
      return Make_Op_Abs
        (Op0 => Operand,
         Source_Location => Sloc (N),
         I_Type => Do_Type_Reference (Etype (N)));
   end Do_Op_Abs;

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
         return Do_Op_Not (N);
      elsif Nkind (N) = N_Op_Minus then
         return Do_Op_Minus (N);
      elsif Nkind (N) = N_Op_Or then
         return Do_Bit_Op (N, Make_Op_Bitor'Access);
      elsif Nkind (N) = N_Op_And then
         return Do_Bit_Op (N, Make_Op_Bitand'Access);
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
           => Report_Unhandled_Node_Kind (Do_Operator_Simple.N,
                                          "Do_Operator_Simple",
                                          "Unsupported operand"));
      end Op_To_Kind;

      Op_Kind : constant Irep_Kind := Op_To_Kind (N_Op (Nkind (N)));
      Ret     : constant Irep      := New_Irep (Op_Kind);

   --  Start of processing for Do_Operator_Simple

   begin
      Set_Source_Location (Ret, Sloc (N));
      if not (Kind (Ret) in Class_Binary_Expr
        | I_Code_Assign
        | I_Code_Function_Call
        | I_Ieee_Float_Op_Expr
        | I_Side_Effect_Expr_Assign)
      then
         Report_Unhandled_Node_Empty (N, "Do_Operator_Simple",
                                      "Unsupported kind of LHS");
         return Ret;
      end if;
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
      Package_Decs : constant Irep := New_Irep (I_Code_Block);
   begin
      Set_Source_Location (Package_Decs, Sloc (N));
      if Present (Visible_Declarations (N)) then
         Process_Declarations (Visible_Declarations (N), Package_Decs);
      end if;
      if Present (Private_Declarations (N)) then
         Process_Declarations (Private_Declarations (N), Package_Decs);
      end if;
   end Do_Package_Specification;

   ---------------------------------
   -- Do_Procedure_Call_Statement --
   ---------------------------------

   function Do_Procedure_Call_Statement (N : Node_Id) return Irep
   is
      Callee : Unbounded_String;
      --  ??? use Get_Entity_Name from gnat2why to handle entries and entry
      --  families (and most likely extend it for accesses to subprograms).

      Proc   : constant Irep := New_Irep (I_Symbol_Expr);
      R      : constant Irep := New_Irep (I_Code_Function_Call);
      Sym_Id : Symbol_Id;

   begin
      if not (Nkind (Name (N)) in N_Has_Entity)
        and then Nkind (Name (N)) /= N_Aspect_Specification
        and then Nkind (Name (N)) /= N_Attribute_Definition_Clause
        and then Nkind (Name (N)) /= N_Freeze_Entity
        and then Nkind (Name (N)) /= N_Freeze_Generic_Entity
      then
         Report_Unhandled_Node_Empty (N, "Do_Procedure_Call_Statement",
                                      "Wrong nkind of name");
         return R;
      end if;
      Callee := To_Unbounded_String (Unique_Name (Entity (Name (N))));
      Sym_Id := Intern (To_String (Callee));
      Set_Identifier (Proc, To_String (Callee));

      Set_Source_Location (R, Sloc (N));
      Set_Lhs (R, Make_Nil (Sloc (N)));
      Set_Function (R, Proc);
      Set_Arguments (R, Do_Call_Parameters (N));

      if Global_Symbol_Table.Contains (Sym_Id) then
         Set_Type (Proc, Global_Symbol_Table (Sym_Id).SymType);
         --  ??? Why not look at type of entity?
      else
         --  Packages with belong to the RTS are not being parsed by us,
         --  therefore functions like "Put_Line" have have no entry
         --  in the symbol table
         Report_Unhandled_Node_Empty (N, "Do_Procedure_Call_Statement",
                                      "sym id not in symbol table");
      end if;

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

   function Do_Record_Definition (N : Node_Id; Discs : List_Id) return Irep is

      Components : constant Irep := New_Irep (I_Struct_Union_Components);
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
         Set_Source_Location (Comp_Irep, Sloc (Comp_Node));
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
      Ret            : constant Irep := New_Irep (I_Struct_Type);

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
            Record_Type : Node_Id;
            Variant_Iter : Node_Id;
            Variant_Spec : Node_Id;
            Union_Selector : constant Irep := New_Irep (I_Member_Expr);
            Substruct_Selector : constant Irep := New_Irep (I_Member_Expr);
            Disc_Selector : constant Irep := New_Irep (I_Member_Expr);
            Disc_Check : constant Irep := New_Irep (I_Op_Eq);
            Comma_Expr : constant Irep := New_Irep (I_Op_Comma);
         begin
            if Nkind (Parent (Etype (Prefix (N)))) /= N_Full_Type_Declaration
            then
               Report_Unhandled_Node_Empty (N, "Do_Selected_Component",
                                           "Parent not full type declaration");
               return Comma_Expr;
            end if;
            Record_Type := Type_Definition (Parent (Etype (Prefix (N))));
            Variant_Iter := First (Variants (Variant_Part (Component_List
                                                             (Record_Type))));
            Variant_Spec := Variant_Part (Component_List (Record_Type));
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

            if not Present (Component_Variant) then
               Report_Unhandled_Node_Empty (N, "Do_Selected_Component",
                                            "Component variant not present");
               return Comma_Expr;
            end if;

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
      Ret   : constant Irep := New_Irep (I_Bounded_Signedbv_Type);

      E : constant Entity_Id := Defining_Entity (Parent (N));
      --  Type entity

   begin
      if not Is_Type (E) then
         Report_Unhandled_Node_Empty (N, "Do_Signed_Integer_Definition",
                                      "Entity id is not a type");
         return Ret;
      end if;
      Set_Lower_Bound (I     => Ret,
                       Value => Store_Bound (Bound_Type (Intval (
                         Low_Bound (N)))));
      Set_Upper_Bound (I     => Ret,
                       Value => Store_Bound (Bound_Type (Intval (
                         High_Bound (N)))));
      Set_Width (I     => Ret,
                 Value => Positive (UI_To_Int (Esize (E))));
      return  Ret;
   end Do_Signed_Integer_Definition;

   ----------------------------------
   -- Do_Floating_Point_Definition --
   ----------------------------------

   function Do_Floating_Point_Definition (N : Node_Id) return Irep is
      E : constant Entity_Id := Defining_Entity (Parent (N));

      --  This determines if ranges were specified or not
      Scalar_Range_Ent : constant Node_Id := Scalar_Range (E);
      Ret : constant Irep :=
        New_Irep (if Nkind (Scalar_Range_Ent) = N_Real_Range_Specification
                  then I_Bounded_Floatbv_Type
                  else I_Floatbv_Type);
      Esize_Width : constant Nat := UI_To_Int (Esize (E));
   begin
      Set_Width (Ret, Integer (Esize_Width));
      Set_F (Ret, Float_Mantissa_Size (Ret));

      --  If user specified range bounds we store them
      if Nkind (Scalar_Range_Ent) = N_Real_Range_Specification then
         declare
            Range_Spec : constant Node_Id := Real_Range_Specification (N);
            Lower_Bound : constant Integer :=
              Store_Real_Bound (Bound_Type_Real (Realval (
                                Low_Bound (Range_Spec))));
            Upper_Bound : constant Integer :=
              Store_Real_Bound (Bound_Type_Real (Realval (
                                High_Bound (Range_Spec))));
         begin
            Set_Lower_Bound (Ret, Lower_Bound);
            Set_Upper_Bound (Ret, Upper_Bound);
         end;
      end if;

      return Ret;
   end Do_Floating_Point_Definition;

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
      else
         --  A return statement always needs a value, so make a nil value
         --  if this is a plain 'Return' statement
         Set_Return_Value (R, Make_Nil (Sloc (N)));
      end if;
      return R;
   end Do_Simple_Return_Statement;

   ------------------------
   -- Do_Subprogram_Body --
   ------------------------

   procedure Do_Subprogram_Body (N : Node_Id) is
      Proc_Name   : constant Symbol_Id :=
        Intern (Unique_Name (Defining_Entity (N)));
      --  Intern (Unique_Name (Corresponding_Spec (N)));

      Proc_Body   : constant Irep := Do_Subprogram_Or_Block (N);
      Proc_Symbol : Symbol;
   begin
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
      Proc_Symbol.Value := Proc_Body;
      Global_Symbol_Table.Replace (Proc_Name, Proc_Symbol);
   end Do_Subprogram_Body;

   -------------------------------
   -- Do_Subprogram_Declaration --
   -------------------------------

   procedure Do_Subprogram_Declaration (N : Node_Id) is
   begin
      Register_Subprogram_Specification (Specification (N));
      --  Todo Aspect specifications
   end Do_Subprogram_Declaration;

   ----------------------------
   -- Do_Subprogram_Or_Block --
   ----------------------------

   function Do_Subprogram_Or_Block (N : Node_Id) return Irep is
      Decls : constant List_Id := Declarations (N);
      HSS   : constant Node_Id := Handled_Statement_Sequence (N);
      Reps : constant Irep := New_Irep (I_Code_Block);
   begin
      if Present (Decls) then
         Process_Declarations (Decls, Reps);
      end if;

      Set_Source_Location (Reps, Sloc (N));
      if Present (HSS) then
         Process_Statement (HSS, Reps);
      end if;

      return Reps;
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

            Param_Type_Base : Irep;
            Param_Type      : Irep;

            Param_Name : constant String :=
                Unique_Name (Defining_Identifier (Param_Iter));

            Param_Irep : constant Irep := New_Irep (I_Code_Parameter);
            Param_Symbol : Symbol;
         begin
            if not (Nkind (Parameter_Type (Param_Iter)) in N_Has_Etype) then
               Report_Unhandled_Node_Empty (N, "Do_Subprogram_Specification",
                                            "Param iter type not have etype");
               return Ret;
            end if;
            Param_Type_Base :=
              Do_Type_Reference (Etype (Parameter_Type (Param_Iter)));
            Param_Type :=
              (if Is_Out
                 then Make_Pointer_Type (Param_Type_Base)
                 else Param_Type_Base);
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
      if Nkind (N) = N_Subtype_Indication then
         Underlying := Do_Type_Reference (Etype (Subtype_Mark (N)));
         Constr := Constraint (N);
         if Present (Constr) then
            case Nkind (Constr) is
            when N_Range_Constraint =>
               return Do_Range_Constraint (Constr, Underlying);
            when N_Index_Or_Discriminant_Constraint =>
               return Do_Index_Or_Discriminant_Constraint (Constr, Underlying);
               when others =>
                  return Report_Unhandled_Node_Irep (N,
                                                     "Do_Subtype_Indication",
                                                    "Unknown expression kind");
            end case;
         else
            return Underlying;
         end if;
      elsif Nkind (N) = N_Identifier then
         --  subtype indications w/o constraint are given only as identifier
         Underlying := Do_Type_Reference (Etype (N));
         return Underlying;
      else
         return Report_Unhandled_Node_Irep (N, "Do_Subtype_Indication",
                                            "Unknown expression kind");
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
      Set_Type (Ret, New_Type);

      if Do_Range_Check (Expression (N)) then
         Set_Op0 (I     => Ret,
                  Value => Make_Range_Assert_Expr (N          => N,
                                                   Value      => To_Convert,
                                                   Bounds_Type => New_Type));
      else
         Set_Op0  (Ret, To_Convert);
      end if;

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
      if not Symbol_Maps.Contains (Global_Symbol_Table, New_Type_Name) then
         Symbol_Maps.Insert (Global_Symbol_Table, New_Type_Name,
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
         when others =>
            return Report_Unhandled_Node_Irep (N, "Do_Type_Definition",
                                               "Unknown expression kind");
      end case;
   end Do_Type_Definition;

   -----------------------
   -- Do_Type_Reference --
   -----------------------

   function Do_Type_Reference (E : Entity_Id) return Irep is
      (Make_Symbol_Type (Identifier => Unique_Name (E)));

   -------------------------
   -- Do_Withed_Unit_Spec --
   -------------------------

   procedure Do_Withed_Unit_Spec (N : Node_Id) is
      Unit_Name : constant String := Get_Name_String (Get_Unit_Name (N));
   begin
      if Defining_Entity (N) = Stand.Standard_Standard or else
        Unit_Name = "system%s"
      then
         --  At the moment Standard or System are not processed: TODO
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
            when others =>
               Put_Line (Standard_Error,
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
   begin
      return Make_Struct_Union_Component (Source_Location => No_Location,
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

   --------------------------------
   --  Warn_Unhandled_Construct  --
   --------------------------------

   procedure Warn_Unhandled_Construct (C : Construct; Mess : String) is
      S : constant String :=
        (case C is
            when Declaration => " declarations ",
            when Statement   => " statements ") & "unhandled";
   begin
      Put_Line (Standard_Error, "Warning: " & Mess & S);
   end Warn_Unhandled_Construct;

   --------------------------
   -- Process_Declaration --
   --------------------------

   procedure Process_Declaration (N : Node_Id; Block : Irep) is
   begin
      --  Deal with the declaration

      case Nkind (N) is

         --  basic_declarations  --

         when N_Full_Type_Declaration =>
            Do_Full_Type_Declaration (N);

         when N_Subtype_Declaration =>
            Do_Subtype_Declaration (N);

         when N_Object_Declaration =>
            Do_Object_Declaration (N, Block);

         when N_Number_Declaration =>
            Warn_Unhandled_Construct (Declaration, "Number");

         when N_Subprogram_Declaration =>
            Do_Subprogram_Declaration (N);

         when N_Abstract_Subprogram_Declaration =>
            Warn_Unhandled_Construct
              (Declaration, "Abstract subprogram");

         when N_Package_Declaration =>
            Warn_Unhandled_Construct (Declaration, "Package");

         when N_Renaming_Declaration =>
            --  renaming declarations are handled by the gnat front-end;
            null;

         when N_Exception_Declaration =>
            Warn_Unhandled_Construct (Declaration, "Exception");

         when N_Generic_Declaration =>
            Warn_Unhandled_Construct (Declaration, "Generic");

         when N_Generic_Instantiation =>
            Warn_Unhandled_Construct (Declaration, "Generic instantiation");

            --  basic_declarative_items  --

         when N_Representation_Clause =>
            Warn_Unhandled_Construct (Declaration, "Representation clause");

         when N_Use_Package_Clause =>
            Warn_Unhandled_Construct (Declaration, "Use package clause");

         when N_Use_Type_Clause =>
            Warn_Unhandled_Construct (Declaration, "Use type clause");

         --  remaining declarative items  --

            --  proper_body  --

         when N_Subprogram_Body =>
            Do_Subprogram_Body (N);

         when N_Package_Body =>
            Warn_Unhandled_Construct (Declaration, "Package body");

         when N_Task_Body =>
            Warn_Unhandled_Construct (Declaration, "Task body");

         when N_Protected_Body =>
            Warn_Unhandled_Construct (Declaration, "Protected body");

            --  body_stub  --

         when N_Subprogram_Body_Stub =>
            Warn_Unhandled_Construct (Declaration, "Subprogram body stub");

         when N_Package_Body_Stub =>
            Warn_Unhandled_Construct (Declaration, "Package body stub");

         when N_Task_Body_Stub =>
            Warn_Unhandled_Construct (Declaration, "Task body stub");

         when N_Protected_Body_Stub =>
            Warn_Unhandled_Construct (Declaration, "Protected body stub");

         --  Pragmas may appear in declarations  --

         when N_Pragma =>
            Warn_Unhandled_Construct (Declaration, "Pragmas in");

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

         when N_Freeze_Entity =>
            --  Ignore, nothing to generate
            null;

         when N_Private_Type_Declaration =>
            Report_Unhandled_Node_Empty (N, "Process_Declaration",
                                       "Private type declaration unsupported");
         when others =>
            Report_Unhandled_Node_Empty (N, "Process_Declaration",
                                         "Unknown declaration kind");

      end case;

   end Process_Declaration;

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
            Warn_Unhandled_Construct (Statement, "goto");

         when N_Procedure_Call_Statement =>
            Append_Op (Block, Do_Procedure_Call_Statement (N));

         when N_Simple_Return_Statement =>
            Append_Op (Block, Do_Simple_Return_Statement (N));

         when N_Entry_Call_Statement =>
            Warn_Unhandled_Construct (Statement, "entry_call");

         when N_Requeue_Statement =>
            Warn_Unhandled_Construct (Statement, "requeue");

         when N_Delay_Statement =>
            Warn_Unhandled_Construct (Statement, "delay");

         when N_Abort_Statement =>
            Warn_Unhandled_Construct (Statement, "abort");

         when N_Raise_Statement =>
            Warn_Unhandled_Construct (Statement, "raise");

         when N_Code_Statement =>
            Warn_Unhandled_Construct (Statement, "code");

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
            Warn_Unhandled_Construct (Statement, "extended_return");

         when N_Accept_Statement =>
            Warn_Unhandled_Construct (Statement, "accept");

            -- Select statements --

         when N_Selective_Accept =>
            Warn_Unhandled_Construct (Statement, "selective_accept");

         when N_Timed_Entry_Call =>
            Warn_Unhandled_Construct (Statement, "timed_entry_call");

         when N_Conditional_Entry_Call =>
            Warn_Unhandled_Construct (Statement, "conditional_entry_call");

         when N_Asynchronous_Select =>
            Warn_Unhandled_Construct (Statement, "asychronous select");

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

         when others =>
            Report_Unhandled_Node_Empty (N, "Process_Statement",
                                         "Unknown expression kind");

      end case;
   end Process_Statement;

   ------------------------
   -- Process_Statements --
   ------------------------

   function Process_Statements (L : List_Id) return Irep is
      Reps : constant Irep := New_Irep (I_Code_Block);
      Stmt : Node_Id := First (L);
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
   begin
      while Power_Of_Two < Mod_Max loop
         Mod_Max_Binary_Logarithm := Mod_Max_Binary_Logarithm + 1;
         Power_Of_Two := Power_Of_Two * 2;
      end loop;
      --  If the max value is 2^w (for w > 0) then we can just
      --  use an unsignedbv of width w
      if Mod_Max = Power_Of_Two then
         return Make_Unsignedbv_Type
           (I_Subtype => Make_Nil_Type,
            Width => Mod_Max_Binary_Logarithm);
      end if;
      return Report_Unhandled_Node_Type
        (N,
         "Do_Modular_Type_Definition",
         "can not handle modular types "
         & "with non-power-of-2 bounds at the moment");
   end Do_Modular_Type_Definition;

   ---------------------------------------
   -- Register_Subprogram_Specification --
   ---------------------------------------

   procedure Register_Subprogram_Specification (N : Node_Id) is
      Subprog_Type : constant Irep :=
        Do_Subprogram_Specification (N);
      Subprog_Name : constant Symbol_Id :=
        Intern (Unique_Name (Defining_Unit_Name (N)));

      Subprog_Symbol : Symbol;

   begin
      Subprog_Symbol.Name       := Subprog_Name;
      Subprog_Symbol.BaseName   := Subprog_Name;
      Subprog_Symbol.PrettyName := Subprog_Name;
      Subprog_Symbol.SymType    := Subprog_Type;
      Subprog_Symbol.Mode       := Intern ("C");
      Subprog_Symbol.Value      := Make_Nil (Sloc (N));

      Global_Symbol_Table.Insert (Subprog_Name, Subprog_Symbol);
   end Register_Subprogram_Specification;

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

end Tree_Walk;
