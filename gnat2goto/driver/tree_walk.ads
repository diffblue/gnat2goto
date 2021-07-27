with Atree;             use Atree;
with Sinfo;             use Sinfo;
with Types;             use Types;
with Symbol_Table_Info; use Symbol_Table_Info;
with Ada.Containers.Ordered_Maps;

with Einfo;                 use Einfo;
with Uintp;                 use Uintp;

with Ireps;                 use Ireps;

package Tree_Walk is

   Global_Symbol_Table : Symbol_Table;
   Anonymous_Type_Counter : Positive := 1;

   --  The following two maps are only ordered because I know how to write
   --  comparators but not hashers as of now; the ordering is unimportant
   --  and they can be switched to hashed_maps as and when.

   --  This maps syntax tree node-ids onto CBMC symbol Ireps
   --  Presently:
   --    an N_Variant_Part maps onto a union type with
   --      members for each variant
   --    an N_Variant maps onto a structure type for a particular
   --      variant.
   --  The values are all I_Symbol_Type.
   package Anonymous_Type_Maps
   is new Ada.Containers.Ordered_Maps
     (Element_Type => Irep,
      Key_Type => Node_Id);

   Anonymous_Type_Map : Anonymous_Type_Maps.Map;

   --  This maps pairs of <element_type, index_type>, each represented
   --  by their definining entity ids, onto function symbols implementing
   --  array duplication. Keys are symbol expressions.

   type Array_Dup_Key is record
      Element_Type : Entity_Id;
      Index_Type : Entity_Id;
   end record;

   function "<" (Left, Right : Array_Dup_Key) return Boolean;

   package Array_Dup_Maps
   is new Ada.Containers.Ordered_Maps
     (Element_Type => Irep,
      Key_Type => Array_Dup_Key);

   Array_Dup_Map : Array_Dup_Maps.Map;

   --  Similar, but for memcpy-style instead of dup-style functions:
   type Array_Copy_Key is record
      LHS_Element_Type : Entity_Id;
      RHS_Element_Type : Entity_Id;
      Index_Type : Entity_Id;
   end record;

   function "<" (Left, Right : Array_Copy_Key) return Boolean;

   package Array_Copy_Maps
   is new Ada.Containers.Ordered_Maps
     (Element_Type => Irep,
      Key_Type => Array_Copy_Key);

   Array_Copy_Map : Array_Copy_Maps.Map;

   package Identifier_Maps
   is new Ada.Containers.Ordered_Maps
     (Element_Type => Irep,
      Key_Type => Entity_Id);

   Identifier_Substitution_Map : Identifier_Maps.Map;

   Check_Function_Symbol : Irep := Ireps.Empty;

   function Do_Compilation_Unit (N : Node_Id) return Symbol
     with Pre => Nkind (N) = N_Compilation_Unit;

   function Do_Defining_Identifier (E : Entity_Id) return Irep
   with Pre  => Nkind (E) = N_Defining_Identifier,
        Post => Kind (Do_Defining_Identifier'Result) in
           I_Symbol_Expr | I_Dereference_Expr;

   procedure Do_Plain_Object_Declaration (Block          : Irep;
                                          Object_Sym     : Irep;
                                          Object_Name    : String;
                                          Object_Def     : Entity_Id;
                                          Init_Expr_Irep : Irep)
     with Pre => Get_Identifier (Object_Sym) = Object_Name;

   function Do_Type_Reference (E : Entity_Id) return Irep
     with Pre  => Is_Type (E),
     Post => Kind (Do_Type_Reference'Result) in Class_Type;

   procedure Do_Type_Declaration (New_Type_In : Irep; E : Entity_Id)
     with Pre => Is_Type (E) and then
     Kind (New_Type_In) in Class_Type;

   function Do_Subtype_Indication (Subtype_Entity : Entity_Id;
                                   N : Node_Id) return Irep
     with Pre  => Nkind (N) in N_Subtype_Indication | N_Identifier
     | N_Expanded_Name,
     Post => Kind (Do_Subtype_Indication'Result) in Class_Type;

   function Make_Struct_Component (Name : String; Ty : Irep) return Irep;

   function Make_Malloc_Function_Call_Expr (Num_Elem : Irep;
                                            Element_Type_Size : Uint;
                                            Source_Loc : Irep)
                                            return Irep
     with Pre => Kind (Num_Elem) in Class_Expr,
     Post => Kind (Make_Malloc_Function_Call_Expr'Result) =
     I_Side_Effect_Expr_Function_Call;

   function Do_Address_Of (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Attribute_Reference;

   function Do_Expression (N : Node_Id) return Irep
   with Pre  => Nkind (N) in N_Subexpr,
        Post => Kind (Do_Expression'Result) in Class_Expr;

   function Do_Function_Call (N : Node_Id) return Irep
     with Pre  => Nkind (N) = N_Function_Call,
     Post => Kind (Do_Function_Call'Result) in Class_Expr;

   function Do_In (N : Node_Id) return Irep
     with Pre => Nkind (N) in N_In | N_Not_In,
       Post => Kind (Do_In'Result) in I_Op_And | I_Nil;
   --  includes I_Nil as currently has unsupported functionality

   function Make_Memcpy_Function_Call_Expr (Destination : Irep;
                                            Source : Irep;
                                            Num_Elem : Irep;
                                            Element_Type_Size : Uint;
                                            Source_Loc : Irep)
                                            return Irep
     with Pre => (Kind (Get_Type (Destination)) = I_Pointer_Type
                  and then Kind (Get_Type (Source)) = I_Pointer_Type
                  and then Kind (Num_Elem) in Class_Expr),
     Post => Kind (Make_Memcpy_Function_Call_Expr'Result) =
     I_Side_Effect_Expr_Function_Call;

   procedure Report_Unhandled_Node_Empty (N : Node_Id;
                                          Fun_Name : String;
                                          Message : String);
   function Report_Unhandled_Node_Irep (N : Node_Id;
                                        Fun_Name : String;
                                        Message : String) return Irep;
   function Report_Unhandled_Node_Kind (N : Node_Id;
                                        Fun_Name : String;
                                        Message : String) return Irep_Kind;

   function Report_Unhandled_Node_Type (N : Node_Id;
                                        Fun_Name : String;
                                        Message : String) return Irep;

   function Do_Range_Constraint (N : Node_Id; Underlying : Irep)
                                 return Irep
     with Pre => Nkind (Range_Expression (N)) = N_Range;

   procedure Append_Declare_And_Init
     (Symbol : Irep; Value : Irep; Block : Irep; Source_Loc : Irep)
     with Pre => Kind (Value) in Class_Expr;

   function Create_Dummy_Irep return Irep;

   function Make_Type_Symbol (Name : Symbol_Id; Defn : Irep) return Symbol;

   function Do_Identifier (N : Node_Id) return Irep
     with Pre  => Nkind (N) in N_Identifier | N_Expanded_Name;

   --  Required by Gnat2goto.Itpes.Declare_Itype
   function Do_Enumeration_Definition (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Enumeration_Type_Definition,
        Post => Kind (Do_Enumeration_Definition'Result) = I_C_Enum_Type;

   --  A higer-level assignment operration than Ireps.Make_Code_Assign.
   --  It is declared here as it used in other packages such as arrays
   --  but not placed in Goto_Utils as it calls subprograms from other
   --  high-level packages.  It may be called recursively from these other
   --  packages.
   procedure Do_Assignment_Op (Block       : Irep;
                               Destination : Irep;
                               Dest_Type   : Entity_Id;
                               Source_Expr : Node_Id);

   --  If a type is an enumeration it needs to be converted to an unsignedbv.
   function Make_Resolved_I_Type (E : Entity_Id) return Irep
   with Pre  => Is_Type (E);

   function Make_Runtime_Check (Condition : Irep) return Irep
   with Pre  => Kind (Get_Type (Condition)) = I_Bool_Type,
        Post => Kind (Make_Runtime_Check'Result) =
                I_Side_Effect_Expr_Function_Call;

end Tree_Walk;
