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

   function Do_Compilation_Unit (N : Node_Id; Unit_Is_Subprogram : out Boolean)
                                 return Symbol
     with Pre => Nkind (N) = N_Compilation_Unit;

   function Do_Type_Reference (E : Entity_Id) return Irep
     with Pre  => Is_Type (E),
     Post => Kind (Do_Type_Reference'Result) in Class_Type;

   procedure Do_Type_Declaration (New_Type_In : Irep; E : Entity_Id)
     with Pre => Is_Type (E) and then
     Kind (New_Type_In) in Class_Type;

   function Do_Subtype_Indication (N : Node_Id) return Irep
     with Pre  => Nkind (N) in N_Subtype_Indication | N_Identifier
     | N_Expanded_Name,
     Post => Kind (Do_Subtype_Indication'Result) in Class_Type;

   function Make_Struct_Component (Name : String; Ty : Irep) return Irep;

   function Make_Malloc_Function_Call_Expr (Num_Elem : Irep;
                                            Element_Type_Size : Uint;
                                            Source_Loc : Source_Ptr)
                                            return Irep
     with Pre => Kind (Num_Elem) in Class_Expr,
     Post => Kind (Make_Malloc_Function_Call_Expr'Result) =
     I_Side_Effect_Expr_Function_Call;

   function Do_Expression (N : Node_Id) return Irep
     with Pre  => Nkind (N) in N_Subexpr,
     Post => Kind (Do_Expression'Result) in Class_Expr;

   function Do_In (N : Node_Id) return Irep
     with Pre => Nkind (N) in N_In,
     Post => Kind (Do_In'Result) = I_Op_And;

   function Make_Memcpy_Function_Call_Expr (Destination : Irep;
                                            Source : Irep;
                                            Num_Elem : Irep;
                                            Element_Type_Size : Uint;
                                            Source_Loc : Source_Ptr)
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
     (Symbol : Irep; Value : Irep; Block : Irep; Source_Loc : Source_Ptr)
     with Pre => Kind (Value) in Class_Expr;

   function Create_Dummy_Irep return Irep;

   function Make_Type_Symbol (Name : Symbol_Id; Defn : Irep) return Symbol;

   function Do_Identifier (N : Node_Id) return Irep
     with Pre  => Nkind (N) in N_Identifier | N_Expanded_Name;

   function Do_Selected_Component (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Selected_Component,
        Post => Kind (Do_Selected_Component'Result) in
          I_Member_Expr | I_Op_Comma;
end Tree_Walk;
