with Atree;             use Atree;
with Einfo;             use Einfo;
with Sinfo;             use Sinfo;

with Ireps;             use Ireps;
with Types;             use Types;

package Arrays is

   type Irep_Array is array (Positive range <>) of Irep;

   function Do_Aggregate_Literal_Array (N : Node_Id) return Irep
     with Pre  => Nkind (N) = N_Aggregate;

   function Make_Array_Default_Initialiser (E : Entity_Id) return Irep;

   function Do_Unconstrained_Array_Definition (N : Node_Id) return Irep
     with Pre  => Nkind (N) in N_Array_Type_Definition,
     Post => Kind (Do_Unconstrained_Array_Definition'Result) =
     I_Struct_Type;

   function Do_Array_Assignment (N : Node_Id) return Irep
     with Pre => Nkind (N) = N_Assignment_Statement,
     Post => Kind (Do_Array_Assignment'Result) = I_Code_Assign;

   function Do_Array_Length (N : Node_Id) return Irep
     with Pre => Nkind (N) = N_Attribute_Reference;

   function Do_Array_Range (N : Node_Id) return Irep
     with Pre  => Nkind (N) = N_Range;

   function Do_RHS_Array_Assign (N : Node_Id) return Irep_Array
     with Pre => Nkind (N) in N_Op_Concat | N_Slice | N_Function_Call;

   function Get_Array_Component_Type (N : Node_Id) return Entity_Id
     with Post => Is_Type (Get_Array_Component_Type'Result);

   function Get_Array_Index_Type (N : Node_Id) return Entity_Id
     with Post => Ekind (Get_Array_Index_Type'Result) = E_Signed_Integer_Type;

   function Make_Array_First_Expr
     (Base_Type : Node_Id; Base_Irep : Irep) return Irep;

   function Make_Array_Index_Op
     (Base_Irep : Irep; Base_Type : Node_Id; Idx_Irep : Irep) return Irep;

   function Do_Slice (N : Node_Id) return Irep
     with Pre  => Nkind (N) = N_Slice;

end Arrays;
