with Atree;             use Atree;
with Einfo;             use Einfo;
with Sinfo;             use Sinfo;

with Ireps;             use Ireps;
with Types;             use Types;

with Symbol_Table_Info; use Symbol_Table_Info;

package Arrays is

   type Irep_Array is array (Positive range <>) of Irep;

   function Do_Aggregate_Literal_Array (N : Node_Id) return Irep
     with Pre  => Nkind (N) = N_Aggregate;

   function Make_Array_Default_Initialiser (E : Entity_Id) return Irep;

   function Do_Constrained_Array_Definition (N : Node_Id) return Irep
     with Pre  => Nkind (N) in N_Array_Type_Definition,
     Post => Kind (Do_Constrained_Array_Definition'Result) = I_Struct_Type;

   function Do_Unconstrained_Array_Definition (N : Node_Id) return Irep
     with Pre  => Nkind (N) in N_Array_Type_Definition,
     Post => Kind (Do_Unconstrained_Array_Definition'Result) =
     I_Struct_Type;

   function Do_Array_Assignment (N : Node_Id) return Irep
     with Pre => Nkind (N) = N_Assignment_Statement,
     Post => Kind (Do_Array_Assignment'Result) = I_Code_Assign;

   function Do_Array_Length (N : Node_Id) return Irep
     with Pre => Nkind (N) = N_Attribute_Reference;

   function Get_Array_Component_Type (N : Node_Id) return Entity_Id
     with Post => Is_Type (Get_Array_Component_Type'Result);

   function Do_Slice (N : Node_Id) return Irep
     with Pre  => Nkind (N) = N_Slice;

   function Do_Indexed_Component (N : Node_Id) return Irep
     with Pre  => Nkind (N) = N_Indexed_Component;

   function Get_First_Index_Component (Array_Struct : Irep;
                                       A_Symbol_Table : Symbol_Table)
                                       return Irep;

   function Get_Last_Index_Component (Array_Struct : Irep;
                                      A_Symbol_Table : Symbol_Table)
                                      return Irep;

   function Get_Data_Component (Array_Struct : Irep;
                                A_Symbol_Table : Symbol_Table)
                                return Irep
     with Pre => (Kind (Array_Struct) in Class_Expr
                  and then Kind (Get_Type (Array_Struct)) in
                    I_Symbol_Type | I_Struct_Type),
     Post => Kind (Get_Type (Get_Data_Component'Result)) = I_Pointer_Type;

   function Get_First_Index (Array_Struct : Irep; Source_Loc : Source_Ptr;
                             A_Symbol_Table : Symbol_Table)
                             return Irep
     with Pre => (Kind (Array_Struct) in Class_Expr
                  and then Kind (Get_Type (Array_Struct)) in
                    I_Symbol_Type | I_Struct_Type),
     Post => Kind (Get_First_Index'Result) = I_Member_Expr;

   function Get_Last_Index (Array_Struct : Irep; Source_Loc : Source_Ptr;
                             A_Symbol_Table : Symbol_Table)
                             return Irep
     with Pre => (Kind (Array_Struct) in Class_Expr
                  and then Kind (Get_Type (Array_Struct)) in
                    I_Symbol_Type | I_Struct_Type),
       Post => Kind (Get_Last_Index'Result) = I_Member_Expr;

private
   function Get_Array_Index_Type (N : Node_Id) return Entity_Id
     with Post => Ekind (Get_Array_Index_Type'Result) = E_Signed_Integer_Type;

   function Do_RHS_Array_Assign (N : Node_Id) return Irep_Array
     with Pre => Nkind (N) in N_Op_Concat | N_Slice | N_Function_Call;

   function Do_Array_Range (N : Node_Id) return Irep
     with Pre  => Nkind (N) = N_Range;

   function Make_Array_First_Expr
     (Base_Type : Node_Id; Base_Irep : Irep) return Irep;

   function Make_Array_Index_Op
     (Base_Irep : Irep; Base_Type : Node_Id; Idx_Irep : Irep) return Irep;

end Arrays;
