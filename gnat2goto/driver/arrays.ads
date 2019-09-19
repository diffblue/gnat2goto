with Atree;             use Atree;
with Einfo;             use Einfo;
with Sinfo;             use Sinfo;

with Ireps;             use Ireps;
with Types;             use Types;

with Symbol_Table_Info; use Symbol_Table_Info;
with GOTO_Utils;        use GOTO_Utils;

package Arrays is

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

   function Do_Array_First (N : Node_Id) return Irep
     with Pre => Nkind (N) = N_Attribute_Reference;

   function Do_Array_Last (N : Node_Id) return Irep
     with Pre => Nkind (N) = N_Attribute_Reference;

   function Get_Array_Component_Type (N : Node_Id) return Entity_Id
     with Post => Is_Type (Get_Array_Component_Type'Result);

   function Do_Slice (N : Node_Id) return Irep
     with Pre  => Nkind (N) = N_Slice;

   function Do_Indexed_Component (N : Node_Id) return Irep
     with Pre  => Nkind (N) = N_Indexed_Component;

   function Get_Data_Component_From_Type (Array_Struct_Type : Irep)
                                          return Irep
     with Pre => Kind (Array_Struct_Type) in I_Struct_Type,
     Post => Kind (Get_Type (Get_Data_Component_From_Type'Result))
       in I_Pointer_Type;

   function Get_First_Index (Array_Struct : Irep) return Irep
     with Pre => (Kind (Array_Struct) in Class_Expr
                  and then Kind (Get_Type (Array_Struct)) in
                    I_Symbol_Type | I_Struct_Type),
     Post => Kind (Get_First_Index'Result) = I_Member_Expr;

   function Get_Last_Index (Array_Struct : Irep) return Irep
     with Pre => (Kind (Array_Struct) in Class_Expr
                  and then Kind (Get_Type (Array_Struct)) in
                    I_Symbol_Type | I_Struct_Type),
     Post => Kind (Get_Last_Index'Result) = I_Member_Expr;

   function Get_Data_Member (Array_Struct : Irep;
                             A_Symbol_Table : Symbol_Table)
                             return Irep
     with Pre => (Kind (Array_Struct) in Class_Expr
                  and then Kind (Get_Type (Array_Struct)) in
                    I_Symbol_Type | I_Struct_Type),
       Post => Kind (Get_Data_Member'Result) = I_Member_Expr;

   function Offset_Array_Data (Base : Irep; Offset : Irep) return Irep
     with Pre => (Kind (Base) in Class_Expr
                  and then Kind (Offset) in Class_Expr),
     Post => Kind (Offset_Array_Data'Result) in Class_Expr;

private

   function Do_RHS_Array_Assign (N : Node_Id) return Irep_Array
     with Pre => Nkind (N) in N_Subexpr;

   function Make_Array_First_Expr
     (Base_Type : Node_Id; Base_Irep : Irep) return Irep;

   function Make_Array_Index_Op
     (Base_Irep : Irep; Idx_Irep : Irep) return Irep;

   function Build_Array_Size (First : Irep; Last : Irep; Idx_Type : Irep)
                              return Irep
     with Pre => (Kind (First) in Class_Expr
                  and Kind (Last) in Class_Expr
                  and Kind (Idx_Type) in Class_Type),
     Post => Kind (Build_Array_Size'Result) = I_Op_Add;

   function Get_First_Index_Component (Array_Struct : Irep)
                                       return Irep;

   function Get_Last_Index_Component (Array_Struct : Irep) return Irep;

   function Get_Data_Component (Array_Struct : Irep;
                                A_Symbol_Table : Symbol_Table) return Irep
     with Pre => (Kind (Array_Struct) in Class_Expr
                  and then Kind (Get_Type (Array_Struct)) in
                    I_Symbol_Type | I_Struct_Type),
     Post => Kind (Get_Type (Get_Data_Component'Result)) = I_Pointer_Type;

end Arrays;
