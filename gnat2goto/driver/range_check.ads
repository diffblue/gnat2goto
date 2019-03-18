with Uintp;                     use Uintp;
with Urealp;                    use Urealp;
with Ada.Containers.Vectors;    use Ada.Containers;

with Ireps;                     use Ireps;
with Types;                     use Types;

package Range_Check is

   type Bound_Type is new Uint;
   type Bound_Type_Real is new Ureal;

   package Integer_Bounds_Vector is new
     Ada.Containers.Vectors (Index_Type   => Natural,
                             Element_Type => Bound_Type);
   package Integer_Bounds_Real_Vector is new
     Ada.Containers.Vectors (Index_Type   => Natural,
                             Element_Type => Bound_Type_Real);

   function Store_Bound (Number : Bound_Type) return Integer;
   function Store_Real_Bound (Number : Bound_Type_Real) return Integer;

   function Make_Range_Assert_Expr (N : Node_Id; Value : Irep;
                                    Bounds_Type : Irep) return Irep;

   function Make_Range_Expression (Value_Expr : Irep; Val_Type : Irep)
                                   return Irep
     with Post => Kind (Make_Range_Expression'Result) in Class_Expr;

private
   Integer_Bounds_Table : Integer_Bounds_Vector.Vector;
   Integer_Bounds_Real_Table : Integer_Bounds_Real_Vector.Vector;

   function Load_Bound_In_Hex (Index : Integer; Actual_Type : Irep)
                               return String
     with Pre => (Kind (Actual_Type) = I_Bounded_Signedbv_Type
                  and then
                    Integer (Integer_Bounds_Table.Length) >= Index);

   function Load_Real_Bound_In_Hex (Index : Integer; Actual_Type : Irep)
                                    return String
     with Pre => (Kind (Actual_Type) = I_Bounded_Floatbv_Type
                  and then
                    Integer (Integer_Bounds_Real_Table.Length) >= Index);

   --  might be best if this is moved to a utility package in future
   --  atm placement documents it is only used by range_check
   function Make_Assert_Call (N : Node_Id; Assertion : Irep;
                              Description : Irep) return Irep;

end Range_Check;
