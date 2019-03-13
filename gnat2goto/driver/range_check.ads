with Uintp;                  use Uintp;
with Urealp;                 use Urealp;
with Ireps;                  use Ireps;

with Ada.Containers.Vectors; use Ada.Containers;
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
end Range_Check;
