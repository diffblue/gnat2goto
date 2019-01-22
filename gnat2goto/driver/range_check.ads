with Uintp;                  use Uintp;
with Types;                  use Types;

with Ada.Containers.Vectors; use Ada.Containers;
package Range_Check is

   type Bound_Type is new Uint;

   package Integer_Bounds_Vector is new
     Ada.Containers.Vectors (Index_Type   => Natural,
                             Element_Type => Bound_Type);

   function Store_Bound (Number : Bound_Type) return Integer;

   Integer_Bounds_Table : Integer_Bounds_Vector.Vector;

   function Load_Bound_In_Hex (Index : Integer; Bit_Width : Pos) return String
     with Pre => Integer (Integer_Bounds_Table.Length) >= Index;

end Range_Check;
