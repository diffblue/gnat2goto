with Binary_To_Hex;         use Binary_To_Hex;

package body Range_Check is

   -----------------
   -- Store_Bound --
   -----------------

   function Store_Bound (Number : Bound_Type) return Integer
   is
      Length : constant Integer := Integer (Integer_Bounds_Table.Length);
   begin
      Integer_Bounds_Table.Append (Number);
      return Length;
   end Store_Bound;

   -----------------------
   -- Load_Bound_In_Hex --
   -----------------------

   function Load_Bound_In_Hex (Index : Integer; Bit_Width : Pos) return String
   is
      Bound : constant Uint := Uint (Integer_Bounds_Table.Element (Index));
   begin
      return Convert_Uint_To_Hex (
                 Value     => Bound,
                 Bit_Width => Bit_Width);
   end Load_Bound_In_Hex;

end Range_Check;
