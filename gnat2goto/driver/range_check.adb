with Binary_To_Hex;         use Binary_To_Hex;
with Types;                 use Types;
with GOTO_Utils;             use GOTO_Utils;

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

   ----------------------
   -- Store_Real_Bound --
   ----------------------

   function Store_Real_Bound (Number : Bound_Type_Real) return Integer
   is
      Length : constant Integer := Integer (Integer_Bounds_Real_Table.Length);
   begin
      Integer_Bounds_Real_Table.Append (Number);
      return Length;
   end Store_Real_Bound;

   -----------------------
   -- Load_Bound_In_Hex --
   -----------------------

   function Load_Bound_In_Hex (Index : Integer; Actual_Type : Irep)
                               return String
   is
      Bit_Width : constant Pos := Pos (Get_Width (Actual_Type));
      Bound : constant Uint := Uint (Integer_Bounds_Table.Element (Index));
   begin
      return Convert_Uint_To_Hex (
                 Value     => Bound,
                 Bit_Width => Bit_Width);
   end Load_Bound_In_Hex;

   ----------------------------
   -- Load_Real_Bound_In_Hex --
   ----------------------------

   function Load_Real_Bound_In_Hex (Index : Integer; Actual_Type : Irep)
                                    return String
   is
      Bit_Width : constant Float_Format := To_Float_Format (Actual_Type);
      Bound : constant Ureal :=
        Ureal (Integer_Bounds_Real_Table.Element (Index));
   begin
      case Bit_Width is
         when IEEE_32_Bit => return Convert_Ureal_To_Hex_32bits_IEEE (Bound);
         when IEEE_64_Bit => return Convert_Ureal_To_Hex_64bits_IEEE (Bound);
      end case;
   end Load_Real_Bound_In_Hex;
end Range_Check;
