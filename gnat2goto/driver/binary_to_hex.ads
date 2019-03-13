with Uintp; use Uintp;
with Urealp; use Urealp;
with Types; use Types;
package Binary_To_Hex is

   function Convert_Binary_To_Hex (Binary : String) return String with
     Pre => (Binary'Length mod 4 = 0);

   function Convert_Uint_To_Hex (Value : Uint; Bit_Width : Pos) return String
     with Pre => (Bit_Width mod 4 = 0);

   function Convert_Ureal_To_Hex_IEEE
     (Value : Ureal;
      Fraction_Bits : Positive;
      Exponent_Bits : Positive;
      Exponent_Bias : Positive) return String
     with Pre => ((Fraction_Bits + Exponent_Bits + 1) mod 4 = 0);

   function Convert_Ureal_To_Hex_32bits_IEEE (Value : Ureal) return String;
   function Convert_Ureal_To_Hex_64bits_IEEE (Value : Ureal) return String;
end Binary_To_Hex;
