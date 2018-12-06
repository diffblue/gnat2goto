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
      Fraction_Bits : Positive := 23;
      Exponent_Bits : Positive := 8;
      Exponent_Bias : Positive := 127) return String
     with Pre => ((Fraction_Bits + Exponent_Bits + 1) mod 4 = 0);
end Binary_To_Hex;
