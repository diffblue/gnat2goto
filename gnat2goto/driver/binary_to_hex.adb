with Uint_To_Binary; use Uint_To_Binary;
with Ureal_To_Binary; use Ureal_To_Binary;

package body Binary_To_Hex is
   function Strip_Leading_Zeroes (Str : String) return String;

   function Convert_Binary_To_Hex (Binary : String) return String is
      type Hex_Digit_Pos is mod 16;
      --  this needs to be uppercase for CBMC
      type Hex_Digit is ('0', '1', '2', '3', '4',
                         '5', '6', '7', '8', '9',
                         'A', 'B', 'C', 'D', 'E',
                         'F');
      subtype Binary_Quartet_T is String (1 .. 4);

      function Convert_Binary_To_Hex_Digit (Binary_Quartet : Binary_Quartet_T)
                                           return Hex_Digit;

      function Convert_Binary_To_Hex_Digit (Binary_Quartet : Binary_Quartet_T)
                                           return Hex_Digit is
         Pos : Hex_Digit_Pos := 0;
      begin
         for Digit of Binary_Quartet loop
            Pos := Pos * 2;
            if Digit = '1' then
               Pos := Pos + 1;
            else
               pragma Assert (Digit = '0');
            end if;
         end loop;
         return Hex_Digit'Val (Pos);
      end Convert_Binary_To_Hex_Digit;

      Binary_Length_Diff : constant Integer := Binary'Length mod 4;
      Zero_Filled_Binary : String (1 .. Binary'Length + Binary_Length_Diff);
      Result : String (1 .. ((Zero_Filled_Binary'Length / 4)));
   begin
      if Binary_Length_Diff > 0 then
         Zero_Filled_Binary (1 .. Binary_Length_Diff) := (others => '0');
      end if;
      Zero_Filled_Binary (1 + Binary_Length_Diff .. Zero_Filled_Binary'Last)
        := Binary;
      for I in Result'Range loop
         declare
            Binary_Quartet_Start : constant Integer :=
              Zero_Filled_Binary'First +
              --  -1 because I starts at 1
              (I - 1) * 4;
            Binary_Quartet_End : constant Integer := Binary_Quartet_Start + 3;
            Binary_Quartet : constant Binary_Quartet_T := Zero_Filled_Binary
              (Binary_Quartet_Start .. Binary_Quartet_End);
         begin
            Result (I) := Character'Value (Hex_Digit'Image
              (Convert_Binary_To_Hex_Digit (Binary_Quartet)));
         end;
      end loop;
      return Strip_Leading_Zeroes (Result);
   end Convert_Binary_To_Hex;

   function Convert_Uint_To_Hex (Value : Uint; Bit_Width : Pos) return String
   is begin
      return Convert_Binary_To_Hex (
        Convert_Uint_To_Binary (Value, Bit_Width));
   end Convert_Uint_To_Hex;

   --  This function should not be used directly: used the wrappers below
   --  instead.
   --  See e.g. https://en.wikipedia.org/wiki/IEEE_754 for more info on the
   --  fraction and exponent bits and the exponent bias; and the conversion
   --  algorithm in general.
   function Convert_Ureal_To_Hex_IEEE
     (Value : Ureal;
      Fraction_Bits : Positive;
      Exponent_Bits : Positive;
      Exponent_Bias : Positive) return String
   is begin
      return Convert_Binary_To_Hex (
       Convert_Ureal_To_Binary_IEEE
         (Value,
          Fraction_Bits,
          Exponent_Bits,
          Exponent_Bias));
   end Convert_Ureal_To_Hex_IEEE;

   --  Convert Ada real to IEEE float in HEX (as a string)
   function Convert_Ureal_To_Hex_32bits_IEEE (Value : Ureal) return String
   is begin
      return Convert_Ureal_To_Hex_IEEE (Value         => Value,
                                        Fraction_Bits => 23,
                                        Exponent_Bits => 8,
                                        Exponent_Bias => 127);
   end Convert_Ureal_To_Hex_32bits_IEEE;

   --  Convert Ada real to IEEE double in HEX (as a string)
   function Convert_Ureal_To_Hex_64bits_IEEE (Value : Ureal) return String
   is begin
      return Convert_Ureal_To_Hex_IEEE (Value         => Value,
                                        Fraction_Bits => 52,
                                        Exponent_Bits => 11,
                                        Exponent_Bias => 1023);
   end Convert_Ureal_To_Hex_64bits_IEEE;

   function Strip_Leading_Zeroes (Str : String) return String is
   begin
      for Ix in Str'Range loop
         if Str (Ix) /= '0' then
            return Str (Ix .. Str'Last);
         end if;
      end loop;
      return "0";
   end Strip_Leading_Zeroes;
end Binary_To_Hex;
