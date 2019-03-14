with Uintp; use Uintp;
with Types; use Types;

--  Known Bugs:

--  Currently doesn't actually work with very large floating point constants

--  This is a _very_ sketchy implementation and probably has loads of bugs.
--  This will probably need a full rewrite going forward
package body Ureal_To_Binary is

   --   Convert a Ureal to a binary representation. We write the 'integer' and
   --  'fraction' parts (left and right of the dot if you will) into separate
   --  strings so we don't have to remember which is which in the end
   --
   --   Number: The number to be converted (normalised Ureal)
   --   Int_Part: Where to write the integer bits
   --   Frac_Part: Where to write the fraction bits
   --   Int_End: The index of the last bit we wrote to
   --            in the integer part
   --   frac_end: The index of the last bit we wrote to
   --            in the frac part
   --
   --   Callers are responsible for ensuring the int part and frac part are
   --  large enough for storing the resultant number (easiest way to achieve
   --  this is to just pass large strings)
   procedure Get_Integer_And_Fractional_Parts (Number : Ureal;
                                              Int_Part : out String;
                                              Frac_Part : out String;
                                              Int_End : out Integer;
                                              Frac_End : out Integer)
     with Pre => (Denominator (Number) > Uint_0);

   procedure Get_Integer_And_Fractional_Parts (Number : Ureal;
                                               Int_Part : out String;
                                               Frac_Part : out String;
                                               Int_End : out Integer;
                                               Frac_End : out Integer) is
      --   Current numerator, we change this throughout the algorithm
      Num : Uint := Norm_Num (Number);
      Den : constant Uint := Norm_Den (Number);

      function Get_Next_Exponent return Int;
      procedure Do_Integer_Part;
      procedure Do_Fractional_Part;

      --   Get the smallest E so that 2^E > Num
      function Get_Next_Exponent return Int is
         E : Int := 0;
         Eth_Power_Of_Two : Uint := Uint_1;
      begin
         while (Den * Eth_Power_Of_Two) < Num loop
            E := E + 1;
            Eth_Power_Of_Two := Eth_Power_Of_Two * 2;
         end loop;
         return E;
      end Get_Next_Exponent;

      procedure Do_Integer_Part is
         E : Int := Get_Next_Exponent;
         Mantissa : Uint := Uint_2**E;
         Digit_Position : Integer := Int_Part'First;
      begin
         --  we start with a string filled with 0
         Int_Part := (others => '0');
         --  if n is the smallest integer so that our number is < 2^n, then
         --  we'll need up to n digits (powers of 2 from 2^0 to 2^(n-1))
         if E > Int_Part'Length then
            raise Integer_Part_Too_Large;
         end if;
         --  writing to the first position
         Int_End := Int_Part'First + Integer (E);

         if Int_End > Int_Part'Last then
            raise Integer_Part_Too_Large;
         end if;

         pragma Assert (Int_End <= Int_Part'Last);
         --  use the definition of the binary representation to get the integer
         --  part of the fraction, i.e. our integer part is going to be b_E
         --  b_(E-1) ... b_0 (binary digits) with the integer part of
         --  our number being sum (n <- 0..E) { b_n * 2^n}
         while E >= Uint_0 loop
            pragma Assert (Digit_Position <= Int_End);
            declare Subtrahend : constant Uint := Mantissa * Den;
            begin
               if Subtrahend <= Num then
                  Num := Num - Subtrahend;
                  Int_Part (Digit_Position) := '1';
               end if;

               E := E - 1;
               Digit_Position := Digit_Position + 1;
               Mantissa := Mantissa / 2;
            end;
         end loop;
      end Do_Integer_Part;

      procedure Do_Fractional_Part is
      begin
         pragma Assert (Frac_Part'Length >= 1);
         Frac_Part := (others => '0');
         Frac_End := Frac_Part'First;
         --  basically, use the long form division algorithm,
         --  for example for 6/5

         --   110 / 101 = 1.0011...
         --  -101
         --   ---
         --     10 / 101 (too small)
         --     100 / 101 (too small)
         --     1000 / 101 = 1
         --    -0101
         --     ----
         --       110 / 101 = 1 (and now we're in a loop)
         while Num /= Uint_0 and Frac_End <= Frac_Part'Last loop
            Num := Num * Uint_2;
            if Den <= Num then
               Num := Num - Den;
               Frac_Part (Frac_End) := '1';
            end if;
            Frac_End := Frac_End + 1;
         end loop;
         --  if we ended up running past the end we cut off the fraction
         --  (i.e. round down)
         if Frac_End > Frac_Part'Last then
            Frac_End := Frac_Part'Last;
         end if;
      end Do_Fractional_Part;

   begin
      Do_Integer_Part;
      Do_Fractional_Part;
   end Get_Integer_And_Fractional_Parts;

   function Convert_Ureal_To_Binary_Fixed (Number : Ureal;
                                          Max_Digits : Positive := 30)
                                           return String is
      Int : String (1 .. Max_Digits);
      Frac : String (1 .. Max_Digits);
      Int_End : Integer;
      Frac_End : Integer;
   begin
      Get_Integer_And_Fractional_Parts (Number, Int, Frac, Int_End, Frac_End);
      return Int (Int'First .. Int_End) & "." & Frac (Frac'First .. Frac_End);
   end Convert_Ureal_To_Binary_Fixed;

   function Convert_Ureal_To_Binary_IEEE (Number : Ureal;
                                         Fraction_Bits : Positive;
                                         Exponent_Bits : Positive;
                                         Exponent_Bias : Positive)
                                         return String is
      Is_Negative : constant Boolean := Number < Ureal_0;
      Int_Part : String (1 .. 2**(Exponent_Bits - 1));
      Frac_Part : String (1 .. 2**(Exponent_Bits - 1));
      Abs_Number : constant Ureal := (if Is_Negative then -Number else Number);
      Result : String (1 .. Fraction_Bits + Exponent_Bits + 1)
          := (others => '0');
      Int_End : Integer;
      Frac_End : Integer;
      Exponent : Integer := 0;
      Sign_Bit_Index : constant Integer := 1;
      Exponent_Start_Index : constant Integer := 2;
      Exponent_End_Index : constant Integer :=
      Exponent_Start_Index + Exponent_Bits - 1;
      Fraction_Start_Index : constant Integer := Exponent_End_Index + 1;
      Fraction_End_Index : constant Integer :=
          Fraction_Start_Index + Fraction_Bits - 1;

      --  Do the fraction parts
      --
      --  This NEEDS to happen first, because this is also where we determine
      --  how big the exponent is (or alternatively, where in the mantiss the
      --  "dot" is)
      procedure Do_Fraction;
      procedure Do_Fraction is
         --  the mantissa is somewhere in the 'middle' of the bitfield, so we
         --  need to remember where we are
         Fraction_Bit : Integer := Fraction_Start_Index;
         --  the first one is implicit in the mantissa so we need to remember
         --  to omit it
         First_One : Boolean := True;

         --  copy the 'integer part' to the mantissa doesn't actually work if
         --  the integer part is larger than the mantissa, this is a bug
         procedure Do_Integer_Part;
         procedure Do_Integer_Part is
         begin
            for Bit of Int_Part (Int_Part'First .. Int_End) loop
               if First_One then
                  --  the first 1 in the mantissa is implicit
                  if Bit = '1' then
                     First_One := False;
                  end if;
               else
                  Result (Fraction_Bit) := Bit;
                  Fraction_Bit := Fraction_Bit + 1;
                  Exponent := Exponent + 1;
               end if;
            end loop;
         end Do_Integer_Part;

         procedure Do_Fraction_Part;
         procedure Do_Fraction_Part is
            Is_LT_One : constant Boolean := Abs_Number < Ureal_1;
         begin
            for Bit of Frac_Part (Frac_Part'First .. Frac_End) loop
               if First_One then
                  if Bit = '1' then
                     First_One := False;
                     if not Is_LT_One then
                        Result (Fraction_Bit) := '1';
                        Fraction_Bit := Fraction_Bit + 1;
                     end if;
                  end if;
                  if Is_LT_One then
                     Exponent := Exponent - 1;
                  end if;
               else
                  Result (Fraction_Bit) := Bit;
                  if Fraction_Bit = Fraction_End_Index then
                     exit;
                  else
                     Fraction_Bit := Fraction_Bit + 1;
                  end if;
               end if;
            end loop;
         end Do_Fraction_Part;

      begin
         Do_Integer_Part;
         if Fraction_Bit < Fraction_End_Index then
            Do_Fraction_Part;
         end if;
      end Do_Fraction;

      procedure Do_Exponent;
      procedure Do_Exponent is
         Power : Integer := 2 ** (Exponent_Bits - 1);
         Exponent_Index : Integer := Exponent_Start_Index;
      begin

         if Exponent >= Power * 2 then
            raise Exponent_Too_Large;
         end if;

         if Exponent < 0 then
            raise Negative_Exponent;
         end if;

         while Power > 0 loop
            if Exponent - Power >= 0 then
               Result (Exponent_Index) := '1';
               Exponent := Exponent - Power;
            end if;
            Exponent_Index := Exponent_Index + 1;
            Power := Power / 2;
         end loop;
      end Do_Exponent;
   begin
      if Number = Ureal_0 then
         return Result;
      end if;
      Result (Sign_Bit_Index) := (if Is_Negative then '1' else '0');
      Get_Integer_And_Fractional_Parts (Abs_Number,
                                      Int_Part,
                                      Frac_Part,
                                      Int_End,
                                      Frac_End);
      if Int_End > Fraction_Bits then
         raise Integer_Part_Too_Large;
      end if;

      Do_Fraction;
      Exponent := Exponent + Exponent_Bias;
      Do_Exponent;
      return Result;
   end Convert_Ureal_To_Binary_IEEE;

end Ureal_To_Binary;
