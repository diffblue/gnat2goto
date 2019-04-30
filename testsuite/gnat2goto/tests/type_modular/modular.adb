procedure Modular is
  type Eight_Bit is mod 256;
  function Next (X : Eight_Bit) return Eight_Bit is (X + 1);
  Byte : constant Eight_Bit := 255;

  type Two_Bit is mod 4;
  function Next (X : Two_Bit) return Two_Bit is (X + 1);
  Number : constant Two_Bit := 3;

  type Unsigned_64 is mod 2**64;
  function Next (X : Unsigned_64) return Unsigned_64 is (X + 1);
  Zero_Unsigned : constant Unsigned_64 := 0;
  Max_Minus_One : constant Unsigned_64 := 2**64 - 1;
begin
  pragma Assert (Zero_Unsigned = 0);
  pragma Assert (Next (Zero_Unsigned) = 1);
  pragma Assert (Zero_Unsigned /= 10);
  pragma Assert (Next (Zero_Unsigned - 1) = 0);
  pragma Assert (Next (Max_Minus_One) = 0);
  pragma Assert (Next (Byte) = 0);
  pragma Assert (Next (Next (Byte)) = 1);
  pragma Assert (Next (Number) = 0);
  pragma Assert (Next (Next (Number)) = 1);
end Modular;
