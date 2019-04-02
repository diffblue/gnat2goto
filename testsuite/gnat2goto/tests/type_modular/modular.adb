procedure Modular is
  type Eight_Bit is mod 256;
  function Next (X : Eight_Bit) return Eight_Bit is (X + 1);
  Byte : constant Eight_Bit := 255;

  type Two_Bit is mod 4;
  function Next (X : Two_Bit) return Two_Bit is (X + 1);
  Number : constant Two_Bit := 3;
begin
  pragma Assert (Next (Byte) = 0);
  pragma Assert (Next (Next (Byte)) = 1);
  pragma Assert (Next (Number) = 0);
  pragma Assert (Next (Next (Number)) = 1);
end Modular;
