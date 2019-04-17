procedure Modular_Type_Support is
  type Unsigned_64 is mod 2**64;
  A : constant Unsigned_64 := 0;
begin
  pragma Assert (A = 0);
  pragma Assert (A /= 10);
end Modular_Type_Support;
