procedure Modular_Type is
  type Less_Than_Ten is mod 10;
  procedure Assert_Val (X : Less_Than_Ten) is
  begin
    pragma Assert (X = 5);
  end Assert_Val;
begin
  Assert_Val (5);
end Modular_Type;
