procedure Modular_Type is
  type Less_Than_Ten is mod 10;
  procedure Assert_Val (X : Less_Than_Ten) is
     Y : constant Less_Than_Ten := X + 3;
     Z : constant Less_Than_Ten := X - 1;
  begin
     pragma Assert ((Y and X) = 0);
     pragma Assert ((Z or X) = 5);
     pragma Assert ((Y xor (Y - 1)) = 9);
     pragma Assert ((not X) = 4);
end Assert_Val;
begin
  Assert_Val (5);
end Modular_Type;
