procedure Modular_Type is
  type Less_Than_Ten is mod 10;
  procedure Assert_Val (X : Less_Than_Ten) is
     Y : constant Less_Than_Ten := X - 1;
  begin
     pragma Assert (X = 5);

     pragma Assert (X + 3 = 8);
     pragma Assert (X + 6 = 1);

     pragma Assert ((X + 7) * X = 0);
     pragma Assert (X * 2 = 0);
     pragma Assert (X * 3 = 5);
     pragma Assert (X * X = 5);
     pragma Assert (X * 4 = 0);
     pragma Assert (Y * 1 = 4);
     pragma Assert (Y * 2 = 8);
     pragma Assert (Y * 3 = 2);
     pragma Assert (Y * 4 = 6);
     pragma Assert (Y * 5 = 0);
     pragma Assert (Y * 6 = 4);
     pragma Assert (Y * 7 = 8);
     pragma Assert (Y * 8 = 2);
     pragma Assert (Y * 9 = 6);

     pragma Assert (X - 6 = 9);

     pragma Assert (X / 2 = 2);
end Assert_Val;
begin
  Assert_Val (5);
end Modular_Type;
