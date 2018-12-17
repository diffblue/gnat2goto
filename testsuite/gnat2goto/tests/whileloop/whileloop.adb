procedure Whileloop is
  a : Integer := 1;
  b : Integer := 10;
  c : constant Integer := 1;
begin
   while A <= B loop
     pragma Assert (A <= B);
      A := A + C;
     pragma Assert (A - C <= B);
   end loop;
   pragma Assert (A <= B);
end;
