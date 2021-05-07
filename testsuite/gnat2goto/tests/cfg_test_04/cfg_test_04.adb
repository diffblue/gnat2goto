procedure CFG_Test_04 (X : in Integer; Y : in Integer; I : in out Integer) is
begin
   I := 0;

   if X = 1 and Y = 1 then
      I := 1;
   elsif X = 2 and then Y = 2 then -- Short cut
      I := 2;
   elsif X = 3 or Y = 3 then
      I := 3;
   elsif X = 4 or else Y = 4 then -- Short cut
      I := 4;
   else
      I := X;
   end if;

   pragma Assert (I /= 0);
end CFG_Test_04;
