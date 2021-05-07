procedure CFG_Test_03 (X : in Integer; I : in out Integer) is
begin
   I := 0;

   if X = 1 then
      I := 1;
   elsif X = 2 then
      I := 2;
   elsif X = 3 then
      I := 3;
   else
      I := X;
   end if;

   pragma Assert (I /= 0);
end CFG_Test_03;
