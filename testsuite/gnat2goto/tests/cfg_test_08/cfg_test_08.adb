procedure CFG_Test_08 (X : in Integer; I : in out Integer) is
begin
   I := 0;

   while X <= 5 loop
      I := I + 1;
   end loop;

   pragma Assert (I /= 0);
end CFG_Test_08;
