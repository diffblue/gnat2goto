procedure CFG_Test_01 (X : in Boolean; I : in out Integer) is
begin
   I := 0;

   if X then
      I := 1;
   end if;

   pragma Assert (I /= 0);
end CFG_Test_01;
