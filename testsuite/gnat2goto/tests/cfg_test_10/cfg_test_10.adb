procedure CFG_Test_10 (X : in Integer; I : in out Integer) is
begin
   I := 1;

   for J in Integer range 1 .. 10 loop
      I := I * J;
   end loop;

   pragma Assert (I > 0);
end CFG_Test_10;
