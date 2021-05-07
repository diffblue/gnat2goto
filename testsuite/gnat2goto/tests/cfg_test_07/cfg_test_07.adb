procedure CFG_Test_07 (X : in Integer; I : in out Integer) is
begin
   I := 0;

   loop  -- Infinite loop
      I := I + X;
   end loop;

   pragma Assert (I /= 0);
end CFG_Test_07;
