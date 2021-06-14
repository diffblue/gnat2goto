procedure CFG_Test_09 (X : in Integer; I : in out Integer) is
begin
   I := 0;

   if X > 0 then
      loop
         exit when I - X > 20;

         I := I + 1;

         exit when I >= 5;
      end loop;
   else
      return;
   end if;

   pragma Assert (I /= 0);
end CFG_Test_09;
