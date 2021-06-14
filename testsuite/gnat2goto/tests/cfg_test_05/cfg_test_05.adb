procedure CFG_Test_05 (X : in Integer; I : in out Integer) is
begin
   I := 0;

   case X is
      when 1 =>
         I := 1;
      when 2 =>
         I := 2;
      when 3 | 5 =>
         I := 3;
      when others =>
         I := X;
   end case;

   pragma Assert (I /= 0);
end CFG_Test_05;
