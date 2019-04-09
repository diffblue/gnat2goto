package body Private_Dec is
   procedure P (X : in out Dec) is
   begin
      X.A := X.A + 1;
   end P;
end Private_Dec;
