package body Incomplete_Dec is
   procedure P (X : in out Partial_Dec) is
   begin
      X.A := X.A + 1;
   end P;
end Incomplete_Dec;
