package body Incomplete_Dec is
   procedure Inc (X : in out Partial_Dec) is
   begin
      X.A := X.A + 1;
   end Inc;
end Incomplete_Dec;
