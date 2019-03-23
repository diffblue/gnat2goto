package body Incomplete_Dec_Priv is
   procedure P (X : in out Incomplete_Dec) is
   begin
      X.A := X.A + 1;
   end P;
end Incomplete_Dec_Priv;
