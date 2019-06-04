package body Incomplete_Dec_Priv is
   procedure P (X : in out Incomplete_Dec) is
   begin
      X.A := X.A + 1;
   end P;
   procedure Q (X : in out Integer) is
      X_Rec : Incomplete_Dec;
   begin
      X_Rec.A := X;
      P(X_Rec);
      X := X_Rec.A;
   end Q;
end Incomplete_Dec_Priv;
