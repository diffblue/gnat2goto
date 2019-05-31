package body Private_Dec is
   procedure P (X : in out Dec) is
   begin
      X.A := X.A + 1;
   end P;
   procedure Q (X : in out Integer) is
      X_Dec : Dec;
   begin
      X_Dec.A := X;
      P(X_Dec);
      X := X_Dec.A;
   end Q;
end Private_Dec;
