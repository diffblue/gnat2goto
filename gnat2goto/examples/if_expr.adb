procedure If_Expr (B : Boolean) is
  A : Integer;
begin
   A := (if B then 2 elsif B then 1 else 0);
end If_Expr;
