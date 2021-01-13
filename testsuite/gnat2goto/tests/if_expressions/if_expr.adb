procedure If_Expr is
   A : Integer := 1;
   B : Integer := (if A = 2 then 4 else 5);
   R : Integer;
begin
   R := (if A = 1 then 2 else 3);

   pragma Assert (R = 2);
   pragma Assert (B = 5);
end;