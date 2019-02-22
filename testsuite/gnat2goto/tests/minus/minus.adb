procedure Minus is
   A : Integer := -22;
   B : Integer;
begin
   B := -A;
   pragma Assert (B = 22);
end Minus;
