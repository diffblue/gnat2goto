procedure Absolute is
   A : Integer := -22;
   B : Integer;
begin
   B := abs A;
   pragma Assert (B >= 0);
end Absolute;
