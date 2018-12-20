procedure Divide is
  A : Integer := 22;
  B : Integer := 11;
  C : Integer;
begin
  C := A / B;
  pragma Assert (C = 2);
end;
