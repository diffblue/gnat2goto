procedure Divide is
  a : Integer := 1;
  b : Integer := 2;
  c : Integer;
begin
  c := a / b;
  pragma Assert (c >= 0);
end;
