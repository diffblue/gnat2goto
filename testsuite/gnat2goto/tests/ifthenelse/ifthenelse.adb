procedure Ifthenelse is
  A : Integer := 1;
  B : Integer := 2;
  C : Integer;
begin
  if A /= B then
    C := A + B;
  else
    C := A - B;
  end if;
  pragma Assert (C = 3);
end;
