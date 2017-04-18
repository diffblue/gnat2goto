procedure Ifthenelse is
  a : Integer := 1;
  b : Integer := 2;
  c : Integer;
begin
  if (a = b) then
    c := a + b;
  else
    c := a - b;
  end if;
end;
