procedure ReverseP is
  A : Integer;
begin
  for I in reverse 1 .. 3 loop
    A := I;
  end loop;
  pragma Assert (A = 1);
end ReverseP;
