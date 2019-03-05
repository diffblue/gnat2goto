procedure op_or is
  A : boolean := true;
  B : boolean := false;
  C : boolean := true;
begin
  pragma Assert (A or B);
  pragma Assert (B or A);
  pragma Assert (A or A);
  pragma Assert (B or B);
  pragma Assert (C or A or B);
end op_or;

