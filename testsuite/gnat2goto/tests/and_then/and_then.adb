procedure And_Then is
  X : Boolean := False;
  Y : Boolean := True;
  N : Integer := 0;
begin
  --  CBMC: SUCCESS
  if X and then (10/N = N) then
    pragma Assert(False);
  end if;

  --  CBMC: SUCCESS
  if Y and then (10/10 = 1) then
    pragma Assert(True);
  end if;

  --  CBMC: FAILURE
  if Y and then (10/N = N) then
  	pragma Assert(False);
  end if;
end And_Then;
