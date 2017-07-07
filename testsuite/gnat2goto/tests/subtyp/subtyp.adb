procedure subtyp is
  type correct is new Integer range 1 .. 2;
  type myint is new Integer;
  x : myint := 2;
  function inc (x : myint) return myint is (x+1);
begin
  x := inc (x);
end subtyp;
