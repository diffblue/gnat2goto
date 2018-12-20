procedure subtyp is
  type MyInt is new Integer;
  x : MyInt := 2;
  function Inc (X : MyInt) return MyInt is (X+1);
begin
  X := Inc (X);
  pragma Assert (X = 3);
end subtyp;
