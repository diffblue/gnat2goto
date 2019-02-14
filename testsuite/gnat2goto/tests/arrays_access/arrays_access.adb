procedure Arrays_Access is
   type Arr7 is array (1 .. 3) of Integer;
   Actual7 : Arr7 := (1,2,3);
begin
   Actual7(1) := 2;
   pragma Assert(Actual7(1) = 2);
end Arrays_Access;
