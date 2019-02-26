procedure Arrays_Concat is
   type Arr is array (1..4) of Integer;
   Actual : Arr := (1,2,3,4);
begin

   Actual (1 .. 4) := Actual (3 .. 4) & Actual (1 .. 2);
   pragma Assert (Actual(2)=4);
end Arrays_Concat;
