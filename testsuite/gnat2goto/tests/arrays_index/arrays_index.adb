procedure Arrays_Index is
   type Arr is array (1..3) of Integer;
   Actual : Arr := (1,2,3);
begin
   Actual(1) := 1;
   Actual(2) := 11;
   Actual(3) := 3;

   pragma Assert (Actual(2) = 11);
end Arrays_Index;
