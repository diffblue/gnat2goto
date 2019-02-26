procedure Arrays_Assign_Nonoverlap is
   type Arr_Long is array (1..4) of Integer;
   Full_Arr : Arr_Long := (1,2,3,4);
   Part1_Arr : Arr_Long := (1,2,0,0);
   Part2_Arr : Arr_Long := (3,4,0,0);
begin
   Full_Arr := Part2_Arr(1..2) & Part2_Arr(1..0) & Part1_Arr(1..2);
   pragma Assert (Full_Arr(1)=3);
   pragma Assert (Full_Arr(2)=4);
   pragma Assert (Full_Arr(3)=1);
   pragma Assert (Full_Arr(4)=2);
end Arrays_Assign_Nonoverlap;
