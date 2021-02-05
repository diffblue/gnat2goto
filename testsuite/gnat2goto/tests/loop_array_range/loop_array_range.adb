procedure Loop_Array_Range is
   type Indet_Array is array(Integer range <>) of Integer;

   My_Arr : Indet_Array(1..10) := (1,2,3,4, others => 0);
   Sum : Integer := 0;
begin
   for I in My_Arr'Range loop
      Sum := Sum + My_Arr(I);
   end loop;

   pragma Assert(Sum=10);
   
   Sum := Sum + My_Arr (My_Arr'Last);
   --  This assertion should fail
   pragma Assert (Sum /= 10);
end Loop_Array_Range;
