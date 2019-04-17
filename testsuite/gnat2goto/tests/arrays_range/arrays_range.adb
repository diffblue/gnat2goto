procedure Arrays_Range is
   type Indet_Array is array(Integer range <>) of Integer;

   procedure Array_Consumer(Arr : Indet_Array) is
      subtype My_Index is Integer range Arr'Range;
      An_Index : My_Index := 1;
   begin
      pragma Assert(An_Index = 1);
   end Array_Consumer;

   My_Arr : Indet_Array(1..2) := (others => 0);
begin
   Array_Consumer(My_Arr);
end Arrays_Range;
