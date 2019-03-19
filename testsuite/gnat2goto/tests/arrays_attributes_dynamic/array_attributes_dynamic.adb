-- covers first, last, and length
procedure Array_Attributes_Dynamic is
   type Indet_Array is array(Integer range <>) of Integer;

   procedure Array_Consumer(Arr : Indet_Array) is
   begin
      pragma Assert(Arr'First = 1);
      pragma Assert(Arr'Last = 1); -- should fail
      pragma Assert(Arr'Length=2);
   end Array_Consumer;

   My_Arr : Indet_Array(1..2) := (others => 0);
begin
   Array_Consumer(My_Arr);
end Array_Attributes_Dynamic;
