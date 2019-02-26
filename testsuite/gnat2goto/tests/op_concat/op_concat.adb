procedure Op_Concat is
   type Arr is array (1..4) of Integer;
   Actual : Arr := (1,2,3,4);

   function Identity (Some_Array : Arr) return Arr
   is
   begin
     return Some_Array;
   end Identity;
begin

   Actual := Identity (Actual (3 .. 4) & Actual (1 .. 2));
   pragma Assert (Actual(2)=4);
end Op_Concat;
