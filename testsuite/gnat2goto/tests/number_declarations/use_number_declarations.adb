with Number_Declarations;
use Number_Declarations;
procedure Use_Number_Declarations is
   Useless_Int,
   Another_Int : Integer;
   Useless_Float,
   Another_Float : Float;
begin
   --  These two statements should not cause an overflow
   Useless_Int := 1 + Small_Int_Number;
   Another_Int := Useless_Int + Small_Int_Number;
   
   --  This statement should cause an overflow
   Useless_Int := Useless_Int + Large_Int_Number;
   
   --  These two statements should not cause an overflow
   Useless_Float := 1.0 + Small_Real_Number;
   Another_Float := Useless_Float + Small_Real_Number;
   
   --  This statement should cause an overflow
   Useless_Float := Useless_Float + Large_Real_Number;

end Use_Number_Declarations;

