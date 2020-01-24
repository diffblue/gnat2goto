with Test;

procedure Use_Package
is
   use Test; --  N_Use_Package_Declaration
   Small_Number : Range_10 := 4;
begin
   pragma Assert (Small_Number * 2 = 8);
end Use_Package;
