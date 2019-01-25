with Test; use Test;
--  without "use" just "with" does not introduce the operators associated with
--  the type from with-ed package
procedure Package_Type
is
   Small_Number : Range_10 := 4;
begin
   pragma Assert (Small_Number * 2 = 8);
end Package_Type;
