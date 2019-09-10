with Other; use Other;
use Other.SubOther;

procedure Test is
   Length : Other.Unsigned_8 := 4;
   Width_1 : Other.SubOther.Unsigned_16 := 10;
   Width_2 : Other.SubOther.Unsigned_16 := 11;
begin

   pragma Assert (Width_1 > Width_2);

end Test;
