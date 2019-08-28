procedure Test is
   type Unsigned_8 is mod 2 ** 4;
   subtype s10_index is Unsigned_8 range 1 .. 10;
   length : Unsigned_8 := 4;
   j : s10_index;
begin
   j := 1;
   for k in s10_index range 1 .. length loop
      j := j + 1;
   end loop;
   pragma Assert (j < 4);

end Test;
