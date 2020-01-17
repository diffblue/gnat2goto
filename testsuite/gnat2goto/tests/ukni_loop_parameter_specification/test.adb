procedure Test is

   type Unsigned_16 is mod 2 ** 16;
   for Unsigned_16'Size use 16;

   RAM_Start : constant Unsigned_16 := 16#0200#;
   RAM_End   : constant Unsigned_16 := 16#2199#;

begin
   for I in Unsigned_16 range RAM_Start .. RAM_End loop
      pragma Assert (I < 16#2200#);
   end loop;
end Test;
