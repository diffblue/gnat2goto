procedure Test is
   type Unsigned_16 is mod 2 ** 4;
   RAM_Start : constant Unsigned_16 := 5;
   RAM_End   : constant Unsigned_16 := 12;
   Sum : Unsigned_16 := 0;

   Test_Passed   : Boolean := True;

   subtype Sub16 is Unsigned_16 range RAM_Start .. RAM_End;
   SubVar : Sub16 := RAM_Start;

   function Address_OK
     (Address    : in Unsigned_16) return Boolean;
   function Address_OK
     (Address    : in Unsigned_16)
     return Boolean
   is
   begin
      Sum := Sum + 1;
      return Test_Passed;
   end Address_OK;
begin
   SubVar := SubVar + 1; -- invokes range check
   for I in Unsigned_16 range RAM_Start .. RAM_End loop
      if Address_OK (Address => I)
      then
         Test_Passed := False;
      end if;

   end loop;
   pragma Assert (Sum = RAM_End - RAM_Start + 1);

end Test;
