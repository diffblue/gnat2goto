procedure Nonliterals is
   subtype Range1 is Integer range 10 .. 20;

   Actual1 : Range1 := 11;

   Result : Integer;
begin
   Result := Range1'First + Range1'Last + Actual1;

   pragma Assert (Result = 41);
end Nonliterals;
