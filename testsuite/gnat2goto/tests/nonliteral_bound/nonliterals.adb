procedure Nonliterals is
   subtype Range1 is Integer range 10 .. 20;

   Actual1 : Range1 := 11;

   subtype Range2 is Integer range -10 .. 20;

   Actual2 : Range2 := -5;

   Result : Integer;
begin
   Result := Range1'First + Range1'Last + Actual1;

   pragma Assert (Result = 41);

   Result := Range2'First + Range2'Last + Actual2;

   pragma Assert (Result = 5);
end Nonliterals;
