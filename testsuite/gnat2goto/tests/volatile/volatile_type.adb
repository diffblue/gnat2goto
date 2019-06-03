procedure Volatile_Type is
   Data: Integer;
   pragma Volatile(Data);
   Flag: Boolean;
   pragma Volatile(Flag);
begin
   Data := 42;
   Flag := True;

   if not Flag then
      Data := 0;
   end if;

   pragma Assert (Data=42);

end Volatile_Type;
