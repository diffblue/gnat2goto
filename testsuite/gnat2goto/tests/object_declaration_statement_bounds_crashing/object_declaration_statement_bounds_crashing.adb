procedure Object_Declaration_Statement_Bounds_Crashing is
  procedure Callit (A : Integer) is
    -- Base Index type
    type Range_Type is range 0 .. 79;
    Index : Range_Type := 0;
  begin
    if A > 10 then
      for I in Range_Type range (Index + 1) .. Range_Type'Last
      loop
        pragma Assert (I < 70);
      end loop;
      -- Intentional false assert
      pragma Assert (A < -5);
    end if;
  end Callit;
begin
  Callit(12);
end Object_Declaration_Statement_Bounds_Crashing;
