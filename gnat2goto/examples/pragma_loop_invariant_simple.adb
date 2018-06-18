procedure Pragma_Loop_Invariant_Simple is
begin
   for I in Integer range 1 .. 10 loop
      pragma Loop_Invariant (True);
   end loop;
end Pragma_Loop_Invariant_Simple;
