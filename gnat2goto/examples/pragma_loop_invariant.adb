procedure Pragma_Loop_Invariant is
   N : Integer := 1;
   M : Integer := 2;
begin
   for I in Integer range 1 .. 10 loop
      N := N + 1;
      pragma Loop_Invariant (N = M);
      M := M + 1;
   end loop;
end Pragma_Loop_Invariant;
