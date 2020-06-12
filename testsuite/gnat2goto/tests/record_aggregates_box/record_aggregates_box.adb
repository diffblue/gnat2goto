procedure Record_Aggregates_Box is
   type R is record
      A, B, C, D : Integer;
   end record;

   V : R := (A => 0, others => <>);
begin
   V := (B => 1, others => <>);
   pragma Assert (V.A = 0);
   pragma Assert (V.B = 1);
end Record_Aggregates_Box;
