
procedure Record_Init is
   type Unconst_Array is array (Positive range <>) of Integer;
   type R1 (Len : Positive) is record
      A : Integer := 2;
      B : Unconst_Array (1 .. Len);
   end record;
   type R2 is record
      A : Integer := 10;
      B : Positive := 20;
   end record;
   Inst1 : R1 := (Len => 3, B => (1, 2, 3), others => <>);
   Inst2 : R2;
begin
   null;
end Record_Init;
