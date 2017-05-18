
procedure Record_Init is
   type Unconst_Array is array (Positive range <>) of Integer;
   type R1 (Len : Positive := 10) is record
      A : Integer := 2;
      B : Unconst_Array (1 .. Len);
   end record;
   type R2 is record
      A : Integer := 10;
      B : Positive := 20;
   end record;
   type R3 (Has_Array1 : Boolean := True; Array2_Max : Integer := 10) is record
      Arr2 : Unconst_Array (6 .. Array2_Max);
      case Has_Array1 is
         when True =>
            Arr1 : Unconst_Array (1 .. 5);
         when False =>
            Prim1 : Integer := 5;
      end case;
   end record;
   Inst1 : R1 := (Len => 3, B => (1, 2, 3), others => <>);
   Inst2 : R2;
   Inst3 : R1;
   Inst4 : R1 (5);
   Inst5 : R3;
   Inst6 : R3 (False, 7);
begin
   null;
end Record_Init;
