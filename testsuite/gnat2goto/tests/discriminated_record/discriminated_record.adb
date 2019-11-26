procedure Discriminated_Record is
   type Uarray is array (Positive range <>) of Integer;
   type Nested_Record (Size3 : Positive) is record
      D : Uarray (1 .. Size3);
   end record;
   type Discriminated_Record (Size1 : Positive; Size2 : Positive) is record
      A : Uarray (1 .. Size1);
      B : Uarray (5 .. Size2);
      C : Nested_Record (Size1);
   end record;
   Inst1 : Discriminated_Record (10, 20);
   function F (X : Discriminated_Record) return Positive is begin
      return X.A'Length;
   end F;
begin
   Inst1.A (5) := 1;
   Inst1.B (10) := 2;

   pragma Assert (Inst1.A (5) = 1);
   Inst1.B (6) := 3;
   pragma Assert (Inst1.A (5) = 1);
end Discriminated_Record;
