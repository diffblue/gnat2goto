procedure Func_Args is
   A : Integer := 1;
   B : Integer := 2;
   C : Integer;

   function Assign2 (D : in out Integer; E : out Integer) return Integer is
   begin
      D := D + 1;
      E := D + 1;
      return E + 1;
   end;

begin
   C := Assign2 (A, B);
   pragma Assert (A = 2);
   pragma Assert (B = 3);
   pragma Assert (C = 4);
   pragma Assert (A > B);
end;
