procedure Func_Args is
   a : Integer := 1;
   b : Integer := 2;
   c : Integer;
   function Assign2 (D : in out Integer; E : out Integer) return Integer is begin
      D := D + 1;
      E := D;
      return D;
   end;
begin
   C := Assign2 (A, B);
end;
