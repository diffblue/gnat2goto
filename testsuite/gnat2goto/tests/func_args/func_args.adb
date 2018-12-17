procedure Func_Args is
   a : Integer := 1;
   b : Integer := 2;
   c : Integer;
   function Assign2 (D : Integer; E : Integer) return Integer is begin
      return D + E;
   end;
begin
   C := Assign2 (A, B);
   pragma Assert (C = 3);
   pragma Assert (Assign2 (5, 6) = 12);
end;
