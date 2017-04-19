procedure Mixed_Parameters is
   a : Integer := 1;
   b : Integer := 2;
   c : Integer;
   function Mixed (D : Integer; E : Integer) return Integer is begin
      return D - E;
   end;
begin
   C := Mixed (A, B);
   C := Mixed (D => C, E => B);
   C := Mixed (E => C, D => B);
   C := Mixed (A, E => C);
end;
