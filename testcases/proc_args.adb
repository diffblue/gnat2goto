procedure Proc_Args is
   a : Integer := 1;
   b : Integer := 2;
   c : Integer;
   procedure Assign2 (D : Integer; E : Integer) is begin
      C := D + E;
   end;
begin
   Assign2 (A, B);
end;
