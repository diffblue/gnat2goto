procedure Func is
   a : Integer := 1;
   b : Integer := 2;
   c : Integer;
   function Assign2 return Integer is begin
      return A + B;
   end;
begin
   C := Assign2;
end;
