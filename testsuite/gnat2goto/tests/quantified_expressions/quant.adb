procedure Quant is
   N : Integer := 88;
begin
   pragma Assert (for some X in 2 .. N / 2 => N mod X = 0);
end;