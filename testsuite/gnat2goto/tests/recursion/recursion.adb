procedure Recursion (A : Integer) is
begin
   if (A <= 0) then
      return;
   else
      Recursion (A + 1);
   end if;
end;
