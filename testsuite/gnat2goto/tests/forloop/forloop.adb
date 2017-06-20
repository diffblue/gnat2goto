procedure Forloop is
  a : Integer := 1;
  type cnt is new Integer range  42 .. 43;
begin
   for i in 1 .. 3 loop
      A := i;
   end loop;

   for i in reverse 1 .. 3 loop
      A := 1;
   end loop;

   for i in cnt'Range loop
      A := 333;
   end loop;
end Forloop;
