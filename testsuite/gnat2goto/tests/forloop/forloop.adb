procedure Forloop is
  A : Integer := 0;
  type Cnt is new Integer range  42 .. 43;
begin
   for I in 1 .. 3 loop
      A := A + I;
   end loop;

   pragma Assert (A = 6);

   for I in Cnt'Range loop
      if I = 43 then
        A := 333;
      end if;
   end loop;
end Forloop;
