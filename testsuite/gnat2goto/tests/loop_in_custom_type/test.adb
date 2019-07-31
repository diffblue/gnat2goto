procedure Test is
   From : constant := 1;
   To : constant := 5;
   subtype My_Type is Integer range From .. To;
   Iter_Counter : Integer := 0;
begin
   for X in My_Type loop
      pragma Assert (X>4);
      Iter_Counter := Iter_Counter + 1;
   end loop;
   pragma Assert (Iter_Counter = To - From + 1);
end Test;
