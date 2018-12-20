procedure Loops is   
   type Cnt is new Integer range  42 .. 45;
   subtype SubInt is Integer range 5 .. 12;
   Z : Integer := 0;
begin
   --  loop with label and unconditional 'exit'
   MyLoop :
   for i in Cnt'Range loop
      if i = Cnt'Last then
         exit MyLoop;
      end if;
      Z := 2;
   end loop MyLoop;

   --  loop with label
   My_Zloop :
   while Z < 10 loop
      Z := 10;
   end loop My_Zloop;

   --  while loop without any label
   while Z > 2 loop
      Z := 1;
   end loop;

   --  while loop without label but with exit
   while Z > 1 loop
      exit;
   end loop;

   for i in 1..10 loop
     pragma Assert(i < 10);
     null;
   end loop;
end Loops;
