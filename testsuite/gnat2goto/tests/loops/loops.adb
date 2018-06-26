procedure Loops is   
   type cnt is new Integer range  42 .. 45;

   subtype subint is Integer range 5 .. 12;

   type weekday is (Mo, Tue, We, Th, Fr, Sa, Su);
   
   z : Integer := 0;
   i1 : Integer; -- must not conflict with loop

begin
	
   --  loop with no label (implicit target for 'exit')
   for i in reverse 1 .. 3 loop
      z := 1;
      exit when i = 2;
   end loop;
   
   --  loop with label and unconditional 'exit'
   myloop : 
   for i in cnt'Range loop
      if i = cnt'Last then
         exit myloop;
      end if;
      z := 2;
   end loop myloop;

   -- for z in weekday'Range loop
   --    i1 := 42;
   -- end loop;

   --  loop with label
   my_zloop :
   while z < 10 loop
      z := 10;
   end loop my_zloop;

   --  while loop without any label
   while z > 2 loop
      z := 1;
   end loop;

   --  while loop without label but with exit
   while z > 1 loop
      exit;
   end loop;

   for i in 1..10 loop
     pragma assert(i < 10);
     null;
   end loop;
end Loops;
