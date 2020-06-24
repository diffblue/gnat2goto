procedure for_loop_var_bound is
   Count, Lower, Upper : Integer;
begin
   Count := 0;
   Lower := 1;
   Upper := 3;
   for I in Integer range 1 .. Upper loop
      if I in 1 .. Upper then
         Count := Count + 1;
      end if;
   end loop;

   pragma Assert (Count = Upper);

   Count := 0;

   for I in Integer range Lower .. 4 loop
      if I in Lower .. 4 then
         Count := Count + 1;
      end if;
   end loop;

   pragma Assert (Count = 4);

   Count := 0;
   Upper := 5;

   for I in Integer range Lower .. Upper loop
      if I in Lower .. Upper then
         Count := Count + 1;
      end if;
   end loop;

   pragma Assert (Count = Upper);

   Count := 0;
   Lower := 5; Upper := 9;

   for I in Integer range Lower .. upper loop
      if I in 5 .. 9 then
         Count := Count + 1;
      end if;
   end loop;

   pragma Assert (Count = Upper - Lower + 1);

   Count := 0;

   for I in Integer range Lower .. Upper loop
      if I in 5 .. 9 then
         Lower := Lower + 1;
         Upper := Upper + 1;
         Count := Count + 1;
      end if;
   end loop;

   pragma Assert (Lower = 5 + Count);
   pragma Assert (Upper = 9 + Count);

   Count := 0;
   Lower := 0;

   for I in Lower + 1 .. Lower + 3 loop
      if I in 1 .. 3 then
         Count := Count + 1;
      end if;
   end loop;

   pragma Assert (Count = 3);

   Count := 0;
   Lower := 1;
   Upper := 3;
   for I in reverse Integer range 1 .. Upper loop
      if I in 1 .. Upper then
         Count := Count + 1;
      end if;
   end loop;

   pragma Assert (Count = Upper);

   Count := 0;

   for I in reverse Integer range Lower .. 4 loop
      if I in Lower .. 4 then
         Count := Count + 1;
      end if;
   end loop;

   pragma Assert (Count = 4);

   Count := 0;
   Upper := 5;

   for I in reverse Integer range Lower .. Upper loop
      if I in Lower .. Upper then
         Count := Count + 1;
      end if;
   end loop;

   pragma Assert (Count = Upper);

   Count := 0;
   Lower := 5; Upper := 9;

   for I in reverse Integer range Lower .. Upper loop
      if I in Lower .. Upper then
         Count := Count + 1;
      end if;
   end loop;

   pragma Assert (Count = Upper - Lower + 1);

   Count := 0;

   for I in reverse Integer range Lower .. Upper loop
      if I in 5 .. 9 then
         Lower := Lower + 1;
         Upper := Upper + 1;
         Count := Count + 1;
      end if;
   end loop;

   pragma Assert (Count = 5);
   pragma Assert (Lower = 5 + Count);
   pragma Assert (Upper = 9 + Count);

   Count := 0;
   Lower := 0;

   for I in reverse Lower + 1 .. Lower + 3 loop
      if I in 1 .. 3 then
         Count := Count + 1;
      end if;
   end loop;

   pragma Assert (Count = 3);

end for_loop_var_bound;
