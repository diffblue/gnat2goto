procedure Pointer_To_Slice is
   type Count is range 0 .. 10;
   subtype Index is Count range 1 .. Count'Last;

   type Arr_T is array (Index) of Count;
   type Arr_P is access all Arr_T;

   Arr_1 : aliased Arr_T;
   Arr_2 : aliased Arr_T := (others => 3);
   Ptr_1 : Arr_P;
   Ptr_2 : Arr_P;

begin
   for I in Index loop
      Arr_1 (I) := I;
   end loop;

   for I in Index loop
      pragma Assert (Arr_1 (I) = I);
   end loop;

   Arr_2 (1 .. 5) := Arr_1 (2 .. 6);

   for I in Index range 1 .. 5 loop
      pragma Assert (Arr_2 (I) = I + 1);
   end loop;

   for I in 6 .. Index'Last loop
      pragma Assert (Arr_2 (I) = 3);
   end loop;

   Ptr_1 := Arr_1'Access;
   Ptr_2 := Arr_2'Access;

   for I in Index loop
      pragma Assert (Ptr_1 (I) = I);
   end loop;

  for I in Index range 1 .. 5 loop
      pragma Assert (Ptr_2 (I) = I + 1);
   end loop;

   for I in 6 .. Index'Last loop
      pragma Assert (Ptr_2 (I) = 3);
   end loop;

   Ptr_1 (6 .. Index'Last) := Ptr_2 (6 .. Index'Last);

   for I in Index range 1 .. 5 loop
      pragma Assert (Arr_1 (I) = I);
   end loop;

   for I in 6 .. Index'Last loop
      pragma Assert (Arr_1 (I) = 3);
   end loop;

   Ptr_2 (1 .. 5) := Ptr_1 (1 .. 5);

   for I in Index range 1 .. 5 loop
      pragma Assert (Arr_2 (I) = I);
   end loop;

   for I in 6 .. Index'Last loop
      pragma Assert (Arr_2 (I) = 3);
   end loop;


end Pointer_To_Slice;
