procedure Pointer_To_Array is
   type Count is range 0 .. 10;
   subtype Index is Count range 1 .. Count'Last;

   type Arr_T is array (Index) of Count;
   type Arr_P is access all Arr_T;

   type U_Arr_T is array (Integer range <>) of Integer;
   type U_Arr_P is access all U_Arr_T;

   Arr : aliased Arr_T;
   Ptr : Arr_P;

   U_Arr : aliased U_Arr_T := (1, 2, 3, 4);
   U_Ptr : U_Arr_P;

   C_dummy : Count;

begin
   for I in Index loop
      Arr (I) := Index'Last - I + 1;
   end loop;

   for I in Index loop
      pragma Assert (Arr (I) = Index'Last - I + 1);
   end loop;

   Ptr := Arr'Access;

   for I in Index loop
      pragma Assert (Ptr (I) = Index'Last - I + 1);
   end loop;

   for I in Index loop
      Arr (I) := I;
   end loop;

   for I in Index loop
      pragma Assert (Ptr (I) = I);
   end loop;

   for I in Index loop
      pragma Assert (Arr (I) = I);
   end loop;

   pragma Assert (Arr (3) = 5);
   pragma Assert (Ptr (5) = 3);
   pragma Assert (Ptr.all (3) = 3);
   pragma Assert (Ptr.all (3) = 5);

  for I in Index loop
      Ptr.all (I) := Index'Last - I + 1;
   end loop;

   for I in Index loop
      pragma Assert (Ptr.all (I) = Index'Last - I + 1);
   end loop;

   for I in Index loop
      pragma Assert (Arr (I) = Index'Last - I + 1);
   end loop;

   pragma Assert (Arr (1) = Index'Last);
   pragma Assert (Ptr (Index'Last) = 1);
   pragma Assert (Ptr.all (1) = Index'Last);
   pragma Assert (Ptr.all (Index'Last) = 1);

   pragma Assert (Ptr'Length = Index'Last);

   U_Ptr := U_Arr'Access;
   C_dummy := U_Ptr'Length;
   C_dummy := Ptr'Length;

   C_Dummy := Count (U_Ptr'First);
   C_Dummy := Count (U_Ptr'Last);


end Pointer_To_Array;
