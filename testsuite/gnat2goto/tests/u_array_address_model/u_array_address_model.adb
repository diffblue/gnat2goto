with System;  use type System.Address;
with System.Address_To_Access_Conversions;
procedure U_Array_Address_Model is

   type My_Int is range 0 .. 100;
   type My_U_Array is array (Positive range  <>) of My_Int;

   type Pointer is access all My_U_Array;

   package Intrinsic_Address_To_Access_Conversions is new
     System.Address_To_Access_Conversions (My_U_Array);
   use Intrinsic_Address_To_Access_Conversions;

   V : My_U_Array (1 ..10);
   PV : Pointer;
begin
   for I in V'Range loop
      V (I) := My_Int (I);
   end loop;

   PV := Pointer (To_Pointer (V'Address));

   for I in V'Range loop
      pragma Assert (PV.all (I) = My_Int (I));
   end loop;

   for I in V'Range loop
      PV.all (I) := My_Int (V'Last - I + 1);
   end loop;

   for I in V'Range loop
      pragma Assert (V (I) = My_Int (V'Last - I + 1));
   end loop;

   pragma Assert (V'Address = To_Address (Object_Pointer (PV)));
end U_Array_Address_Model;
