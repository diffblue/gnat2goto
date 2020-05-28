with System;  use type System.Address;
with System.Address_To_Access_Conversions;
--  with System.Address_Image;
--  with Text_IO; use Text_IO;
procedure Simple_Address_Model is

   type My_Int is range 0 .. 100;
   type Pointer is access all My_Int;

   package Intrinsic_Address_To_Access_Conversions is new
     System.Address_To_Access_Conversions (My_Int);
   use Intrinsic_Address_To_Access_Conversions;

   V : My_Int;
   PV : Pointer;
begin
   V := 3;
   PV := Pointer (To_Pointer (V'Address));

--     Put_Line ("PV = " & My_Int'Image (PV.all));
   pragma Assert (PV.all = 3);

   PV.all := 5;

--     Put_Line ("V = " & My_Int'Image (V));
   pragma Assert (V = 5);

--     Put_Line ("V'Address = " & System.Address_Image (V'Address));
--     Put_Line ("PV points to " &
--                 System.Address_Image (To_Address (Object_Pointer (PV))));
   pragma Assert (V'Address = To_Address (Object_Pointer (PV)));
end Simple_Address_Model;
