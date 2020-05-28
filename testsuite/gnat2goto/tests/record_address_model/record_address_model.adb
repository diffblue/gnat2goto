with System;  use type System.Address;
with System.Address_To_Access_Conversions;
procedure Record_Address_Model is

   type My_Int is range 0 .. 100;
   type My_Record is record
      A , B : My_Int;
   end record;

   type Pointer is access all My_Record;

   package Intrinsic_Address_To_Access_Conversions is new
     System.Address_To_Access_Conversions (My_Record);
   use Intrinsic_Address_To_Access_Conversions;

   V : My_Record;
   PV : Pointer;
begin
   V.A := 3;
   V.B := 5;
   PV := Pointer (To_Pointer (V'Address));

   pragma Assert (PV.all.A = 3);
   pragma Assert (PV.all.B = 5);

   PV.all.A := 7;
   PV.all.B := 11;

   pragma Assert (V.A = 7);
   pragma Assert (V.B = 11);

   pragma Assert (V'Address = To_Address (Object_Pointer (PV)));
end Record_Address_Model;
