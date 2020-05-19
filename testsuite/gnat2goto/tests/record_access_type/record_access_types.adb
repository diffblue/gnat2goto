procedure Record_Access_Types is
   type R is record
      I : Integer;
   end record;

   type R_Pointer is access all R;

   VR : aliased R;
   VP : R_Pointer;

begin
   VR.I := 5;
   pragma Assert (VR.I = 5);
   VP := VR'Access;
   VP.all.I := 3;
   pragma Assert (VR.I = 5, "This assertion should fail");
   pragma Assert (VR.I = 3);
   pragma Assert (VP.I = 3);

   VP.I := 7;
   pragma Assert (VR.I = 5, "This assertion should fail");
   pragma Assert (VR.I = 7);
   
   VR.I := 11;
   pragma Assert (VP.I = 7, "This assertion should fail");
   pragma Assert (VP.I = 11);

end Record_Access_Types;
