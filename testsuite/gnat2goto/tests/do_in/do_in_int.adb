procedure Do_In_Int is
   type Small_Int is range 1 .. 6;

  procedure Inc (E : in out Small_Int) is
   begin
      --  This assertion should succeed.
      pragma Assert (E in Small_Int);
      --  Should get a check failure here if E = 6.
      --  TJJ 23/06/20 - cbmc - check does fail when E = 6.
      E := E + 1;
   end Inc;

   procedure Dec (E : in out Small_Int) is
   begin
      --  This assertion should succeed.
      pragma Assert (E in Small_Int);
      --  Should get a check failure here if E = .
      --  TJJ 04/01/21 - cbmc - check does fail when E = 1.
      E := E - 1;
   end Dec;

   I : Small_Int;

begin
   I := 6;
   Inc (I);
   --  TJJ 23/06/20 - Assertion seems to succeed. Is it because the
   --  range check at line 10?
   pragma Assert (I in Small_Int);

   I := 1;
   Dec (I);
   --  TJJ 23/06/20 - Assertion seems to succeed. Is it because the
   --  range check at line 19?
   pragma Assert (I in Small_Int);

   I := 3;
   Inc (I);
   --  Assertion should succeed - and does.
   pragma Assert (I in Small_Int);

   Dec (I);
   --  Assertion should succeed - and does.
   pragma Assert (I in Small_Int);

   --  Assertion should succeed - and does.
   pragma Assert (I = 3);
end Do_In_Int;
