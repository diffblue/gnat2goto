procedure Division_Check is

   type float1 is digits 7 range 0.0 .. 10.0;

   Int1 : Integer;
   Int2 : Integer;
   Fl1 : float1;
   Fl2 : float1;

begin

   Fl1 := 2.0;
   Fl2 := 2.0;

   Fl1 := Fl1 / Fl2;

   pragma Assert (Fl1 = 1.0);

   Fl2 := 0.0;

   --  this will cause a Constraint Error
   Fl1 := Fl1 / Fl2;

   Int1 := 2;
   Int2 := 2;

   Int1 := Int1 / Int2;

   pragma Assert (Int1 = 1);

   Int2 := 0;

   --  this will cause a Constraint Error
   Int1 := Int1 / Int2;

   Int1 := 3;
   Int2 := 2;

   Int1 := Int1 rem Int2;

   pragma Assert (Int1 = 1);

   Int2 := 0;

   --  this will cause a Constraint Error
   Int1 := Int1 rem Int2;

   Int1 := 3;
   Int2 := 2;

   Int1 := Int1 mod Int2;

   pragma Assert (Int1 = 1);

   Int2 := 0;

   --  this will cause a Constraint Error
   Int1 := Int1 mod Int2;

end Division_Check;
