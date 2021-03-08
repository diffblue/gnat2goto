package body Division_Check_Float is

   procedure Float_Check (val1 : float; Val2 : float) is

      type float1 is digits 7 range 0.0 .. 10.0;

      Fl1 : float1;
      Fl2 : float1;

   begin

      Fl1 := 2.0;
      Fl2 := 2.0;

      Fl1 := float1 (val1) / float1 (Val2);

      --  valid float division
      pragma Assert (Fl1 = 1.0);

      Fl2 := float1 (val1) - float1 (Val2);

      --  add a failure to check
      pragma Assert (false);

      --  this will cause a Constraint Error
      Fl1 := Fl1 / Fl2;

   end Float_Check;

end Division_Check_Float;
