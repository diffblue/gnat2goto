package Private_Dec_Extension is
   type Input_Source is abstract tagged limited private;
   type String_Input is new Input_Source with private;
   procedure P (X : in out String_Input);

private
   type Input_Source is abstract tagged limited record
      Prolog_Size : Natural := 0;
   end record;

   type String_Input is new Input_Source with
      record
         Index    : Natural;
      end record;
end Private_Dec_Extension;
