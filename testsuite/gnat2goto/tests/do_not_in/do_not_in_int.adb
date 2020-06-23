procedure Do_Not_In_Int is
   subtype Small_Int is Integer range 1 .. 6;

  procedure Inc (E : in out Integer) is
   begin
      pragma Assert (E in Small_Int);
      E := E + 1;
   end Inc;

   procedure Dec (E : in out Integer) is
   begin
      pragma Assert (E in Small_Int);
      E := E - 1;
   end Dec;

   I : Integer;

begin
   I := 6;
   Inc (I);
   pragma Assert (I not in Small_Int);

   I := 1;
   Dec (I);
   pragma Assert (I not in Small_Int);

   I := 3;
   Inc (I);
   pragma Assert (I in Small_Int);

   Dec (I);
   pragma Assert (I in Small_Int);

   pragma Assert (I = 3);
end Do_Not_In_Int;
