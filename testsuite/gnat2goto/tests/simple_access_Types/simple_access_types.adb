procedure Simple_Access_Types is
   type A_Int is access all Integer;

   I : aliased Integer := 5;

   Acc : A_Int;

begin
   pragma Assert (I = 5);
   Acc := I'Access;
   Acc.all := 3;
   pragma Assert (I = 5, "This assertion should fail");
   pragma Assert (I = 3);

end Simple_Access_Types;
