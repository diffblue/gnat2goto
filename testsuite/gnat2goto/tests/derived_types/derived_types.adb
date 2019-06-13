procedure Derived_Types is
   type My_Int is range -23 .. 23;
   type My_Real is new Float;
   
begin
   pragma Assert (0 in My_Int);
   pragma Assert (11.0 in My_Real);
   pragma Assert (My_Int'Succ (22) = 23);
   null;
end Derived_Types;
