with Private_Type; use Private_Type;
procedure Derived_Types is
   type My_Int is range -23 .. 23;
   type My_Real is new Float;
   type Derived_Priv is new Priv;
begin
   pragma Assert (0 in My_Int);
   pragma Assert (11.0 in My_Real);
   pragma Assert (My_Int'Succ (22) = 23);
   null;
end Derived_Types;
