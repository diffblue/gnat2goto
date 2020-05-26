procedure Nondet_Int is
   type My_Int is range -500 .. 500;

   V : My_Int := 7;

   function Nondet return My_Int
     with Annotate => (ASVAT, Nondet_Function),
     Import;

   function In_Type (M : My_Int) return Boolean
     with Annotate => (ASVAT, In_Type_Function),
     Import;

begin
   pragma Assert (V >= My_Int'First and V <= My_Int'Last);
   pragma Assert (V in My_Int);
end Nondet_Int;
