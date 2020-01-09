procedure Main is
   type Day_Of_Month is range 1 .. 31;
   type Day_Of_Month_Access is access all Day_Of_Month;

   -- funny case where the subtype-indication has subtype-mark
   type Restricted_Day_Of_Month_Access is access all Day_Of_Month range 10 .. 15;

   A : aliased Day_Of_Month;
   Ptr1 : Day_Of_Month_Access := A'Access;
   Ptr2 : Day_Of_Month_Access := A'Access;

   Restricted_A : aliased Day_Of_Month range 10 .. 15 := 12;
   Ptr3 : Restricted_Day_Of_Month_Access := Restricted_A'Access;
begin
   Ptr1.all := 2;
   pragma Assert (A=2); -- should succeed
   Ptr2.all := 5;
   pragma Assert (Ptr1.all=4); -- should fail

   Ptr3.all := 11;
   pragma Assert (Restricted_A = 11); -- should succeed
end;
