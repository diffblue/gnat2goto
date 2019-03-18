--  NB our package implementation does not support function calls atm, so
--  although 'use type' can work fully if only built-in operators are used,
--  any use of functions or overloaded operators or new 'basic operations'
--  for a custom type will fail.
----------------------------------------------------------------------

--  'use type' clause makes visible only the operators associated with a type
--  (NB but not the type itself, this must still be fully qualified
with Count_Types; use type Count_Types.Count;

--  'use all type' clause makes visible both the operators and primitive
--  operations associated with a type (NB but not the type itself, this must
--  still be fully qualified)
with Count_Types; use all type Count_Types.Counter;

procedure Use_Type_Clause is
   A : Count_Types.Count := 1;

   B : Count_Types.Counter :=1;
begin
   pragma Assert (A=1);

   B := Double(B);  --  Double is visible due to the 'use all type' clause
   pragma Assert(B=2);
end Use_Type_Clause;
