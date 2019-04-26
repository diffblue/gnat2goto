procedure Dynamic_Range_Variable is
   type Static_Range_Type is range 1..100;
   Range_Bound : Static_Range_Type := 50;
   subtype Dynamic_Range_Type is Static_Range_Type range 1..Range_Bound;
   Var : Dynamic_Range_Type := 20;
begin
   Var := 2 * Var; -- to invoke range check
   pragma Assert(Var = 40);
end Dynamic_Range_Variable;
