--  This example requires a configuration file (gnat.adc)
--  which contains the configuration pragma
--  pragma Extend_System (Aux_Num);
--  which logically extends system so that the entities declared in
--  the package declaration of System.Aux_Num are visible when
--  package System is visible.
--  Hence wthe with and use clauses below.
with System; use System;
procedure Types_2 is
   TYPE id_nums IS
      RECORD
         num1 : numeric.types.uinteger8s;
         num2 : numeric.types.uinteger8s;
         num3 : numeric.types.uinteger8s;
         num4 : numeric.types.uinteger8s;
      END RECORD;
   pragma Pack (id_nums);
begin
   pragma Assert (id_nums'size = 32);
end Types_2;
