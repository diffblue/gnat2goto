procedure Nondet_Enum_Sub is
   type My_Enum is (one, two, three, four, five, six);

   subtype My_Sub_Enum is My_Enum range two .. four;

   subtype Sub_Int is Integer range  0 .. 10;

   function Nondet_My_Enum return My_Enum
     with Annotate => (ASVAT, Nondet_Function),
     Import;

   function In_Type (V : My_Enum) return Boolean
     with Annotate => (ASVAT, In_Type_Function),
     Import;

   function Nondet_Sub_Int return Sub_Int
     with Annotate => (ASVAT, Nondet_Function),
     Import;

   function In_Type (V : Sub_Int) return Boolean
     with Annotate => (ASVAT, In_Type_Function),
     Import;

   Var_Enum : My_Enum := three;
   Var_Sub_Int : Sub_Int := 5;
begin
   pragma Assert (In_Type (Var_Enum));
   pragma Assert (In_Type (Var_Sub_Int));

   Var_Enum := Nondet_My_Enum;
   Var_Sub_Int := Nondet_Sub_Int;

   pragma Assert (In_Type (Var_Enum));
   pragma Assert (In_Type (Var_Sub_Int));

   pragma Assume (In_Type (Var_Enum));
   pragma Assume (In_Type (Var_Sub_Int));

   pragma Assert (Var_Enum in Var_Enum);
   pragma Assert (Var_Enum >= My_Enum'First and Var_Enum <= My_Enum'Last);
   pragma Assert (In_Type (Var_Sub_Int));
   pragma Assert (Var_Sub_Int in Sub_Int);

end Nondet_Enum_Sub;
