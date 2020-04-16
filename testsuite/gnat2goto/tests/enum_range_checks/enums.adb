procedure Enums is
   type My_Enum is (one, two, three, four, five);

   subtype Enum_Sub is My_Enum range two .. four;

   VE : My_Enum;
   VS : Enum_Sub;
   T  : My_Enum;
begin
   VE := one;
   VS := two;
   T  := VE;
   VE := VS;
   VS := VE;
   VE := T;
   pragma Assert (VE in My_Enum);
   pragma Assert (VE >= My_Enum'First and VE <= My_Enum'Last);
   pragma Assert (VE >= Enum_Sub'First and VE <= Enum_Sub'Last);
   pragma Assert (VS in My_Enum);
   pragma Assert (VS >= My_Enum'First and VS <= My_Enum'Last);
   pragma Assert (VS >= Enum_Sub'First and VS <= Enum_Sub'Last);
   pragma Assert (VE <= VS);
   pragma Assert (VE <= My_Enum'Last);
   pragma Assert (VE >= My_Enum'First);
   pragma Assert (VE in Enum_Sub);
   pragma Assert (VE >= Enum_Sub'First);
   pragma Assert (VE = one);
   pragma Assert (VE = T);
   pragma Assert (VS = two);
   pragma Assert (T = one);
end Enums;
