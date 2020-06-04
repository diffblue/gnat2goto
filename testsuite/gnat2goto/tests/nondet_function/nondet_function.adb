procedure Nondet_Function is
   type My_Int is range -500 .. 500;

   subtype My_Sub_Int is My_Int range 0 .. 100;

   type My_Enum is (one, two, three, four, five, six);

   subtype My_Sub_Enum is My_Enum range two .. four;

   subtype Index is Integer range 1 .. 10;

   type Fixed_Array is array (Index) of My_Sub_Int;

   type Rec is record
      A : My_Sub_Int;
      B : My_Enum;
      I : Index;
      D : Fixed_Array;
   end record;

   function Nondet_My_Int return My_Int
     with Annotate => (ASVAT, Nondet_Function),
     Import;

   function Nondet_My_Sub_Int return My_Sub_Int
     with Annotate => (ASVAT, Nondet_Function),
     Import;

   function Nondet_My_Enum return My_Enum
     with Annotate => (ASVAT, Nondet_Function),
     Import;

   function Nondet_My_Sub_Enum return My_Sub_Enum
     with Annotate => (ASVAT, Nondet_Function),
     Import;

   function Nondet_Integer return Integer
     with Annotate => (ASVAT, Nondet_Function),
     Import;

   function Nondet_Index return Index
     with Annotate => (ASVAT, Nondet_Function),
     Import;

   function In_Type (V : My_Int) return Boolean
     with Annotate => (ASVAT, In_Type_Function),
     Import;

   function In_Type_Sub (V : My_Sub_Int) return Boolean
     with Annotate => (ASVAT, In_Type_Function),
     Import;

   function In_Type (V : My_Enum) return Boolean
     with Annotate => (ASVAT, In_Type_Function),
     Import;

   function In_Type_Sub (V : My_Sub_Enum) return Boolean
     with Annotate => (ASVAT, In_Type_Function),
     Import;

   function In_Type (I : Index) return Boolean
     with Annotate => (ASVAT, In_Type_Function),
     Import;

   Var_My_Int : My_Int := 3;
   Var_My_Sub_Int : My_Sub_Int := 5;

   Var_My_Enum : My_Enum := one;
   Var_My_Sub_Enum : My_Sub_Enum := three;

   Var_Fixed_Array : Fixed_Array := (others => 7);
   Var_Rec : Rec := (11,
                     five,
                     1,
                     (others => 13));
 begin
   pragma Assert (Var_My_Int = 3);
   pragma Assert (Var_My_Sub_Int = 5);

   pragma Assert (Var_My_Enum = one);
   pragma Assert (Var_My_Sub_Enum = three);

   for I in Index loop
      pragma Assert (Var_Fixed_Array (I) = 7);
   end loop;

   pragma Assert (Var_Rec.A = 11);
   pragma Assert (Var_Rec.B = five);
   pragma Assert (Var_Rec.I = 1);
   for I in Index loop
      pragma Assert (Var_Rec.D (I) = 13);
   end loop;

   pragma Assert (Var_My_Int >= My_Int'First and Var_My_Int <= My_Int'Last);
   pragma Assert (Var_My_Sub_Int in My_Sub_Int);

   pragma Assert (Var_My_Enum in My_Enum);
   pragma Assert (Var_My_Sub_Enum in My_Sub_Enum);

   for I in Index loop
      pragma Assert (Var_Fixed_Array (I) in My_Sub_Int);
   end loop;

   pragma Assert (Var_Rec.A in My_Sub_Int);
   pragma Assert (Var_Rec.B in My_Enum);
   pragma Assert (Var_Rec.I in Index);
   for I in Index loop
      pragma Assert (Var_Rec.D (I) in My_Sub_Int);
   end loop;

   pragma Assert (In_Type (Var_My_Int));
   pragma Assert (In_Type_Sub (Var_My_Sub_Int));
   pragma Assert (In_Type (Var_My_Enum));
   pragma Assert (In_Type_Sub (Var_My_Sub_Enum));

  for I in Index loop
      pragma Assert (In_Type_Sub (Var_Fixed_Array (I)));
   end loop;

   pragma Assert (In_Type_Sub (Var_Rec.A));
   pragma Assert (In_Type (Var_Rec.B));
   pragma Assert (In_Type (Var_Rec.I));
   for I in Index loop
      pragma Assert (In_Type_Sub (Var_Rec.D (I)));
   end loop;

   Var_My_Int := Nondet_My_Int;
   pragma Assert (Var_My_Int = 3);

   Var_My_Sub_Int := Nondet_My_Sub_Int;
   pragma Assert (Var_My_Sub_Int = 5);

   Var_My_Enum := Nondet_My_Enum;
   pragma Assert (Var_My_Enum = one);

   Var_My_Sub_Enum := Nondet_My_Sub_Enum;
   pragma Assert (Var_My_Sub_Enum = three);

   for I in Var_Fixed_Array'Range loop
      Var_Fixed_Array (I) := Nondet_My_Sub_Int;
   end loop;

   for I in Index loop
      pragma Assert (Var_Fixed_Array (I) = 7);
   end loop;

   Var_Rec :=
     (Nondet_My_Sub_Int, Nondet_My_Enum, Nondet_Index,
      (others => Nondet_My_Sub_Int));

   pragma Assert (Var_Rec.A = 11);
   pragma Assert (Var_Rec.B = five);
   pragma Assert (Var_Rec.I = 1);
   for I in Index loop
      pragma Assert (Var_Rec.D (I) = 13);
   end loop;

   pragma Assert (Var_My_Int >= My_Int'First and Var_My_Int <= My_Int'Last);
   pragma Assert (Var_My_Sub_Int in My_Sub_Int);

   pragma Assert (Var_My_Enum in My_Enum);
   pragma Assert (Var_My_Sub_Enum in My_Sub_Enum);

   for I in Index loop
      pragma Assert (Var_Fixed_Array (I) in My_Sub_Int);
   end loop;

   pragma Assert (Var_Rec.A in My_Sub_Int);
   pragma Assert (Var_Rec.B in My_Enum);
   pragma Assert (Var_Rec.I in Index);
   for I in Index loop
      pragma Assert (Var_Rec.D (I) in My_Sub_Int);
   end loop;

   pragma Assume (In_Type (Var_My_Int));
   pragma Assume (In_Type_Sub (Var_My_Sub_Int));
   pragma Assume (In_Type (Var_My_Enum));
   pragma Assume (In_Type_Sub (Var_My_Sub_Enum));

  for I in Index loop
      pragma Assume (In_Type_Sub (Var_Fixed_Array (I)));
   end loop;

   pragma Assume (In_Type_Sub (Var_Rec.A));
   pragma Assume (In_Type (Var_Rec.B));
   pragma Assume (In_Type (Var_Rec.I));
   for I in Index loop
      pragma Assume (In_Type_Sub (Var_Rec.D (I)));
   end loop;

   pragma Assert (Var_My_Int >= My_Int'First and Var_My_Int <= My_Int'Last);
   pragma Assert (Var_My_Sub_Int in My_Sub_Int);

   pragma Assert (Var_My_Enum >= My_Enum'First and Var_My_Enum <= My_Enum'Last);
   pragma Assert (Var_My_Sub_Enum in My_Sub_Enum);

   for I in Index loop
      pragma Assert (Var_Fixed_Array (I) in My_Sub_Int);
   end loop;

   pragma Assert (Var_Rec.A in My_Sub_Int);
   pragma Assert (Var_Rec.B in My_Enum);
   pragma Assert (Var_Rec.I in Index);
   for I in Index loop
      pragma Assert (Var_Rec.D (I) in My_Sub_Int);
   end loop;

end Nondet_Function;
