procedure More_Pragmas is
   --  The following pragma should not be reported as unsupported.
   pragma Linker_Options ("Dummy");

   pragma Compile_Time_Warning (True, "This should not be reported as unsupported");

   type A1 is array (1 .. 10) of Integer;
   --  The following pragma should not be reported as unsupported.
   pragma Suppress_Initialization (A1);

   type My_Int is range 0 .. 100;
   --  The following pragma should not be reported as unsupported.
   pragma Suppress_Initialization (My_Int);

   type A2 is array (1 .. 10) of Integer with Default_Component_Value => 0;
   --  The following pragma should be reported as unsupported.
   pragma Suppress_Initialization (A2);

   type R is record
      A, B : Integer;
   end record;
    --  The following pragma should be reported as unsupported.
   pragma Suppress_Initialization (R);

   type My_Int_2 is range 0 .. 100 with Default_Value => 0;
    --  The following pragma should be reported as unsupported.
   pragma Suppress_Initialization (My_int_2);

   V : Integer := 0;
    --  The following pragma should be reported as unsupported.
   pragma Suppress_Initialization (V);

   pragma Compile_Time_Warning (False, "This should not be reported as unsupported");
begin
   pragma Compile_Time_Warning (True, "This should not be reported as unsupported");
   pragma Assert (V = 0);
   pragma Compile_Time_Warning (V /= 0, "This should not be reported as unsupported");
end More_Pragmas;
