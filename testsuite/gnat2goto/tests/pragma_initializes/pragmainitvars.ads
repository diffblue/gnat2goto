package PragmaInitVars
is
   pragma initializes ((Variable1,Variable2));
   
   Variable1 : Integer := 1;
   Variable2 : Integer := 2;
   Variable3 : Integer := 3;

   procedure Init;
   
end PragmaInitVars;
