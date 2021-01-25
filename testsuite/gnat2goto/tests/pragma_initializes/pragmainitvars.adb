package body PragmaInitVars
is

   procedure Init 
   is
   begin
   
      pragma Assert (Variable1 = 1);
      pragma Assert (Variable2 = 2);
      pragma Assert (Variable3 = 3);

   end Init;

end PragmaInitVars;
