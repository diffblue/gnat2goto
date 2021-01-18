package body PragmaInitNull
is

   procedure Init 
   is
   begin
   
      pragma Assert (Variable1 = 1);
      pragma Assert (Variable2 = 2);

   end Init;

end PragmaInitNull;
