procedure Access_Parameter is

   procedure P (A : access Integer) is
   begin
      A.all := A.all + 1;
   end P;

   I : aliased Integer := 1;
begin
   P (I 'Access);
   pragma Assert (I = 2);
end Access_Parameter;
