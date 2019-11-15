procedure Main is

   procedure Fail(Val : Integer);
   pragma No_Return(Fail);

   procedure Fail(Val : Integer) is
      Generic_Failure : exception;
   begin
      raise Generic_Failure with "Generic Failure";
   end Fail;

   Val : Integer := 5;
begin
   if (Val = 5)
   then
     pragma Assert (Val = 5);
   else
      Fail(Val);
   end if;
end Main;
