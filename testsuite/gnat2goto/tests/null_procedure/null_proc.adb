procedure Null_Proc is
   procedure Do_Nothing (X : in out Integer) is
   begin
      null;
   end;

   A : Integer := 1;
begin
   Do_Nothing (A);

   pragma Assert (A = 1);
end;
