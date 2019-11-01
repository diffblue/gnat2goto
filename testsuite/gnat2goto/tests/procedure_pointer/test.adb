procedure Test is
   Global_Number : Integer;

   procedure Increment_By (Num : Integer) is
   begin Global_Number := Global_Number + Num;
   end Increment_By;

   procedure Decrement_By (Num : Integer) is
   begin Global_Number := Global_Number - Num;
   end Decrement_By;

   type Modifier is access procedure (Num : Integer);

   procedure Assert_Modify(Proc : in not null Modifier) is
   begin
      Proc.all(5);
      pragma Assert (Global_Number = 5);
   end Assert_Modify;

begin
   Global_Number := 0;
   Assert_Modify(Increment_By'Access);
   Global_Number := 0;
   Assert_Modify(Decrement_By'Access);

end Test;
