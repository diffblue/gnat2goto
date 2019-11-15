procedure Test is
   Large_Positive : Integer := Integer'Last;
   Larger_Still : Integer;

   function Double (Num : Integer) return Integer
   is
   begin
      -- will raise (overflow) exception
      return 2 * Num;
   end;

   Error_Count : Integer := 0;
begin
   begin
      Larger_Still := Double (Large_Positive);
   exception
      when Constraint_Error =>
         -- intentionally dead-coded (exception handling)
         Error_Count := Error_Count + 1;
   end;
   --  this will be unreachable due to implementation of `raise`
   pragma Assert (Error_Count = 1);
end Test;
