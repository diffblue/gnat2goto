procedure Test is
   Too_Small : exception;
   procedure More_Than_Five (Num : Integer) is
   begin
      if Num <= 5 then
         raise Too_Small with "way too small!";
      end if;
   end More_Than_Five;

   Error_Count : Integer := 0;
begin
   begin
      More_Than_Five (10);
   exception
      when Too_Small =>
         Error_Count := Error_Count + 1;
   end;
   begin
      More_Than_Five (5);
   exception
      when Too_Small =>
         Error_Count := Error_Count + 1;
   end;
   --  this will be unreachable due to implementation of `raise`
   pragma Assert (Error_Count = 1);
end Test;
