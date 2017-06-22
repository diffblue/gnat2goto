procedure Pragmas is
   i : Integer;
begin
   i := 1;

   ------------
   -- Assert --
   ------------

   pragma Assert (Check => i > 1, Message => "not good");   
   pragma Assert (i > 2);
 
   --  Rejected by frontend already ("params out of order")
   --  pragma Assert (Message => "too big", Check => i > 3);

end Pragmas;
