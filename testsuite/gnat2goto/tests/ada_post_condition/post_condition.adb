procedure Post_Condition is

   type count is range 1 .. 100;
   
   function Double (value : count) return count
     with Post => Double'Result <=20;
   
   function Double (value : count) return count
   is
   begin
      return value * 2;
   end Double;

   procedure Triple (value : count; output : out count)
     with Pre => value <= 10,
     Post => output <=20 and then output = value * 3;

   procedure Triple (value : count; output : out count)
   is 
   begin
      output := value * 3;
   end Triple;
   
   procedure Triple_Update (value : in out count; 
                            update : in Boolean;
                            output : out count)
     with Pre => value <= 10,
     Post => output <=20 and then output = value'Old * 3;

   procedure Triple_Update (value : in out count; 
                            update : in Boolean;
                            output : out count)
   is 
   begin
      output := value * 3;
      if update then
         value := output;
      end if;
   end Triple_Update;
   
   I : count := 8;
   
   J : count := 3;

   K : count;
   
   Bool_1 : Boolean;
   Bool_2 : Boolean;

begin

   --  post condition will pass
   I := Double (I);
   
   pragma Assert (I = 16);
   
   if Bool_1 then
      --  post condition will fail
      I := Double (I);
      pragma Assert (False);  -- will report SUCCESS
   end if;
   
   --  post condition will pass
   Triple (J,J);
   
   pragma Assert (J = 9);

   if Bool_2 then
      --  post condition will fail
      Triple (J,J);
      pragma Assert (False);  -- will report SUCCESS
   end if;
   
   J := 3;

   -- post condition will pass
   Triple_Update (J, True, K);

   pragma Assert (J = 9);
   pragma Assert (K = 9);

   -- check assert fail
   pragma Assert (False);  -- will report FAILURE

    -- post condition will fail
   Triple_Update (J, True, K);  
   pragma Assert (False);  -- will report SUCCESS

end Post_Condition;
