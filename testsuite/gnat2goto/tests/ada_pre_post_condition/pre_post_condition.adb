procedure Post_Condition is

   type count is range 1 .. 20;
   
   function Double (value : count) return count
     with Pre => value <= 10,
     Post => Double'Result <=20;
   
   function Double (value : count) return count
   is
   begin
      return value * 2;
   end Double;

   procedure Triple (value : count; output : out count)
     with Pre => value <= 6,
     Post => output <=20 and then output = value * 3;

   procedure Triple (value : count; output : out count)
   is 
   begin
      output := value * 3;
   end Triple;
   
   procedure Triple_Update (value : in out count; 
                            update : in Boolean;
                            output : out count)
     with Pre => value <= 6,
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
   
begin

   pragma Assert (I = 8);
   
   --  post condition will pass
   I := Double (I);
   
   pragma Assert (I = 16);
   
   --  post condition will fail
   I := Double (i);

   pragma Assert (J = 3);
   
   --  post condition will pass
   Triple (J,J);
   
   pragma Assert (J = 9);
   
   --  post condition will fail
   Triple (J,J);

end Post_Condition;
