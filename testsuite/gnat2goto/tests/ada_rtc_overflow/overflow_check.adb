procedure Overflow_Check (Val1 : Integer; Val2 : Integer) is

   var1 : Integer := Integer'First;
   var2 : Integer := 10;
   var3 : Integer := Integer'Last;
   var4 : Integer;
   var5 : Integer;
   var6 : Integer;

begin

   --  test undetermined will succeed
   var6 := Val1 + Val2;
   
   --  test in range will successed
   var4 := var1 + var2;

   pragma Assert (var4 = -2_147_483_638);
 
   -- expect failure check
   pragma Assert (false);

   -- test out of range will fail
   var5 :=  var2 + var3;

end Overflow_Check;
