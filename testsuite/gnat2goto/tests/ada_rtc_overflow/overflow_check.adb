procedure Overflow_Check is

   type Int16 is range -(2**15) .. (2**15 -1);
   for Int16'Size use 16;
  
   
   var1 : Int16 := Int16'First;
   var2 : Int16 := 10;
   var3 : Int16 := Int16'Last;
   var4 : Int16;
   var5 : Int16;

   int1 : Integer := Integer(Int16'First);
   int2 : Integer := 100;
   int3 : Integer := Integer(Int16'Last);
   
   oor : Integer;
   
begin

   --  test in range
   var4 := var1 + var2;

   pragma Assert (var4 = -32_758);
 
   -- test out of range
   var5 :=  var2 + var3;

   oor := Integer(var3) + 10;
   
   pragma Assert (Integer(var5) = oor);

   -- test in range integers
   var4 := int16(int1 + int2);
 
   pragma Assert (var4 = -32_668);

   -- test out of range integers
   var5 := int16(int2 + int3);
 
   oor := Integer(var3) + 100;
   
   pragma Assert (Integer(var5) = oor);


end Overflow_Check;
