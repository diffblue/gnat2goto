with Ada.Unchecked_Conversion;

procedure Test is

   type Enum is (ONE, TWO, THREE, FOUR, FIVE);
   for Enum'Size use 16;
   
   type Int8 is range 0 .. 255;
   for Int8'Size use 16;
   
   function Convert is new Ada.Unchecked_Conversion (Int8, Enum);
   
   Var1 : Int8;
   
   Var2 : Enum;
      
begin

   Var1 := 2;

   Var2 := Convert (Var1);
   
   --  pragma Assert (Var2 = TWO);

   --  Var1 := 6;

   --  Var2 := Convert (Var1);  --  this is invalid

end Test;
