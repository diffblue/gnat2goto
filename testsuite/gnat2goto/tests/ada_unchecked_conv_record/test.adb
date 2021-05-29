with Ada.Unchecked_Conversion;

procedure Test is

   --  define type as 8-bit range but in 16 bits
   type Int8 is range -(2**7) .. (2**7 -1);
   for Int8'Size use 8;
   
   type Int16 is range -(2**15) .. (2**15 -1);
   for Int16'Size use 16;
   
   type Int32 is range -(2**31) .. (2**31 -1);
   for Int32'Size use 32;
   
   type My_Rec is record
      Int1 : Int16;
      Int2 : Int8;
      Int3 : Int8;
   end record;
   for My_Rec'Size use 32;
   
   function Convert is new Ada.Unchecked_Conversion (Int32, My_Rec);
   
   Var1 : Int32;
   
   Var2 : My_Rec;
      
begin

   Var1 := 0;

   Var2 := Convert (Var1);
   
   pragma Assert (Var2 = My_Rec'(Int1 => 0,
                                 Int2 => 0,
                                 Int3 => 0));
end Test;
