with Ada.Unchecked_Conversion;

procedure Test is

   --  define type as 8-bit range but in 16 bits
   type Int8 is range -(2**7) .. (2**7 -1);
   for Int8'Size use 8;
   
   type Int16 is range -(2**15) .. (2**15 -1);
   for Int16'Size use 16;
   
   type Arr8T is array (Integer range 1 .. 10) of Int8;
   
   type Arr16T is array (Integer range <>) of Int16;
   
   function Convert is new Ada.Unchecked_Conversion (Arr16T, Arr8T);
   
   Var1 : Arr8T;
   
   Var2 : Arr16T(1 .. 5) := (others => 1);
   
   Var3 : Arr16T(1 .. 4) := (others => 2);
   
begin

   Var1 := Convert (Var2);
   
   pragma Assert (Var1 = (0,1,0,1,0,1,0,1,0,1));
   
   Var1 := Convert (Var3);
   
   pragma Assert (Var1 = (0,2,0,2,0,2,0,2,0,1));
   
end Test;
