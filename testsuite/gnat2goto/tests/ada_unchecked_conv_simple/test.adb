with Ada.Unchecked_Conversion;

procedure Test is

   type Real64 is digits 7 range 0.0 .. 100.0;
   for Real64'Size use 64;
   
   type Int64 is range -(2**63) .. (2**63 -1);
   for Int64'Size use 64;
   
   function Convert is new Ada.Unchecked_Conversion (Int64, Real64);
   
   Var1 : Int64;
   
   Var2 : Real64;
      
begin

   Var1 := 0;

   Var2 := Convert (Var1);
   
   pragma Assert (Var2 = 0.0);
end Test;
