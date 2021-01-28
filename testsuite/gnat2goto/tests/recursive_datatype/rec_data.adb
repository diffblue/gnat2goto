procedure Rec_Data is
   type R_D;
   type R_D_I is range 1 .. 10;
   type R_D_Ptr is access all R_D;

   type R_D is record
      N : Integer;
      Next : R_D_Ptr;
   end record;

   type R_D_A;
   type R_D_A is record
      N : Integer;
      Next : access all R_D_A;
   end record;

   V : R_D;
   Next : aliased R_D;

   W : R_D_A;
   W_Next : aliased R_D_A;
begin
   V.N := 3;
   V.Next := Next'Access;
   V.Next.N := 5;
   pragma Assert (V.N = 3);
   pragma Assert (V.Next.N = 5);

   W.N := 7;
   W.Next := W_Next'Access;
   W.Next.N := 11;
   pragma Assert (W.N = 7);
   pragma Assert (W.Next.N = 11);

   null;
end Rec_Data;

