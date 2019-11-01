procedure Test is

   function Eq_Zero (Num : Integer) return Boolean is
   begin return Num = 0;
   end Eq_Zero;

   function Neq_Zero (Num : Integer) return Boolean is
   begin return Num /= 0;
   end Neq_Zero;

   type Comparator is access function (Num : Integer) return Boolean;

   procedure Assert_Compare(Func : in not null Comparator) is
   begin
      pragma Assert (Func.all(5));
   end Assert_Compare;

begin

   Assert_Compare(Eq_Zero'Access);
   Assert_Compare(Neq_Zero'Access);

end Test;
