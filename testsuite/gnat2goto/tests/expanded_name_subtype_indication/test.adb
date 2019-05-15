with Base;
use type Base.Real;
procedure Test is
   subtype X is Base.Real;
   procedure Test (Val : X) is
   begin
      -- success
      pragma Assert (Val < 2.0);
      -- success
      pragma Assert (Val > 0.0);
      pragma Assert (Val * 20.0 < 10.0);
   end Test;
begin
   Test (1.0);
end Test;
