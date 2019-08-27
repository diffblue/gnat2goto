with Lib;
procedure Test is
   procedure Use_Lib is
      use Lib;
   begin
      A;
   end Use_Lib;
begin
   Use_Lib;
end Test;
