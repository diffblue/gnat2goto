with Incomplete_Dec; use Incomplete_Dec;

procedure Test is
   My_Rec : Partial_Dec := (A=>1);
begin
   Inc (My_Rec);
   pragma Assert (My_Rec.A = 2);
end Test;
