with Const_Decs; use Const_Decs;

procedure Test is
   My_P : Integer := 4;
begin
   Inc (My_P);
   --  Will fail: symex does not see the value
   pragma Assert (My_P = 50);
end Test;
