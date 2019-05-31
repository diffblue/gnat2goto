with Deferred_Const; use Deferred_Const;

procedure Test is
   My_P : Integer := 4;
begin
   pragma Assert (My_P = 4);
end Test;
