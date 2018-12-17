procedure Divide (B : Integer; C : Integer) is
--  A : Integer;
begin
  pragma Assert (B / 1 = B);
  if B > 1 and then C > B then
   pragma Assert (B / B = 1);
   --  reachability check, should be false
   pragma Assert (B < 0);

   -- the following should also be true,
   -- but cbmc will take a very long time to verify them
   --  A := C / B;
   --  pragma Assert (A > 0);
   --  pragma Assert (A < C);
   --  pragma Assert (C - A*B < B);
  end if;
end;
