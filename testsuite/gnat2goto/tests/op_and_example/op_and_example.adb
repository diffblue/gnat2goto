procedure Op_And_Example is
  X : Integer := 0;
  function Boom return Boolean is
  begin
    X := 1;
    return True;
  end Boom;
  A : Boolean := False;
begin
  if A and Boom then
    pragma Assert (False); --  we don't get here because and returns false
  end if;

  --  X is now 1 because Boom side effect was executed
  pragma Assert (X = 1);
  pragma Assert (False);
end Op_And_Example;
