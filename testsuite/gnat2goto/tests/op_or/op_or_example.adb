procedure Op_Or_Example is
  X : Integer := 0;
  function Boom return Boolean is
  begin
    X := 1;
    return True;
  end Boom;
  A : Boolean := True;
begin
  if A or Boom then
    pragma Assert (True); --  we should get here so this should be SUCCESS
  end if;

  --  X is now 1 because Boom side effect was executed
  pragma Assert (X = 1);
  pragma Assert (False);
end Op_Or_Example;
