procedure Test is

  Val1 : Integer;
  Val2 : Integer;

begin

  if Val1 /= Val2 then
    if Integer'Max(Val1, Val2) = Val1 then
      pragma Assert(Integer'Min(Val1, Val2) = Val2);
    else
      pragma Assert(Integer'Min(Val1, Val2) = Val1);
    end if;
  else
    pragma Assert(Integer'Max(Val1, Val2) = Integer'Min(Val1, Val2));
  end if;
end Test;
