function Case_Statement_Range return Integer is
  type T is range 0 .. 10;
  X : T := 6;
begin
  case X is
    when 0 .. 5  => pragma Assert (false); return 10;
    when 6 .. 10 => return 20;
    when others  => pragma Assert (false); return 30;
  end case;
end Case_Statement_Range;
