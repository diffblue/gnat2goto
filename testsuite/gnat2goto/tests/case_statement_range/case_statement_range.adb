function Case_Statement_Range return String is
  type T is range 0 .. 10;
  X : T := 6;
begin
  case X is
    when 0 .. 5  => return "Zero to five";
    when 6 .. 10 => return "Six to ten";
    when others  => return "Invalid";
  end case;
end Case_Statement_Range;
