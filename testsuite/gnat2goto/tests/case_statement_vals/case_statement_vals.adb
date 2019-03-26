function Case_Statement_Vals return String is
  I : Integer := 3;
begin
  case I is
    when 0 | 1 | 2 => return "Valid ternary";
    when 3         => return "Invalid ternary";
    when others    => return "";
  end case;
end Case_Statement_Vals;
