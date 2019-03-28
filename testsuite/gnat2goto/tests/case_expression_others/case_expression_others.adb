procedure Case_Expression_Others is
  Invalid_Ternary : Integer := 3;
  Valid_Result : Integer;
begin
  Valid_Result := (case Invalid_Ternary is
    when 0 => 0,
    when 1 => 0,
    when 2 => 0,
    when others => 1);
    
  pragma Assert (Valid_Result = 1);
  pragma Assert (Valid_Result /= 0);
end Case_Expression_Others;
