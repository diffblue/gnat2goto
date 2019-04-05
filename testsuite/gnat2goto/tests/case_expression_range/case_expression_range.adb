procedure Case_Expression_Range is
  Random_Val : Integer := 10;
  Result : Integer;
begin
  Result := (case Random_Val is
    when 1..10 => 1,
    when 11..20 => 2,
    when others => 3);

  pragma Assert (Result = 1);
  pragma Assert (Result /= 3);
end Case_Expression_Range;
