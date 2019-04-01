procedure Case_Expression_Others is
  Invalid_Ternary : Integer := 3;
  Valid_Result : Integer;
  --  We need this to make sure that the
  --  frontend's constant folding isn't
  --  causing us to test something other
  --  than what we originally expected.
  function Nested_One (Ternary : Integer) return Integer is
  begin
    return (case Invalid_Ternary is
            when 0 => 0,
            when 1 => 0,
            when 2 => 0,
            when others => 1);
  end Nested_One;
begin
  Valid_Result := Nested_One (Invalid_Ternary);
  pragma Assert (Valid_Result = 1);
  pragma Assert (Valid_Result /= 0);
end Case_Expression_Others;
