procedure Case_Statement_Vals is
  function Case_Statement_Return return Integer is
    I : Integer := 3;
  begin
    case I is
      when 0 | 1 | 2 => return 0;
      when 3         => return 1;
      when others    => return 2;
    end case;
  end Case_Statement_Return;

  Result : Integer := Case_Statement_Return;
begin
  pragma Assert (Result /= 0);
  pragma Assert (Result = 1);
  pragma Assert (Result /= 2);
end Case_Statement_Vals;
