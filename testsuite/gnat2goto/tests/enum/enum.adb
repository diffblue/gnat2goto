procedure Enum is
  type E is (A, B);

  function Get_A return E is (A);
  function Get_B return E is (B);
begin
  --  CBMC Success
  pragma Assert (A = Get_A);
  --  CBMC Success
  pragma Assert (B = Get_B);
  --  CBMC Success
  pragma Assert (Get_A /= Get_B);
  --  CBMC Failure
  pragma Assert (Get_A = Get_B);
end Enum;
