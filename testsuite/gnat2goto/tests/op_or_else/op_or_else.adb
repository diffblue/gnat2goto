procedure Op_Or_Else is
  A : Boolean := True;
  B : boolean := False;
  C : boolean := True;
  
  --  The following two functions serve the purpose of having
  --  a division by zero check that serves to demonstrate that
  --  because of the path followed short circuiting at the call
  --  site has occured.

  function Div_By_Zero(Y : Integer) return Boolean is
    X : Integer := 10;
    Z : Integer;
    Ret : Boolean;
  begin
    Z := (X / Y);
    Ret := Z = 0;
    return Ret;
  end Div_By_Zero;

  function Div_By_Zero_Not_Called(Y : Integer) return Boolean is
    X : Integer := 10;
    Z : Integer;
    Ret : Boolean;
  begin
    Z := (X / Y);
    Ret := Z = 0;
    return Ret;
  end Div_By_Zero_Not_Called;

begin
  --  for these, similar behaviour to plain or operator 
  --  is expected
  pragma Assert (A or else B);
  pragma Assert (B or else A);
  pragma Assert (A or else A);
  pragma Assert (C or else A or else B);

  --  or else operator should not hit a division by zero
  --  check because it short circuits.
  --  CBMC: Success
  pragma Assert(A or else div_by_zero_not_called (0));
  --  plain or operator should hit a division by zero
  --  check because it doesn't short circuit.
  --  CBMC: Success
  pragma Assert(A or div_by_zero (0));
end op_or_else;

