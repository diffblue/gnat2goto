procedure Op_And is
  A : boolean := True;
  B : boolean := False;
  C : boolean := True;
begin
  --  CBMC: false
  pragma Assert (A and B);
  --  CBMC: false
  pragma Assert (B and A);
  --  CBMC: success
  pragma Assert (A and A);
  --  CBMC: false
  pragma Assert (B and B);
  --  CBMC: false
  pragma Assert (C and A and B);
end Op_And;

