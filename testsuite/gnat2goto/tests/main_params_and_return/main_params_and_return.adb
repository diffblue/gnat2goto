function Main_Params_And_Return (A : Integer; B : Integer) return Integer is
  C : Integer;

  function Bound (X : Integer; Min : Integer; Max : Integer) return Integer is
    R : Integer;
  begin
    if X < Min then
      R := Min;
    elsif X > Max then
      R := Max;
    else
      R := X;
    end if;
    return R;
  end Bound;

begin
  C := Bound (A, 0, 10) + Bound (B, 0, 10);
  pragma Assert (C <= 20);
  pragma Assert (C >= 0);
  return C;
end;
