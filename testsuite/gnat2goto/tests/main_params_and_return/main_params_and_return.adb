function Main_Params_And_Return (a : Integer; b : Integer) return Integer is
  c : Integer;

  function Bound (x : Integer; Min : Integer; Max : Integer) return Integer is
    R : Integer;
  begin
    if x < Min then
      R := Min;
    elsif x > Max then
      R := Max;
    else
      R := x;
    end if;
    return R;
  end Bound;

begin
  c := Bound (a, -10, 10) + Bound (b, -10, 10);
  return c;
end;
