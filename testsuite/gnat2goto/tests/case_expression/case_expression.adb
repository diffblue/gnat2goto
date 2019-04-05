procedure Case_Expression is
  type Weekday is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
  Day : Weekday := Tue;
  Hours : Integer;
  --  We need this to avoid the front-end's
  --  constant folding mess up with the validity
  --  of this test.
  function Nested_Case_Expr (WDay : Weekday) 
    return Integer is
  begin
     return (case WDay is
              when Mon => 1,
              when Tue => 2,
              when Wed => 3,
              when Thu => 4,
              when Fri => 5,
              when Sat => 6,
              when Sun => 7);
  end Nested_Case_Expr;
begin
  Hours := Nested_Case_Expr (Day);
  pragma Assert (Hours = 2);
  pragma Assert (Hours /= 1);
end Case_Expression;
