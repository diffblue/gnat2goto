procedure Case_Expression is
  type Weekday is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
  Day : Weekday := Tue;
  Hours : Integer;
begin
  Hours := (case Day is
            when Mon => 1,
            when Tue => 2,
            when Wed => 3,
            when Thu => 4,
            when Fri => 5,
            when Sat => 6,
            when Sun => 7);
  pragma Assert (Hours = 2);
  pragma Assert (Hours /= 1);
end Case_Expression;
