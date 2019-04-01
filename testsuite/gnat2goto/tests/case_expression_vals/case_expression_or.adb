procedure Case_Expression_Or is
  type Weekday is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
  Day : Weekday := Tue;
  Hours : Integer;
begin
  Hours := (case Day is
            when Mon | Tue => 1,
            when Wed | Thu => 2,
            when Fri | Sat => 3,
            when Sun       => 0);
  pragma Assert (Hours = 1);
  pragma Assert (Hours /= 2);
end Case_Expression_Or;
