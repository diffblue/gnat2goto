--  This test is for checking that our assertion pretty printing works - as in
--  CBMC, inline comments, consecutive whitespace and newlines should be
--  stripped so everything fits neatly into a line. Further, Ada also allows
--  naming the parameters to the Assert pragma, and passing an optional
--  message, so we want to make sure we deal with those as well (by ignoring
--  them for now, alternatively we could also just use the assertion text in
--  place of the condition when present, but I suspect this isn't always going
--  to be super helpful; we could also show both. I think it might be best to
--  have this as a config option, but for now this should work well enough)
procedure Assertion_Pretty_Printing
is
  procedure Check (X : Integer) is
  begin
    -- simple assertion
    pragma Assert (X = 5);
  end Check;

  procedure Check2 (X : Integer) is
  begin
    pragma Assert(False -- Some comment
    or (X < 10 -- Some other commane
    -- another comment, followed by an empty line

    and X > 0));
  end Check2;

  function Plus (X : Integer; Y : Integer) return Integer
  is (X + Y);

  procedure Check3 (X : Integer) is
  begin
    pragma Assert
      (Check => Plus (X, Plus (X, 1)) <= 15
      --^ assertion with named parameters
      --  and also nested function calls
      and Plus (X => X, Y => -5) >= -5,
      Message => "Some Message");
  end Check3;

  procedure Check4 (X : Integer) is
  begin
    -- assertion with message
    pragma Assert (X < 10, "x less than 10");
  end Check4;
begin
    Check (6);
    Check2 (7);
    Check4 (10);
    Check3 (9);
end Assertion_Pretty_Printing;
