procedure Range_Third is
  type An_Int is range 1..1000;
  subtype Another_Int is An_Int range 5..100;

  procedure Testable (Result : An_Int) is
    test_val : Another_Int := Result;
  begin
    null;
  end Testable;

begin
  Testable (50);
  Testable (200);
end Range_Third;
