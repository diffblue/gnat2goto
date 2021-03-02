procedure Range_Second is
  type Indicator_Type is (
    CALIBRATION_MODE,
    MEASUREMENT_MODE,
    CALIBRATION_PASS,
    CALIBRATION_FAIL,
    MEASUREMENT_NOT_PROVEN,
    MEASUREMENT_PRESENT);

  subtype Result_Indicator_Type is Indicator_Type range CALIBRATION_PASS .. MEASUREMENT_NOT_PROVEN;

  procedure Testable_2 (Result : Result_Indicator_Type) is
  begin
    null;
  end Testable_2;

  procedure Testable (Result : Indicator_Type) is
  begin
    Testable_2 (Result);
  end Testable;

begin
  Testable (CALIBRATION_PASS);
  Testable (MEASUREMENT_PRESENT);
end Range_Second;
