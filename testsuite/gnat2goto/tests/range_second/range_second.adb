procedure Range_Second is
  type Indicator_Type is (
    CALIBRATION_MODE,
    MEASUREMENT_MODE,
    CALIBRATION_PASS,
    CALIBRATION_FAIL,
    MEASUREMENT_NOT_PROVEN,
    MEASUREMENT_PRESENT);

  subtype Result_Indicator_Type is Indicator_Type range CALIBRATION_PASS .. MEASUREMENT_NOT_PROVEN;

  procedure Testable (Result : Result_Indicator_Type) is
  begin
    null;
  end Testable;

begin
  Testable (CALIBRATION_PASS);
  Testable (MEASUREMENT_PRESENT);
end Range_Second;
