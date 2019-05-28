procedure Range_Constraint is
  type Indicator_Type is (
    CALIBRATION_MODE,
    MEASUREMENT_MODE,
    CALIBRATION_PASS,
    CALIBRATION_FAIL,
    MEASUREMENT_NOT_PROVEN,
    MEASUREMENT_PRESENT);
  
  --  this particular test is testing a syntactic fix for ranges constraint by enum symbols.
  subtype Result_Indicator_Type is Indicator_Type range CALIBRATION_PASS .. MEASUREMENT_NOT_PROVEN;
begin
  null;
end Range_Constraint;
