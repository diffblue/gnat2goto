procedure Range_Constraint is
  type Indicator_Type is (
    CALIBRATION_MODE,
    MEASUREMENT_MODE,
    CALIBRATION_PASS,
    CALIBRATION_FAIL,
    MEASUREMENT_NOT_PROVEN,
    MEASUREMENT_PRESENT);
  
  subtype Result_Indicator_Type is Indicator_Type range CALIBRATION_PASS .. MEASUREMENT_NOT_PROVEN;

  --  this test is here because subtype assignment is broken and needs to be activated againt when fixed
  Value : constant Result_Indicator_Type := CALIBRATION_FAIL;
begin
  pragma assert (Value = CALIBRATION_FAIL);
end Range_Constraint;
