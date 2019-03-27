procedure Case_Statement is
  function Case_Statement_Return return Integer is
    type Sensor_Type is (Elevation, Azimuth, Distance);
    Sensor : Sensor_Type := Elevation;
  begin
    case Sensor is
      when Elevation => return 1;
      when Azimuth   => return 2;
      when Distance  => return 3;
      when others    => return 4;
    end case;
  end Case_Statement_Return;

  Result : Integer := Case_Statement_Return;
begin
  pragma Assert (Result = 1);
  pragma Assert (Result /= 4);
end Case_Statement;
