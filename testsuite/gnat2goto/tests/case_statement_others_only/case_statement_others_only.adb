procedure Case_Statement_Others_Only is
  function Case_Statement_Return return Integer is
    type Sensor_Type is (Elevation, Azimuth, Distance);
    Sensor : Sensor_Type := Elevation;
  begin
    case Sensor is
      when others => return 1;
    end case;
  end Case_Statement_Return;

  Result : Integer := Case_Statement_Return;
begin
  pragma Assert (Result = 1);
end Case_Statement_Others_Only;
