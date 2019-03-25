function Case_Statement_Others_Only return String is
  type Sensor_Type is (Elevation, Azimuth, Distance);
  Sensor : Sensor_Type := Elevation;
begin
  case Sensor is
    when others    => return "";
  end case;
end Case_Statement_Others_Only;
