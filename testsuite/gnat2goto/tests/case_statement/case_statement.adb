function Case_Statement return String is
  type Sensor_Type is (Elevation, Azimuth, Distance);
  Sensor : Sensor_Type := Elevation;
begin
  case Sensor is
    when Elevation => return "Elevation sensor";
    when Azimuth   => return "Azimuth sensor";
    when Distance  => return "Distance sensor";
    when others    => return "";
  end case;
end Case_Statement;
