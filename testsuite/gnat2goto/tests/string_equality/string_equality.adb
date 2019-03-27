procedure String_Equality is
  function Constant_String return String is
    ("sensor");
begin
  pragma Assert (Constant_String = "sensor");
end String_Equality;
