package body Counter is
  function New_Counter (Inc : Integer) return T is
    ((Current_Value => 0, Current_Increment => Inc));
  procedure Increment (C : in out T) is
  begin
    C.Current_Value := C.Current_Value + C.Current_Increment;
  end Increment;
  function Value_Equals (C : T; V : Integer) return Boolean is
    (C.Current_Value = V);
end Counter;
