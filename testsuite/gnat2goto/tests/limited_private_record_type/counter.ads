package Counter is
  type T is limited private;

  function New_Counter (Inc : Integer) return T;
  procedure Increment (C : in out T);
  function Value_Equals (C : T; V : Integer) return Boolean;

private
  type T is
    record
      Current_Value : Integer;
      Current_Increment : Integer;
    end record;
end Counter;
