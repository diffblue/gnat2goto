with Counter;
procedure Test is
  C : Counter.T := Counter.New_Counter (Inc => 1);
begin
  pragma Assert (Counter.Value_Equals (C, 0));
  Counter.Increment (C);
  pragma Assert (Counter.Value_Equals (C, 1));
  pragma Assert (False);
end Test;
