procedure not_op is
  x : boolean;
begin
  pragma Assert (not x /= x);
  pragma Assert (not (not x) = x);
end;

