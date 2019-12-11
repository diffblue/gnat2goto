procedure Null_Pointer is
  type Ptr is access Integer;
  X : constant Ptr := null;
begin
  pragma Assert (X = null);
end Null_Pointer;
