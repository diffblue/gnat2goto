procedure Ranges is
   type Int1 is range 1 .. 10;
   type Int2 is new Int1 range 2 .. 9;
   subtype Int3 is Int1 range 3 .. 8;
   A : Int1 := 5;
   B : Int2;
   C : Int3;
begin
   B := Int2(A);
   C := A;
end;
