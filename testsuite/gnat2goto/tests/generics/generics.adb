procedure Generics is

  generic
    type T is private;
    with function "+"(Lhs: T; Rhs: T) return T is <>;
  procedure Check_Assoc(A: T; B: T; C: T);

  procedure Check_Assoc(A: T; B: T; C: T) is
  begin
    pragma Assert ((A + (B + C)) = ((A + B) + C));
  end Check_Assoc;

  procedure Check_Assoc_Int is new Check_Assoc (Integer);
  procedure Check_Assoc_Int_Minus is new Check_Assoc (Integer, "+" => "-");

  pragma Warnings (Off, "is read but never assigned");
  A, B, C: Integer;
begin
  Check_Assoc_Int (A, B, C);
  Check_Assoc_Int_Minus (A, B, C);
end Generics;
