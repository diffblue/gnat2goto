procedure Func is

   A : Integer;
   B : Integer := 3;
   Return_Code : Integer;

   function Set_A (X : out Integer) return Integer
   is
   begin
      X := 5;
      return 0;
   end;

   function Read_B (Y: in Integer) return Integer
   is
   begin
      return Y + 1;
   end;

   function Set_A_Read_B (X : out Integer; Y : in Integer) return Integer
   is
   begin
      X := 7 + Y;
      return 13;
   end;

   function Set_A_Read_A (X : in out Integer) return Integer
   is
   begin
      X := X + 1;
      return 7;
   end;
begin

   Return_Code := Set_A (A);
   pragma Assert (Return_Code = 0);
   pragma Assert (A = 5);

   A := Read_B (A);
   pragma Assert (A = 6);

   Return_Code := Set_A_Read_B (A, B);
   pragma Assert (Return_Code = 13);
   pragma Assert (A = 10);

   Return_Code := Set_A_Read_A (A);
   pragma Assert (Return_Code = 7);
   pragma Assert (A = 11);
end;