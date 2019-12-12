with system;
procedure Attrib_Address is

   I : Integer := 0;

   type T_A is array (Positive range <>) of Integer;

   A : T_A := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
   for A'Alignment use 32;

   Add_1 : System.Address;
   Add_2 : System.Address;
begin
   Add_1 := I'Address;
   Add_2 := A'Address;
end Attrib_Address;
