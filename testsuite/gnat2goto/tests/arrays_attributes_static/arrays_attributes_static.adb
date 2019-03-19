-- covers array attributes: first, last, range, length
procedure Arrays_Attributes_Static is
   subtype Range1 is Integer range 10 .. 20;
   type Arr3 is array (Range1) of Integer;
   Actual3 : Arr3 := (7, 8, 9, others => 10);
   subtype My_Index is Integer range Actual3'Range;
   Array_Attribs : Integer;
   Val : Integer := 10;
   An_Index : My_Index := 12;
begin
   Actual3 (10 .. 11) := Actual3 (17 .. 18);
   Actual3 (11) := 100;
   Actual3 (13 .. 16) := Actual3 (11 .. 12) & Actual3 (15 .. 16);
   Array_Attribs := Actual3'First + Actual3'Last + Actual3'Length;

   pragma Assert (Array_Attribs = 41);
   pragma Assert (An_Index = 12);
end Arrays_Attributes_Static;
