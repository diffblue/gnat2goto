
procedure Arrays is
   type Arr1 is array (Positive) of Integer;
   type Arr2 is array (Integer) of Integer;
   subtype Range1 is Integer range 10 .. 20;
   type Arr3 is array (Range1) of Integer;
   type Arr4 is array (Integer range 30 .. 32) of Integer;
   type Arr5 is array (50 .. 52) of Integer;
   type Arr6 is array (Integer range <>) of Integer;
   type Arr7 is array (1 .. 3, 1 .. 3) of Integer;

   -- Can't really realise these, since their fixed range is too large:
   -- Actual1 : constant Arr1 := (1, 2, 3);
   -- Actual2 : constant Arr2 := (4, 5, 6);
   Actual3 : Arr3 := (7, 8, 9, others => 10);
   Actual4 : constant Arr4 := (10, 11, 12);
   Actual5 : constant Arr5 := (13, 14, 15);
   Actual6 : constant Arr6 (1000 .. 1002) := (16, 17, 18);
   Actual7 : constant Arr7 := ((19, 20, 21),
                               (22, 23, 24),
                               (25, 26, 27));
   type Derived is new Integer range Actual6'Range;
   Array_Attribs : Integer;
   Empty : constant Arr6 (1 .. 0) := (others => 0);
begin
   Actual3 (10 .. 11) := Actual3 (17 .. 18);
   Actual3 (11) := 100;
   -- Actual3 (13 .. 15) := 200 & (300, 400);
   Actual3 (13 .. 16) := Actual3 (11 .. 12) & Actual3 (15 .. 16);
   Array_Attribs := Actual3'First + Actual3'Last + Actual3'Length;
end Arrays;
