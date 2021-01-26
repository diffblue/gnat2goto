--  This tests the use of Pragma Atomic
procedure Pragma_Atomic is

   --  test Atomic variable
   Var1 : Integer := 1;
   pragma Atomic (Var1);

   --  test Atomic type
   type newtype is record
     int1 : Integer := 1;
     int2 : Integer := 2;
   end record;
   pragma Atomic (newtype);

   Var2 : newtype;
 
   -- test Atomic_Components
   type arr_index is range 0..3;
   type arr1 is array (arr_index) of Integer;
   pragma Atomic_Components (arr1);
   
   Var3 : arr1 := (others => 0);
   
   procedure SetArr (Arr : out arr1)
   is
   begin
      Arr := (0, 1, 2, 3);
   end SetArr;
   
   function GetArr (Index : arr_index; Arr : arr1) return Integer is (Arr (Index));
                                                                      
   
begin

   SetArr (Arr   => Var3);
   
   pragma Assert (Var1 = 1);
   pragma Assert (Var2.int1 = 1);
   pragma Assert (Var2.int2 = 2);
   pragma Assert (GetArr (2, Var3) = 2);

end Pragma_Atomic;
