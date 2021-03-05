package body Division_Check_Int is

   procedure Int_Check (Val1 : Integer; Val2 : Integer) is

      Int1 : Integer;
      Int2 : Integer;

      Bool_1 : Boolean;
      Bool_2 : Boolean;

   begin

      Int1 := Val1;
      Int2 := Val1;

      --  valid integer division
      Int1 := Int1 / Int2;

      pragma Assert (Int1 = 1);

      if Bool_1 then
         Int2 := Int2 - (Int1 *2);
         --  this will cause a Constraint Error
         Int1 := Int1 / Int2;
      end if;

      --  REM is not supported by CBMC

      Int1 := Val2;
      Int2 := Val1;

      --  valid mod
      Int1 := Int1 mod Int2;

      pragma Assert (Int1 = 1);

      if Bool_2 then
         Int2 := Int2 - (Int1 *2);
         --  this will cause a Constraint Error
         Int1 := Int1 mod Int2;
      end if;

      
      -- add an assert fail for check
      pragma Assert (False);

   end Int_Check;

end Division_Check_Int;
