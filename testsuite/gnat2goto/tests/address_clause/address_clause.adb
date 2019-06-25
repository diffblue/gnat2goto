with System; use System;
with System.Storage_Elements;
procedure Address_Clause is
   Var_Addr_1 : Integer;
   for Var_Addr_1'Address use System'To_Address (16#6F#);

   Var_Addr_2 : Integer;
   for Var_Addr_2'Address use System.Storage_Elements.To_Address (16#80#);
begin
   Var_Addr_1 := 1;
   Var_Addr_2 := 2;
   pragma Assert (Var_Addr_1 + Var_Addr_2 = 3);
end Address_Clause;
