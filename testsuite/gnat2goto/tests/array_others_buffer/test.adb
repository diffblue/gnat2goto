--  Test based on StratoX s-bbcppr.adb code to detect unsupported function
with System; use System;
procedure Test is

   type Context_Id is range 0..100;
   SP_Process : constant Context_Id := 8;

   type Context_Buffer is array (Context_Id) of System.Address;
 
   procedure Initialize_Context
     (Buffer : not null access Context_Buffer;
      New_SP :  System.Address)
   is
   begin

     Buffer.all := (SP_process => New_SP, others => Null_Address);
   end Initialize_Context;

   Buff : access Context_Buffer;
   Addr : constant System.Address := Null_Address;
begin

   Initialize_Context (Buff, Addr);

   pragma Assert (Buff (8) = Null_Address);
 
end Test;
