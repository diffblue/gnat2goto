procedure Func_Components is
   type Message_Handler is access procedure (Parameter : in out Integer);
   type Func_Ptr is access function (X : Integer) return Integer;

   type Rec is record
      P : Message_Handler;
      F : Func_Ptr;
   end record;

   type MH_Array is array (1 .. 2) of Message_Handler;
   type FP_Array is array (1 .. 2) of Func_Ptr;

   procedure My_Message (Parameter : in out Integer) is
   begin
      Parameter := Parameter + 1;
   end My_Message;

   function My_Func (X : Integer) return Integer is (X + 1);

   My_Rec : constant Rec := (My_Message'Access, My_Func'Access);
   My_MH  : constant MH_Array := (others => My_Message'Access);
   My_FP  : constant FP_Array := (others => My_Func'Access);

   IM  : Integer := 1;
   IFn : Integer := 5;
begin
   My_Rec.P (IM);
   pragma Assert (IM = 2);

   IFn := My_Rec.F (IFn);
   pragma Assert (IFn = 6);

   My_MH (1) (IM);
   pragma Assert (IM = 3);

   IFn := My_FP (2) (IFn);
   pragma Assert (IFn = 7);
end Func_Components;
