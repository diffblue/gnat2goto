procedure Access_Subprog_Access_Param is
   type Acc_Proc is access procedure (I : access Integer);
   type Acc_Func is access function (I : access Integer) return Integer;

   procedure My_Proc (I : access Integer) is
   begin
      I.all := I.all + 1;
   end My_Proc;

   function My_Func (I : access Integer) return Integer is (I.all + 1);

   My_Acc_Proc : constant Acc_Proc := My_Proc'Access;
   My_Acc_Func : constant Acc_Func := My_Func'Access;

   IP : aliased Integer := 1;
   IM : aliased Integer := 5;
begin
   My_Acc_Proc (IP'Access);
   pragma Assert (IP = 2);

   IM := My_Acc_Func (IM'Access);
   pragma Assert (IM = 6);
end Access_Subprog_Access_Param;
