with System_Types;
with Problem_Size; use Problem_Size;
function Check_Message
     (M      : System_Control_Message;
      Oldest : System_Types.Time;
      Newest : System_Types.Time) return Boolean
is
   Content_Size : constant Natural := M'Size - M.Header'Size;
begin
   pragma Assert (Oldest'Size = 32);
   pragma Assert (M.Header'Size = 80);
   pragma Assert (M'Size = 88);
   pragma Assert (Content_Size = 8);
   --  The next assertion should fail as Newest'Size = 32.
   pragma Assert (Newest'Size = 64);
   return Content_Size /= 10;
end Check_Message;
