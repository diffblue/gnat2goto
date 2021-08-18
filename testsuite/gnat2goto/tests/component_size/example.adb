procedure Example is
   type Message_Header is record
      Content_Size : Natural;
   end record;

   type System_Control_Message is record
      Header : Message_Header;
      Action : Integer;
   end record;

   SCM : System_Control_Message;

begin
   SCM.Action := 0;
   pragma Assert (SCM.Header'Size = 5);
   pragma Assert (SCM.Header'Size = 32);
end Example;
