with System_Types;
package Problem_Size is

   type Subsystem is
     (No_Subsystem,
      Health_Monitor,
      Clock,
      Navigation,
      Control_Panel,
      User_Display
   );

   type Topics is
     (No_Topic,
      System_Control_Topic,
      Health_Status_Topic,
      Time_Topic,
      Location_Topic,
      Waypoint_Control_Topic,
      User_Notification_Topic
   );

   type Message_Header is record
      Sender       : Subsystem;
      Topic        : Topics;
      Timestamp    : System_Types.Time;
      Content_Size : Natural;
   end record;

   type System_Control_Action is
     (No_Action, Clean_Shutdown, Emergency_Shutdown, Restart );

   type System_Control_Message is record
      Header : Message_Header;
      Action : System_Control_Action;
   end record;

end Problem_Size;
