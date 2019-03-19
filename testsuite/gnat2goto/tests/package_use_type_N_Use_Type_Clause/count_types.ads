package Count_Types is

   type Count is new Integer;

   type Counter is new Integer;
   function Double (X : in out Counter) return Counter;

end Count_Types;
