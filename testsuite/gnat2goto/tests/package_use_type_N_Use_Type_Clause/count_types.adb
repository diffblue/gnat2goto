package body Count_Types is

   function Double (X : in out Counter) return Counter is
   begin
      return X*2;
   end Double;

end Count_Types;
