package body Test
is
   function Double(Input : Positive) return Positive
   is
   begin
      return Input + Input;
   end Double;
end Test;
