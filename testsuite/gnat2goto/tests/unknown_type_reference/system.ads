package System is
   type Address is private;
private
   type Address is mod 2 ** 8;
end System;
