--  Test case checks the withing of Ada.Exceptions
--  This used to cause a stack overflow.
with Ada.Exceptions;
procedure With_Exception is
begin
   null;
end With_Exception;
