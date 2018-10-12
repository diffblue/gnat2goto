--  @summary
--  A type to represent the union between a successful result and some error

--  @description
--  This is meant to be used in situations where a function might succeed or
--  fail in interesting ways that should be handled immediately. This is similar
--  to exceptions, with the difference that the possibility for an error to
--  occur is apparent from the type of the function, and it is not possible to
--  silently ignore the possible error (as Ada does not permit discarding the
--  results of function calls)
generic
   type Error_T is private;
   type Ok_T is private;
package Result is
   --  A type representing Union(Error, Ok)
   type Result_T (<>) is private;

   --  Create an error result from an error value
   function Make_Error (Error_Value : Error_T) return Result_T;

   --  Create a success result from an ok value
   function Make_Ok (Ok_Value : Ok_T) return Result_T;

   --  Is the result an error?
   function Is_Error (Result : Result_T) return Boolean;

   --  Is the result a success?
   function Is_Ok (Result : Result_T) return Boolean;

   --  Get the error from an error result
   function Get_Error (Result : Result_T) return Error_T
     with Pre => Is_Error (Result);

   --  Get the ok value from a successful result
   function Get_Ok (Result : Result_T) return Ok_T
     with Pre => Is_Ok (Result);

private
   type Status_T is (Ok, Error);
   type Result_T (Status : Status_T) is
      record
         case Status is
            when Error =>
               Error : Error_T;
            when Ok =>
               Ok : Ok_T;
         end case;
      end record;
end Result;
