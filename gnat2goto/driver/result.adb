package body Result is

   function Make_Error (Error_Value : Error_T) return Result_T is
     ((Status => Error, Error => Error_Value));

   function Make_Ok (Ok_Value : Ok_T) return Result_T is
      ((Status => Ok, Ok => Ok_Value));

   function Is_Error (Result : Result_T) return Boolean
   is (Result.Status = Error);

   function Is_Ok (Result : Result_T) return Boolean is (Result.Status = Ok);

   function Get_Error (Result : Result_T) return Error_T
     is (Result.Error);

   function Get_Ok (Result : Result_T) return Ok_T
     is (Result.Ok);
end Result;
