with Types;
with Uintp; use Uintp;

package Uint_To_Binary is

   use type Types.Int;
   --  For operators on Uint and Int

   function Convert_Uint_To_Binary
     (Input : Uint;
      Width : Positive)
      return String
   with Pre => Input >= -(2 ** (Width - 1)) and then Input < 2 ** Width;
   --  ???                                          "Input < 2 ** (Width - 1)"

end;
