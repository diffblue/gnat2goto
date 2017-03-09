with Types; use Types;
with Uintp; use Uintp;

package Uint_To_Binary is

   function Convert_Uint_To_Binary
     (Input : Uint;
      Width : Pos)
      return String
     with Pre => Input >= -(UI_From_Int (2) ** (UI_From_Int (Width) - UI_From_Int (1)))
     and then Input < UI_From_Int (2) ** UI_From_Int (Width);
   -- This handles both signed and unsigned types, hence the bound
   -- of 2 ** width not 2 ** (width - 1) as might be expected.

end;
