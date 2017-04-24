with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;

package body Uint_To_Binary is

   --  TODO: handle negative numbers
   function Convert_Uint_To_Binary
     (Input : Uint;
      Width : Pos)
      return String
   is
      function Hex_To_Binary (Input : String) return String;

      -------------------
      -- Hex_To_Binary --
      -------------------

      function Hex_To_Binary (Input : String) return String is

         function Hex_Char_To_Integer (Ch : Character) return Integer
         with Pre  => Ch in '0' .. '9' | 'A' .. 'F' | 'a' .. 'f',
              Post => Hex_Char_To_Integer'Result in 0 .. 15;

         function Hex_Char_To_Integer (Ch : Character) return Integer is
         begin
            case Ch is
               when '0' .. '9' =>
                  return (Character'Pos (Ch) - Character'Pos ('0'));
               when 'A' .. 'F' =>
                  return 10 + (Character'Pos (Ch) - Character'Pos ('A'));
               when 'a' .. 'f' =>
                  return 10 + (Character'Pos (Ch) - Character'Pos ('a'));
               when others =>
                  raise Program_Error;
            end case;
         end Hex_Char_To_Integer;

         Ret : Unbounded_String := To_Unbounded_String ("");
         Buf : String (1 .. 7);
         package IO is new Integer_IO (Integer);

      begin
         for I of Input loop
            IO.Put (Item => Hex_Char_To_Integer (I), Base => 2, To => Buf);
            declare
               Trimmed    : constant String := Trim (Buf, Side => Left);
               Padded_Str : String (1 .. 4);
            begin
               Move (Source  => Trimmed
                                  (Trimmed'First + 2 .. Trimmed'Last - 1),
                     Target  => Padded_Str,
                     Justify => Right,
                     Pad     => '0');
               Append (Ret, Padded_Str);
            end;
         end loop;
         return To_String (Ret);
      end Hex_To_Binary;

      Nonnegative_Input : constant Uint :=
        (if Input >= 0
         then Input
         else Input + (2 ** UI_From_Int (Width)));

   begin
      declare
         Unpadded_Hex : constant String := UI_Image (Nonnegative_Input, Hex);
         Padded_Hex   : String (1 .. Integer (Width / 4));
      begin
         --  Unpadded_Hex is in the form 16#val#
         Move (Source  => Unpadded_Hex
                            (Unpadded_Hex'First + 3 .. Unpadded_Hex'Last - 1),
               Target  => Padded_Hex,
               Justify => Right,
               Pad     => '0');

         return Hex_To_Binary (Padded_Hex);
      end;
   end Convert_Uint_To_Binary;

end Uint_To_Binary;
