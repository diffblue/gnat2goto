
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;

with Types; use Types;

package body Uint_To_Binary is
   
   -- TODO: handle negative numbers
   function Convert_Uint_To_Binary (Input : Uint; Width : Integer) return String is
      
      function Hex_To_Binary (Input : String) return String is 
	 
	 function Hex_Char_To_Integer (Ch : Character) return Integer is
	 begin
	    case Ch is
	       when '0' .. '9' =>
		  return (Character'Pos (Ch) - Character'Pos ('0'));
	       when 'A' .. 'F' =>
		  return 10 + (Character'Pos (Ch) - Character'Pos('A'));
	       when 'a' .. 'f' =>
		  return 10 + (Character'Pos (Ch) - Character'Pos('a'));
	       when others =>
		  raise Program_Error;
	    end case;
	 end;
	 
	 Ret : Unbounded_String := To_Unbounded_String ("");
	 Intval : Integer;
	 Buf : String(1..7);
	 package IO is new Integer_IO (Integer);

      begin
	 for I in Integer range Input'First .. Input'Last loop
	    Intval := Hex_Char_To_Integer (Input (I));
	    IO.Put (Item => Intval, Base => 2, To => Buf);
	    declare
	       Trimmed : constant String := Trim (Buf, Side => Left);
	       Padded_Str : String(1..4);
	    begin
	       Move(Source => Trimmed (Trimmed'First + 2 .. Trimmed'Last - 1),
		    Target => Padded_Str,
		    Justify => Right,
		    Pad => '0');
	       Append (Ret, Padded_Str);
	    end;
	 end loop;
	 return To_String (Ret);
      end;
      
      Positive_Input : Uint := Input;
      
   begin
      pragma Assert (Input < 2 ** Width);
      pragma Assert (Input >= -(2 ** (Width - 1)));
      if (Input < 0) then
	 Positive_Input := Input + (2 ** Width);
      end if;
      declare
	 Unpadded_Hex : constant String := UI_Image (Positive_Input, Hex);
	 Padded_Hex : String (1..(Width/4));	 
      begin
	 pragma Assert (Unpadded_Hex (Unpadded_Hex'First) = ' ');
	 Move(Source => Unpadded_Hex (Unpadded_Hex'First + 1 .. Unpadded_Hex'Last),
	      Target => Padded_Hex,
	      Justify => Right,
	      Pad => '0');
	 return Hex_To_Binary (Padded_Hex);
      end;
   end;
   
end Uint_To_Binary;
