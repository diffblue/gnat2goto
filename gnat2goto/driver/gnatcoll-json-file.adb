with GNATCOLL.JSON.Utility;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package body GNATCOLL.JSON.File is

   procedure Write
     (File    : File_Type;
      Item    : JSON_Value;
      Compact : Boolean;
      Indent  : Natural);
   --  Auxiliary file write function

   -----------
   -- Write --
   -----------

   procedure Write
     (File    : File_Type;
      Item    : JSON_Value;
      Compact : Boolean;
      Indent  : Natural)
   is
      procedure Do_Indent (File : File_Type; Val : Natural);
      --  Adds whitespace characters to output file corresponding
      --  to the indentation level.

      ---------------
      -- Do_Indent --
      ---------------

      procedure Do_Indent (File : File_Type; Val : Natural) is
      begin
         if Compact then
            return;
         end if;

         Put (File, (1 .. 2 * Val => ' '));
      end Do_Indent;

   begin
      case Item.Kind is
         when JSON_Null_Type =>
            Put (File, "null");

         when JSON_Boolean_Type =>
            if Item.Data.Bool_Value then
               Put (File, "true");
            else
               Put (File, "false");
            end if;

         when JSON_Int_Type =>
            declare
               S : constant String := Item.Data.Int_Value'Img;
            begin
               if S (S'First) = ' ' then
                  Put (File, S (S'First + 1 .. S'Last));
               else
                  Put (File, S);
               end if;
            end;

         when JSON_Float_Type =>
            declare
               S : constant String := Item.Data.Flt_Value'Img;
            begin
               if S (S'First) = ' ' then
                  Put (File, S (S'First + 1 .. S'Last));
               else
                  Put (File, S);
               end if;
            end;

         when JSON_String_Type =>
            Put (File, To_String
                   (JSON.Utility.Escape_String (Item.Data.Str_Value)));

         when JSON_Array_Type =>
            Put (File, '[');

            if not Compact then
               Put (File, ASCII.LF);
            end if;

            for J in Item.Data.Arr_Value.Vals.First_Index ..
              Item.Data.Arr_Value.Vals.Last_Index
            loop
               Do_Indent (File, Indent + 1);
               Write
                 (File, Item.Data.Arr_Value.Vals.Element (J),
                  Compact, Indent + 1);

               if J < Item.Data.Arr_Value.Vals.Last_Index then
                  Put (File, ",");
               end if;

               if not Compact then
                  Put (File, ASCII.LF);
               end if;
            end loop;

            Do_Indent (File, Indent);
            Put (File, ']');

         when JSON_Object_Type =>
            declare
               use Object_Items_Pkg;
               J : Object_Items_Pkg.Cursor := Item.Data.Obj_Value.Vals.First;

            begin
               Put (File, '{');

               if not Compact then
                  Put (File, ASCII.LF);
               end if;

               while Has_Element (J) loop
                  Do_Indent (File, Indent + 1);
                  Put
                    (File,
                     To_String
                       (GNATCOLL.JSON.Utility.Escape_String
                            (Element (J).Key)));

                  Put (File, ':');
                  if not Compact then
                     Put (File, ' ');
                  end if;

                  Write (File, Element (J).Val, Compact, Indent + 1);

                  Next (J);

                  if Has_Element (J) then
                     Put (File, ",");
                  end if;

                  if not Compact then
                     Put (File, ASCII.LF);
                  end if;
               end loop;

               Do_Indent (File, Indent);
               Put (File, '}');
            end;

      end case;
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (File : File_Type;
      Item : JSON_Value;
      Compact : Boolean := True)
   is
   begin
      Write (File, Item, Compact, 0);
   end Write;

end GNATCOLL.JSON.File;
