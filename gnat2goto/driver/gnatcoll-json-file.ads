with Ada.Text_IO; use Ada.Text_IO;
package GNATCOLL.JSON.File is

   procedure Write (File : File_Type;
                    Item : JSON_Value;
                    Compact : Boolean := True);
   --  Writes A JSON value directly to the file "File".
end GNATCOLL.JSON.File;
