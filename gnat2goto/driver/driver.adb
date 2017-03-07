------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2GOTO COMPONENTS                           --
--                                                                          --
--                               D R I V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                  Copyright (C) 2017, Altran UK Limited                   --
--                                                                          --
-- gnat2goto is  free  software;  you can redistribute it and/or  modify it --
-- under terms of the  GNU General Public License as published  by the Free --
-- Software  Foundation;  either version 3,  or (at your option)  any later --
-- version.  gnat2goto is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public License  distributed with gnat2goto;  see file COPYING3. --
-- If not,  go to  http://www.gnu.org/licenses  for a complete  copy of the --
-- license.                                                                 --
--                                                                          --
------------------------------------------------------------------------------

with Switch;    use Switch;
with Tree_Walk; use Tree_Walk;
with Irep_Schemata; use Irep_Schemata;

package body Driver is

   procedure GNAT_To_Goto (GNAT_Root : Node_Id)
   is
      Unused : constant Irep_Code_Block := Do_Compilation_Unit (GNAT_Root);
   begin
      null;  --  ??? dump to JSON file
   end GNAT_To_Goto;

   function Is_Back_End_Switch (Switch : String) return Boolean is
      First : constant Natural := Switch'First + 1;
      Last  : constant Natural := Switch_Last (Switch);

   begin
      --  For now we allow the -g/-O/-f/-m/-W/-w and -pipe switches, even
      --  though they will have no effect. This permits compatibility with
      --  existing scripts.

      return
        Is_Switch (Switch)
          and then (Switch (First) in 'f' | 'g' | 'm' | 'O' | 'W' | 'w'
                      or else Switch (First .. Last) = "pipe");
   end Is_Back_End_Switch;

end Driver;
