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
with Symbol_Table_Info; use Symbol_Table_Info;
with Gather_Irep_Symbols;
with Iinfo; use Iinfo;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNATCOLL.JSON; use GNATCOLL.JSON;

package body Driver is

   procedure GNAT_To_Goto (GNAT_Root : Node_Id)
   is
      Program_Irep : constant Irep_Code_Block := Do_Compilation_Unit (GNAT_Root);
      Void_Type : constant Irep_Void_Type := Make_Irep_Void_Type;
      Start_Name : constant Unbounded_String := To_Unbounded_String ("_start");
      Function_Type : Irep_Code_Type := Make_Irep_Code_Type;
      Local_Symbol_Table : Symbol_Table;
      Function_Symbol : Symbol;
   begin
      Gather_Irep_Symbols.Gather (Local_Symbol_Table, Irep (Program_Irep));

      -- For now, emit this as _start.
      Function_Symbol.Name := Start_Name;
      Function_Symbol.PrettyName := Start_Name;
      Function_Symbol.BaseName := Start_Name;
      Set_Return_Type (Function_Type, Irep (Void_Type));
      Function_Symbol.SymType := Irep (Function_Type);
      Function_Symbol.Value := Irep (Program_Irep);
      Function_Symbol.Mode := To_Unbounded_String ("C");
      Symbol_Maps.Insert (Local_Symbol_Table, Start_Name, Function_Symbol);

      Put_Line (Create (SymbolTable2Json (Local_Symbol_Table)).Write);
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
