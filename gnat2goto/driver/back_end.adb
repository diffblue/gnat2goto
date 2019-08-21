------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2GOTO COMPONENTS                          --
--                                                                          --
--                             B A C K _ E N D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                       Copyright (C) 2010-2017, AdaCore                   --
--                       Copyright (C) 2017, Altran UK Limited              --
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

with Adabkend;
with Elists;
with Errout;
with Driver;
with Namet;
with Opt;
with Stringt;
with System;

package body Back_End is

   package GNAT2Goto_BE is new Adabkend
     (Product_Name       => "GNAT2GOTO",
      Copyright_Years    => "2017-2017",
      Driver             => Driver.GNAT_To_Goto,
      Is_Back_End_Switch => Driver.Is_Back_End_Switch);

   -------------------
   -- Call_Back_End --
   -------------------

   procedure Call_Back_End (Mode : Back_End_Mode_Type) is
      pragma Unreferenced (Mode);
      use type Opt.Warning_Mode_Type;

      Save_Warning_Mode : constant Opt.Warning_Mode_Type := Opt.Warning_Mode;
      --  Save original frontend warning mode for restoration before returning
      --  from Call_Back_End, as various checks which may issue warnings are
      --  performed after that.

   begin
      --  Since the back end is called with all tables locked, first unlock any
      --  tables that we need to change.

      Namet.Unlock;
      Stringt.Unlock;
      Elists.Unlock;

      Errout.Finalize (Last_Call => False);
      if Errout.Compilation_Errors then
         goto Unlock;
      end if;
      Errout.Reset_Warnings;

      Opt.Warning_Mode := Opt.Normal;

      GNAT2Goto_BE.Call_Back_End;

      Errout.Finalize (Last_Call => False);
      if Errout.Compilation_Errors then
         goto Unlock;
      end if;
      Errout.Reset_Warnings;

      --  Restore the original frontend warning mode

      Opt.Warning_Mode := Save_Warning_Mode;

   <<Unlock>>
      --  Make sure to lock any unlocked tables again before returning

      Namet.Lock;
      Stringt.Lock;
      Elists.Lock;
   end Call_Back_End;

   -------------------------------
   -- Gen_Or_Update_Object_File --
   -------------------------------

   procedure Gen_Or_Update_Object_File is
   begin
      null;
   end Gen_Or_Update_Object_File;

   -----------------------------
   -- Scan_Compiler_Arguments --
   -----------------------------

   procedure Scan_Compiler_Arguments is
      gnat_argv, save_argv : System.Address;
      pragma Import (C, gnat_argv, "gnat_argv");
      pragma Import (C, save_argv, "save_argv");

      gnat_argc, save_argc : Integer;
      pragma Import (C, gnat_argc, "gnat_argc");
      pragma Import (C, save_argc, "save_argc");

      use type System.Address;

   begin
      --  If save_argv is non null, it means we are part of gnat1+gnat2why
      --  and need to set gnat_argv to save_argv so that Ada.Command_Line
      --  has access to the command line.

      if save_argv /= System.Null_Address then
         gnat_argv := save_argv;
         gnat_argc := save_argc;
      end if;

      --  We are in the gnat2why executable, so GNATprove_Mode is always true
      --  note that this flag needs to be set very early on, since e.g.
      --  Scan_Compiler_Arguments uses it.

      Opt.GNATprove_Mode := True;

      GNAT2Goto_BE.Scan_Compiler_Arguments;

      --  --  Read extra options for gnat2why

      --  Gnat2Why_Args.Init
      --    (if Opt.SPARK_Switches_File_Name = null then ""
      --     else Opt.SPARK_Switches_File_Name.all);

      Opt.Disable_ALI_File := False;
   end Scan_Compiler_Arguments;

end Back_End;
