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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Switch;                use Switch;

with Ireps;                 use Ireps;
with Symbol_Table_Info;     use Symbol_Table_Info;

with Tree_Walk;             use Tree_Walk;
with Gather_Irep_Symbols;

with GNATCOLL.JSON; use GNATCOLL.JSON;

package body Driver is

   procedure GNAT_To_Goto (GNAT_Root : Node_Id)
   is
      Program_Symbol : constant Symbol := Do_Compilation_Unit (GNAT_Root);
      Program_Expr : constant Irep := New_Irep (I_Symbol_Expr);
      Program_Type : constant Irep := Program_Symbol.SymType;
      Program_Return_Type : constant Irep :=  Get_Return_Type (Program_Type);
      Program_Args : constant Irep_List := Get_Parameter (Get_Parameters (Program_Type));
      Void_Type : constant Irep := New_Irep (I_Void_Type);
      Start_Name : constant Unbounded_String := To_Unbounded_String ("_start");
      Start_Symbol : Symbol;
      Start_Type : constant Irep := New_Irep (I_Code_Type);
      Start_Body : constant Irep := New_Irep (I_Code_Block);
      Initial_Call : constant Irep := New_Irep (I_Code_Function_Call);
      Initial_Call_Args : constant Irep := New_Irep (I_Argument_List);
   begin
      -- Gather local symbols and put them in the symtab:
      declare
         Local_Symbols : Symbol_Table;
      begin
         for Sym_Iter in Global_Symbol_Table.Iterate loop
            if Kind (Global_Symbol_Table (Sym_Iter).SymType) = I_Code_Type then
               Gather_Irep_Symbols.Gather (Local_Symbols, Global_Symbol_Table (Sym_Iter).Value);
            end if;
         end loop;
         for Sym_Iter in Local_Symbols.Iterate loop
            declare
               Ignored1 : Boolean;
               Ignored2 : Symbol_Maps.Cursor;
            begin
               -- Insert new symbol if not present already.
               Symbol_Maps.Insert (Global_Symbol_Table,
                                   Symbol_Maps.Key (Sym_Iter),
                                   Symbol_Maps.Element (Sym_Iter),
                                   Ignored2,
                                   Ignored1);
            end;
         end loop;
      end;

      -- Generate a simple _start function that calls the entry point:
      declare
         C : List_Cursor := List_First (Program_Args);
      begin
         while List_Has_Element (Program_Args, C) loop
            -- For each argument, declare and nondet-initialise a parameter local
            -- and add it to the call argument list.
            declare
               Arg : constant Irep := List_Element (Program_Args, C);
               Arg_Type : constant Irep := Get_Type (Arg);
               Arg_Id : constant Unbounded_String :=
                 "input_" & To_Unbounded_String (Get_Identifier (Arg));
               Arg_Symbol : Symbol;
               Arg_Symbol_Expr : constant Irep := New_Irep (I_Symbol_Expr);
               Arg_Decl : constant Irep := New_Irep (I_Code_Decl);
               Arg_Nondet : constant Irep :=
                 New_Irep (I_Side_Effect_Expr_Nondet);
               Arg_Assign : constant Irep := New_Irep (I_Code_Assign);
            begin
               Arg_Symbol.Name := Arg_Id;
               Arg_Symbol.PrettyName := Arg_Id;
               Arg_Symbol.BaseName := Arg_Id;
               Arg_Symbol.Mode := To_Unbounded_String ("C");
               Arg_Symbol.SymType := Arg_Type;
               Arg_Symbol.IsStateVar := True;
               Arg_Symbol.IsLValue := True;
               Arg_Symbol.IsAuxiliary := True;
               Global_Symbol_Table.Insert (Arg_Id, Arg_Symbol);
               Set_Identifier (Arg_Symbol_Expr, To_String (Arg_Id));
               Set_Type (Arg_Symbol_Expr, Arg_Type);
               Set_Symbol (Arg_Decl, Arg_Symbol_Expr);
               Append_Op (Start_Body, Arg_Decl);
               Set_Type (Arg_Nondet, Arg_Type);
               Set_Lhs (Arg_Assign, Arg_Symbol_Expr);
               Set_Rhs (Arg_Assign, Arg_Nondet);
               Append_Op (Start_Body, Arg_Assign);
               Append_Argument (Initial_Call_Args, Arg_Symbol_Expr);
            end;
            C := List_Next (Program_Args, C);
         end loop;
      end;
      Set_Arguments (Initial_Call, Initial_Call_Args);

      -- Catch the call's return value if it has one.
      if Kind (Program_Return_Type) /= I_Empty then
         declare
            Return_Symbol : Symbol;
            Return_Expr : constant Irep := New_Irep (I_Symbol_Expr);
            Return_Id : constant Unbounded_String := To_Unbounded_String ("return'");
            Return_Decl : constant Irep := New_Irep (I_Code_Decl);
         begin
            Return_Symbol.Name := Return_Id;
            Return_Symbol.BaseName := Return_Id;
            Return_Symbol.PrettyName := Return_Id;
            Return_Symbol.Mode := To_Unbounded_String ("C");
            Return_Symbol.SymType := Program_Return_Type;
            Global_Symbol_Table.Insert (Return_Id, Return_Symbol);
            Set_Identifier (Return_Expr, To_String (Return_Id));
            Set_Type (Return_Expr, Return_Symbol.SymType);
            Set_Lhs (Initial_Call, Return_Expr);
            Set_Symbol (Return_Decl, Return_Expr);
            Append_Op (Start_Body, Return_Decl);
         end;
      end if;

      Set_Identifier (Program_Expr, To_String (Program_Symbol.Name));
      Set_Type (Program_Expr, Program_Symbol.SymType);

      Set_Function (Initial_Call, Program_Expr);

      Append_Op (Start_Body, Initial_Call);

      Start_Symbol.Name := Start_Name;
      Start_Symbol.PrettyName := Start_Name;
      Start_Symbol.BaseName := Start_Name;
      Set_Return_Type (Start_Type, Void_Type);
      Start_Symbol.SymType := Start_Type;
      Start_Symbol.Value := Start_Body;
      Start_Symbol.Mode := To_Unbounded_String ("C");
      Symbol_Maps.Insert (Global_Symbol_Table, Start_Name, Start_Symbol);

      Put_Line (Create (SymbolTable2Json (Global_Symbol_Table)).Write);
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
