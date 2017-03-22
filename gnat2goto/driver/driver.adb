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
      Program_Expr : Irep_Symbol_Expr := Make_Irep_Symbol_Expr;
      Program_Type : constant Irep_Code_Type := Irep_Code_Type (Program_Symbol.SymType);
      Program_Return_Type : constant Irep_Type :=
        Irep_Type (Program_Type.Named_Sub (To_Unbounded_String ("return_type")).all);
      Program_Args : constant Irep_Parameter_List :=
        Irep_Parameter_List (Program_Type.Named_Sub (To_Unbounded_String ("parameters")).all);
      Void_Type : constant Irep_Void_Type := Make_Irep_Void_Type;
      Start_Name : constant Unbounded_String := To_Unbounded_String ("_start");
      Start_Symbol : Symbol;
      Start_Type : Irep_Code_Type := Make_Irep_Code_Type;
      Start_Body : Irep_Code_Block := Make_Irep_Code_Block;
      Initial_Call : Irep_Code_Function_Call := Make_Irep_Code_Function_Call;
      Initial_Call_Args : Irep_Argument_List := Make_Irep_Argument_List;
   begin
      -- Gather local symbols and put them in the symtab:
      declare
         Local_Symbols : Symbol_Table;
      begin
         for Sym_Iter in Global_Symbol_Table.Iterate loop
            if Global_Symbol_Table (Sym_Iter).SymType.Id = To_Unbounded_String("code") then
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
      for Arg_Ptr of Program_Args.Sub loop
         -- For each argument, declare and nondet-initialise a parameter local
         -- and add it to the call argument list.
         declare
            Arg : constant Irep := Arg_Ptr.all;
            Arg_Type : Irep := Arg.Named_Sub (To_Unbounded_String ("type")).all;
            Arg_Id : constant Unbounded_String :=
              "input_" & Arg.Comment (To_Unbounded_String ("identifier")).all.Id;
            Arg_Symbol : Symbol;
            Arg_Symbol_Expr : Irep_Symbol_Expr := Make_Irep_Symbol_Expr;
            Arg_Decl : Irep_Code_Decl := Make_Irep_Code_Decl;
            Arg_Nondet : Irep_Side_Effect_Expr_Nondet :=
              Make_Irep_Side_Effect_Expr_Nondet;
            Arg_Assign : Irep_Code_Assign := Make_Irep_Code_Assign;
         begin
            Arg_Symbol.Name := Arg_Id;
            Arg_Symbol.PrettyName := Arg_Id;
            Arg_Symbol.BaseName := Arg_Id;
            Arg_Symbol.Mode := To_Unbounded_String ("C");
            Arg_Symbol.Value := Trivial.Trivial_Irep ("nil");
            Arg_Symbol.SymType := Arg_Type;
            Arg_Symbol.IsStateVar := True;
            Arg_Symbol.IsLValue := True;
            Arg_Symbol.IsAuxiliary := True;
            Global_Symbol_Table.Insert (Arg_Id, Arg_Symbol);
            Set_Identifier (Arg_Symbol_Expr, To_String (Arg_Id));
            Set_Type (Arg_Symbol_Expr, Arg_Type);
            Set_Symbol (Arg_Decl, Irep (Arg_Symbol_Expr));
            Add_Op (Start_Body, Irep (Arg_Decl));
            Set_Type (Arg_Nondet, Arg_Type);
            Set_Lhs (Arg_Assign, Irep (Arg_Symbol_Expr));
            Set_Rhs (Arg_Assign, Irep (Arg_Nondet));
            Add_Op (Start_Body, Irep (Arg_Assign));
            Add_Argument (Initial_Call_Args, Irep (Arg_Symbol_Expr));
         end;
      end loop;
      Set_Arguments (Initial_Call, Irep (Initial_Call_Args));

      -- Catch the call's return value if it has one.
      if Program_Return_Type.Id /= To_Unbounded_String ("empty") then
         declare
            Return_Symbol : Symbol;
            Return_Expr : Irep_Symbol_Expr := Make_Irep_Symbol_Expr;
            Return_Id : constant Unbounded_String := To_Unbounded_String ("return'");
            Return_Decl : Irep_Code_Decl := Make_Irep_Code_Decl;
         begin
            Return_Symbol.Name := Return_Id;
            Return_Symbol.BaseName := Return_Id;
            Return_Symbol.PrettyName := Return_Id;
            Return_Symbol.Mode := To_Unbounded_String ("C");
            Return_Symbol.Value := Trivial.Trivial_Irep ("nil");
            Return_Symbol.SymType := Irep (Program_Return_Type);
            Global_Symbol_Table.Insert (Return_Id, Return_Symbol);
            Set_Identifier (Return_Expr, To_String (Return_Id));
            Set_Type (Return_Expr, Return_Symbol.SymType);
            Set_Lhs (Initial_Call, Irep (Return_Expr));
            Set_Symbol (Return_Decl, Irep (Return_Expr));
            Add_Op (Start_Body, Irep (Return_Decl));
         end;
      else
         Set_Lhs (Initial_Call, Trivial.Trivial_Irep ("nil"));
      end if;

      Set_Identifier (Program_Expr, To_String (Program_Symbol.Name));
      Set_Type (Program_Expr, Program_Symbol.SymType);

      Set_Function (Initial_Call, Irep (Program_Expr));

      Add_Op (Start_Body, Irep (Initial_Call));

      Start_Symbol.Name := Start_Name;
      Start_Symbol.PrettyName := Start_Name;
      Start_Symbol.BaseName := Start_Name;
      Set_Return_Type (Start_Type, Irep (Void_Type));
      Start_Symbol.SymType := Irep (Start_Type);
      Start_Symbol.Value := Irep (Start_Body);
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
