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

with Ada.Text_IO;           use Ada.Text_IO;

with Sem_Util;              use Sem_Util;
with Stand;                 use Stand;
with Switch;                use Switch;
with Einfo;                 use Einfo;
with Atree;                 use Atree;
with Uintp;                 use Uintp;

with Ireps;                 use Ireps;
with Symbol_Table_Info;     use Symbol_Table_Info;

with Tree_Walk;             use Tree_Walk;
with Gather_Irep_Symbols;

with GNATCOLL.JSON;         use GNATCOLL.JSON;

with Sinfo;                 use Sinfo;

package body Driver is

   procedure Translate_Standard_Types;

   procedure GNAT_To_Goto (GNAT_Root : Node_Id)
   is
   begin
      Translate_Standard_Types;
      Translate_Compilation_Unit (GNAT_Root);
   end GNAT_To_Goto;

   procedure Translate_Compilation_Unit (GNAT_Root : Node_Id)
   is
      pragma Assert (Nkind (GNAT_Root) = N_Compilation_Unit);
      Add_Start : Boolean;
      Program_Symbol : constant Symbol := Do_Compilation_Unit (GNAT_Root,
                                                               Add_Start);

      Program_Expr        : Irep;
      Program_Type        : Irep;
      Program_Return_Type : Irep;

      Program_Args        : Irep_List;

      Void_Type : constant Irep := New_Irep (I_Void_Type);

      Start_Name : constant Symbol_Id := Intern ("_start");

      Start_Symbol      : Symbol;
      Start_Type        : constant Irep := New_Irep (I_Code_Type);
      Start_Body        : constant Irep := New_Irep (I_Code_Block);
      Initial_Call      : constant Irep := New_Irep (I_Code_Function_Call);
      Initial_Call_Args : constant Irep := New_Irep (I_Argument_List);

   begin
      --  Gather local symbols and put them in the symtab
      declare
         Local_Symbols : Symbol_Table;
      begin
         for Sym of Global_Symbol_Table loop
            if Kind (Sym.SymType) = I_Code_Type then
               Gather_Irep_Symbols.Gather (Local_Symbols, Sym.Value);
            end if;
         end loop;

         for Sym_Iter in Local_Symbols.Iterate loop
            declare
               Ignored : Boolean;
               Unused  : Symbol_Maps.Cursor;
            begin
               --  Insert new symbol if not present already
               Global_Symbol_Table.Insert
                 (Key      => Symbol_Maps.Key (Sym_Iter),
                  New_Item => Local_Symbols (Sym_Iter),
                  Inserted => Ignored,
                  Position => Unused);
            end;
         end loop;
      end;

      if not Add_Start then
         Put_Line (Create (SymbolTable2Json (Global_Symbol_Table)).Write);
         return;
      end if;

      Program_Expr := New_Irep (I_Symbol_Expr);
      Program_Type := Program_Symbol.SymType;
      Program_Return_Type := Get_Return_Type (Program_Type);
      Program_Args := Get_Parameter (Get_Parameters (Program_Type));

      --  Generate a simple _start function that calls the entry point
      declare
         C : List_Cursor := List_First (Program_Args);
      begin
         while List_Has_Element (Program_Args, C) loop
            --  For each argument, declare and nondet-initialise a parameter
            --  local and add it to the call argument list.
            declare
               Arg      : constant Irep := List_Element (Program_Args, C);
               Arg_Type : constant Irep := Get_Type (Arg);
               Arg_Id   : constant Symbol_Id :=
                 Intern ("input_" &  Get_Identifier (Arg));
               Arg_Symbol : Symbol;

               Arg_Symbol_Expr : constant Irep := New_Irep (I_Symbol_Expr);
               Arg_Decl        : constant Irep := New_Irep (I_Code_Decl);
               Arg_Nondet      : constant Irep :=
                 New_Irep (I_Side_Effect_Expr_Nondet);
               Arg_Assign      : constant Irep := New_Irep (I_Code_Assign);

            begin
               Arg_Symbol.Name        := Arg_Id;
               Arg_Symbol.PrettyName  := Arg_Id;
               Arg_Symbol.BaseName    := Arg_Id;
               Arg_Symbol.Mode        := Intern ("C");
               Arg_Symbol.SymType     := Arg_Type;
               Arg_Symbol.IsStateVar  := True;
               Arg_Symbol.IsLValue    := True;
               Arg_Symbol.IsAuxiliary := True;
               Global_Symbol_Table.Insert (Arg_Id, Arg_Symbol);

               Set_Identifier (Arg_Symbol_Expr, Unintern (Arg_Id));
               Set_Type       (Arg_Symbol_Expr, Arg_Type);

               Set_Symbol (Arg_Decl, Arg_Symbol_Expr);
               Append_Op  (Start_Body, Arg_Decl);

               Set_Type (Arg_Nondet, Arg_Type);
               Set_Lhs  (Arg_Assign, Arg_Symbol_Expr);
               Set_Rhs  (Arg_Assign, Arg_Nondet);

               Append_Op (Start_Body, Arg_Assign);

               Append_Argument (Initial_Call_Args, Arg_Symbol_Expr);
            end;
            C := List_Next (Program_Args, C);
         end loop;
      end;
      Set_Arguments (Initial_Call, Initial_Call_Args);

      --  Catch the call's return value if it has one
      if Kind (Program_Return_Type) /= I_Empty then
         declare
            Return_Symbol : Symbol;

            Return_Expr : constant Irep := New_Irep (I_Symbol_Expr);
            Return_Decl : constant Irep := New_Irep (I_Code_Decl);
            Return_Id   : constant Symbol_Id := Intern ("return'");
         begin
            Return_Symbol.Name       := Return_Id;
            Return_Symbol.BaseName   := Return_Id;
            Return_Symbol.PrettyName := Return_Id;
            Return_Symbol.Mode       := Intern ("C");
            Return_Symbol.SymType    := Program_Return_Type;
            Global_Symbol_Table.Insert (Return_Id, Return_Symbol);

            Set_Identifier (Return_Expr, Unintern (Return_Id));
            Set_Type (Return_Expr, Return_Symbol.SymType);
            Set_Lhs (Initial_Call, Return_Expr);
            Set_Symbol (Return_Decl, Return_Expr);
            Append_Op (Start_Body, Return_Decl);
         end;
      end if;

      Set_Identifier (Program_Expr, Unintern (Program_Symbol.Name));
      Set_Type (Program_Expr, Program_Symbol.SymType);

      Set_Function (Initial_Call, Program_Expr);

      Append_Op (Start_Body, Initial_Call);

      Start_Symbol.Name       := Start_Name;
      Start_Symbol.PrettyName := Start_Name;
      Start_Symbol.BaseName   := Start_Name;

      Set_Return_Type (Start_Type, Void_Type);

      Start_Symbol.SymType := Start_Type;
      Start_Symbol.Value   := Start_Body;
      Start_Symbol.Mode    := Intern ("C");

      Global_Symbol_Table.Insert (Start_Name, Start_Symbol);

      Put_Line (Create (SymbolTable2Json (Global_Symbol_Table)).Write);
   end Translate_Compilation_Unit;

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

   ------------------------------
   -- Translate_Standard_Types --
   ------------------------------

   procedure Translate_Standard_Types is
   begin
      --  Add primitive types to the symtab
      for Standard_Type in S_Types'Range loop
         declare
            Builtin_Node : constant Node_Id := Standard_Entity (Standard_Type);

            Type_Kind : constant Irep_Kind :=
              (case Ekind (Builtin_Node) is
                 when E_Floating_Point_Type    => I_Floatbv_Type,
                 when E_Signed_Integer_Subtype => I_Signedbv_Type,
                 when E_Enumeration_Type       => I_Unsignedbv_Type,
                 when others                   => I_Empty);

         begin
            if Type_Kind /= I_Empty then
               declare
                  Type_Irep : constant Irep := New_Irep (Type_Kind);
                  Builtin   : Symbol;

                  Esize_Width : constant Nat :=
                    UI_To_Int (Esize (Builtin_Node));

               begin
                  Set_Width (Type_Irep, Integer (Esize_Width));

                  if Type_Kind = I_Floatbv_Type then
                     --  Ada's floating-point types are interesting, as they're
                     --  specified in terms of decimal precision. Entirely too
                     --  interesting for now... Let's use float32 or float64
                     --  for now and fix this later.

                     Set_F (Type_Irep,
                            (case Esize_Width is
                                when 32     => 23,
                                --  23-bit mantissa, 8-bit exponent

                                when 64     => 52,
                                --  52-bit mantissa, 11-bit exponent

                                when others => raise Program_Error));
                  end if;

                  Builtin.Name       := Intern (Unique_Name (Builtin_Node));
                  Builtin.PrettyName := Builtin.Name;
                  Builtin.BaseName   := Builtin.Name;
                  Builtin.SymType    := Type_Irep;
                  Builtin.IsType     := True;

                  Global_Symbol_Table.Insert (Builtin.Name, Builtin);
               end;
            end if;
         end;
      end loop;
   end Translate_Standard_Types;

end Driver;
