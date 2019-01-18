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

with Follow;                use Follow;
with Ireps;                 use Ireps;
with Symbol_Table_Info;     use Symbol_Table_Info;

with Tree_Walk;             use Tree_Walk;
with Gather_Irep_Symbols;

with GNATCOLL.JSON;         use GNATCOLL.JSON;

with Sinfo;                 use Sinfo;
with Namet;                 use Namet;
with Lib;                   use Lib;
with GNAT_Utils;            use GNAT_Utils;
with GOTO_Utils;            use GOTO_Utils;

with GNAT2GOTO.Options;

package body Driver is

   procedure Translate_Standard_Types;
   procedure Initialize_CProver_Internal_Variables (Start_Body : Irep);
   procedure Add_CProver_Internal_Symbols;
   procedure Follow_Type_Declarations (Old_Table : Symbol_Table;
                                       New_Table : in out Symbol_Table);

   procedure GNAT_To_Goto (GNAT_Root : Node_Id)
   is
   begin
      Translate_Standard_Types;
      Add_CProver_Internal_Symbols;
      Translate_Compilation_Unit (GNAT_Root);
   end GNAT_To_Goto;

   procedure Initialize_CProver_Internal_Variables (Start_Body : Irep) is
      Int_32_T : constant Irep := Make_Signedbv_Type (Ireps.Empty, 32);

      procedure Initialize_CProver_Rounding_Mode;
      procedure Initialize_CProver_Rounding_Mode is
         Rounding_Mode_Sym : constant Irep := Make_Symbol_Expr
           (I_Type => Int_32_T,
            Identifier => "__CPROVER_rounding_mode",
            Source_Location => No_Location);
         Rounding_Mode_Val_Bits : constant String (1 .. 32) := (others => '0');
         Rounding_Mode_Val : constant Irep := Make_Constant_Expr
           (I_Type => Int_32_T,
            Value => Rounding_Mode_Val_Bits,
            Source_Location => No_Location);
         Initialization_Statement : constant Irep := Make_Code_Assign
           (Lhs => Rounding_Mode_Sym,
            Rhs => Rounding_Mode_Val,
            Source_Location => No_Location);
      begin
         Append_Op (Start_Body, Initialization_Statement);
      end Initialize_CProver_Rounding_Mode;

      procedure Initialize_Enum_Values;
      procedure Initialize_Enum_Values is

         procedure Initialize_Enum_Member (Member : Irep);
         procedure Initialize_Enum_Member (Member : Irep) is
            Member_Name : constant Symbol_Id := Intern
              (Get_Identifier (Member));
            Member_Symbol : constant Symbol :=
              Global_Symbol_Table.Element (Member_Name);
            Member_Assignment : constant Irep := Make_Code_Assign
              (Lhs => Symbol_Expr (Member_Symbol),
               Rhs => Member_Symbol.Value,
               Source_Location => No_Location);
         begin
            Append_Op (Start_Body, Member_Assignment);
         end Initialize_Enum_Member;

         procedure Initialize_Enum_Members (Enum_Type : Irep);
         procedure Initialize_Enum_Members (Enum_Type : Irep) is
            Members : constant Irep_List := Get_Member (Get_Body (Enum_Type));
            C : List_Cursor := List_First (Members);
         begin
            while List_Has_Element (Members, C) loop
               Initialize_Enum_Member (List_Element (Members, C));
               C := List_Next (Members, C);
            end loop;

         end Initialize_Enum_Members;
      begin
         for Sym of Global_Symbol_Table loop
            if Kind (Sym.SymType) = I_C_Enum_Type and Sym.IsType then
               Initialize_Enum_Members (Sym.SymType);
            end if;
         end loop;
      end Initialize_Enum_Values;

   begin
      Initialize_CProver_Rounding_Mode;
      Initialize_Enum_Values;
   end Initialize_CProver_Internal_Variables;

   procedure Translate_Compilation_Unit (GNAT_Root : Node_Id)
   is
      pragma Assert (Nkind (GNAT_Root) = N_Compilation_Unit);

      Void_Type : constant Irep := New_Irep (I_Void_Type);

      Start_Name : constant Symbol_Id := Intern ("__CPROVER__start");

      Start_Symbol      : Symbol;
      Start_Type        : constant Irep := New_Irep (I_Code_Type);
      Start_Body        : constant Irep := New_Irep (I_Code_Block);
      Initial_Call      : constant Irep := New_Irep (I_Code_Function_Call);
      Initial_Call_Args : constant Irep := New_Irep (I_Argument_List);

      Add_Start : Boolean;
      Program_Symbol : constant Symbol := Do_Compilation_Unit (GNAT_Root,
                                                               Add_Start);

      Sym_Tab_File : File_Type;
      Base_Name  : constant String :=
        File_Name_Without_Suffix
          (Get_Name_String (Unit_File_Name (Main_Unit)));

      Followed_Symbol_Table : Symbol_Table;
   begin
      Create (Sym_Tab_File, Out_File, Base_Name & ".json_symtab");
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
         Put_Line (Sym_Tab_File,
                   Create (SymbolTable2Json (Global_Symbol_Table)).Write);
      else
         Initialize_CProver_Internal_Variables (Start_Body);
         declare
            Program_Expr : constant Irep := New_Irep (I_Symbol_Expr);
            Program_Type : constant Irep := Program_Symbol.SymType;
            Program_Return_Type : constant Irep :=
              Get_Return_Type (Program_Type);
            Program_Args : constant Irep_List :=
              Get_Parameter (Get_Parameters (Program_Type));
         begin
            --  Generate a simple _start function that calls the entry point
            declare
               C : List_Cursor := List_First (Program_Args);
            begin
               while List_Has_Element (Program_Args, C) loop
                  --  For each argument, declare and nondet-initialise a
                  --  parameter local and add it to the call argument list.
                  declare
                     Arg      : constant Irep :=
                       List_Element (Program_Args, C);
                     Arg_Type : constant Irep := Get_Type (Arg);
                     Arg_Id   : constant Symbol_Id :=
                       Intern ("input_" &  Get_Identifier (Arg));
                     Arg_Symbol : Symbol;

                     Arg_Symbol_Expr : constant Irep :=
                       New_Irep (I_Symbol_Expr);
                     Arg_Decl        : constant Irep :=
                       New_Irep (I_Code_Decl);
                     Arg_Nondet      : constant Irep :=
                       New_Irep (I_Side_Effect_Expr_Nondet);
                     Arg_Assign      : constant Irep :=
                       New_Irep (I_Code_Assign);

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
         end;

         Append_Op (Start_Body, Initial_Call);

         Start_Symbol.Name       := Start_Name;
         Start_Symbol.PrettyName := Start_Name;
         Start_Symbol.BaseName   := Start_Name;

         Set_Return_Type (Start_Type, Void_Type);

         Start_Symbol.SymType := Start_Type;
         Start_Symbol.Value   := Start_Body;
         Start_Symbol.Mode    := Intern ("C");

         Global_Symbol_Table.Insert (Start_Name, Start_Symbol);
         Follow_Type_Declarations (Global_Symbol_Table, Followed_Symbol_Table);
         Put_Line (Sym_Tab_File,
                   Create (SymbolTable2Json (Followed_Symbol_Table)).Write);
      end if;

      Close (Sym_Tab_File);
   end Translate_Compilation_Unit;

   function Is_Back_End_Switch (Switch : String) return Boolean is
      First : constant Natural := Switch'First + 1;
      Last  : constant Natural := Switch_Last (Switch);
      use GNAT2GOTO.Options;
   begin
      --  For now we allow the -g/-O/-f/-m/-W/-w and -pipe switches, even
      --  though they will have no effect. This permits compatibility with
      --  existing scripts.
      if Is_Switch (Switch) then
         if Switch (First) in 'f' | 'g' | 'm' | 'O' | 'W' | 'w'
           or else Switch (First .. Last) = "pipe"
         then
            return True;
         elsif Switch (First .. Last) = Dump_Statement_AST_On_Error_Option
         then
            Dump_Statement_AST_On_Error := True;
            return True;
         end if;
      end if;
      return False;
   end Is_Back_End_Switch;

   ------------------------------
   -- Translate_Standard_Types --
   ------------------------------

   procedure Translate_Standard_Types is
      procedure Add_Standard_String;
      procedure Add_Standard_String is
         Builtin   : Symbol;
         Builtin_Node : constant Node_Id := Standard_String;
         Type_Irep : constant Irep := Make_String_Type;
      begin
         Builtin.Name       := Intern (Unique_Name (Builtin_Node));
         Builtin.PrettyName := Builtin.Name;
         Builtin.BaseName   := Builtin.Name;
         Builtin.SymType    := Type_Irep;
         Builtin.IsType     := True;

         Global_Symbol_Table.Insert (Builtin.Name, Builtin);
      end Add_Standard_String;

      procedure Add_Universal_Integer;
      procedure Add_Universal_Integer is
         Builtin   : Symbol;
         Builtin_Node : constant Node_Id := Universal_Integer;
         Type_Irep : constant Irep := Make_Integer_Type;
      begin
         Builtin.Name       := Intern (Unique_Name (Builtin_Node));
         Builtin.PrettyName := Builtin.Name;
         Builtin.BaseName   := Builtin.Name;
         Builtin.SymType    := Type_Irep;
         Builtin.IsType     := True;

         Global_Symbol_Table.Insert (Builtin.Name, Builtin);
      end Add_Universal_Integer;

   begin
      --  Add primitive types to the symtab
      for Standard_Type in S_Types'Range loop
         declare
            Builtin_Node : constant Node_Id := Standard_Entity (Standard_Type);

            Type_Kind : constant Irep_Kind :=
              (case Ekind (Builtin_Node) is
                 when E_Floating_Point_Type    => I_Floatbv_Type,
                 when E_Signed_Integer_Subtype => I_Signedbv_Type,
                 when E_Enumeration_Type       =>
                                                (if Standard_Type /= S_Boolean
                                                then I_Unsignedbv_Type
                                                else I_Bool_Type),
                 when others                   => I_Empty);

         begin
            if Type_Kind /= I_Empty then
               declare
                  Type_Irep : constant Irep := New_Irep (Type_Kind);
                  Builtin   : Symbol;

                  Esize_Width : constant Nat :=
                    UI_To_Int (Esize (Builtin_Node));

               begin
                  if Kind (Type_Irep) in Class_Bitvector_Type then
                     Set_Width (Type_Irep, Integer (Esize_Width));
                  end if;

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
      Add_Universal_Integer;
      Add_Standard_String;
   end Translate_Standard_Types;

   procedure Add_CProver_Internal_Symbols is
      procedure Add_Global_Sym (Name : Symbol_Id; Sym_Type : Irep);
      procedure Add_Global_Sym (Name : Symbol_Id; Sym_Type : Irep) is
         Sym : constant Symbol :=
           (Name => Name,
            BaseName => Name,
            PrettyName => Name,
            SymType => Sym_Type,
            Module => Intern (""),
            Mode => Intern ("C"),
            IsStaticLifetime => True,
            IsLValue => True,
            others => <>
           );
      begin
         Global_Symbol_Table.Insert (Name, Sym);
      end Add_Global_Sym;

      Int_32_T : constant Irep := Make_Signedbv_Type (Ireps.Empty, 32);
   begin
      Add_Global_Sym (Intern ("__CPROVER_rounding_mode"), Int_32_T);
   end Add_CProver_Internal_Symbols;

   procedure Follow_Type_Declarations (Old_Table : Symbol_Table;
                                       New_Table : in out Symbol_Table) is
      function Follow_Symbol (I : Irep) return Irep;
      function Follow_Symbol (I : Irep) return Irep is
      begin
         return Follow_Symbol_Type (I, Old_Table);
      end Follow_Symbol;
   begin
      for Sym_Iter in Old_Table.Iterate loop
         declare
            Unused_Inserted : Boolean;
            Unused_Position  : Symbol_Maps.Cursor;
            Current_Symbol : constant Symbol := Old_Table (Sym_Iter);
            Modified_Symbol : Symbol := Current_Symbol;
            SymType : constant Irep := Current_Symbol.SymType;
            Value : constant Irep := Current_Symbol.Value;
         begin
            Modified_Symbol.SymType :=
                    Follow_Irep (SymType, Follow_Symbol'Access);
            Modified_Symbol.Value :=
                    Follow_Irep (Value, Follow_Symbol'Access);

            New_Table.Insert
                 (Key      => Symbol_Maps.Key (Sym_Iter),
                  New_Item => Modified_Symbol,
                  Inserted => Unused_Inserted,
                  Position => Unused_Position);
         end;
      end loop;
   end Follow_Type_Declarations;
end Driver;
