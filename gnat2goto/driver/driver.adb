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
with Binary_To_Hex;         use Binary_To_Hex;
with Range_Check;           use Range_Check;
with ASVAT.Size_Model;

with GNAT2GOTO.Options;

package body Driver is

   procedure Translate_Standard_Types;
   procedure Initialize_CProver_Internal_Variables (Start_Body : Irep);
   procedure Add_CProver_Internal_Symbols;
   procedure Sanitise_Type_Declarations (Old_Table : Symbol_Table;
                                       New_Table : in out Symbol_Table);

   procedure Add_Malloc_Symbol;

   procedure Add_Malloc_Symbol is
      Malloc_Name : constant String := "malloc";
      Malloc_Params : constant Irep := Make_Parameter_List;
      Size_Param : constant Irep :=
        Create_Fun_Parameter (Fun_Name        => Malloc_Name,
                              Param_Name      => "size",
                              Param_Type      => CProver_Size_T,
                              Param_List      => Malloc_Params,
                              A_Symbol_Table  => Global_Symbol_Table);
      Malloc_Type : constant Irep :=
        Make_Code_Type (Parameters  => Malloc_Params,
                        Ellipsis    => False,
                        Return_Type => Make_Pointer_Type (CProver_Void_T),
                        Inlined     => False,
                        Knr         => False);
      Malloc_Symbol : Symbol;
   begin
      if Kind (Size_Param) = I_Code_Parameter then
         Malloc_Symbol :=
           New_Function_Symbol_Entry (Name           => Malloc_Name,
                                      Symbol_Type    => Malloc_Type,
                                      Value          => Ireps.Empty,
                                      A_Symbol_Table => Global_Symbol_Table);
         pragma Assert (Kind (Malloc_Symbol.SymType) = I_Code_Type);
      end if;
   end Add_Malloc_Symbol;

   procedure Add_Memcpy_Symbol;

   procedure Add_Memcpy_Symbol is
      Memcpy_Name : constant String := "memcpy";
      Memcpy_Params : constant Irep := Make_Parameter_List;
      Destination_Param : constant Irep :=
        Create_Fun_Parameter (Fun_Name        => Memcpy_Name,
                              Param_Name      => "destination",
                              Param_Type      =>
                                Make_Pointer_Type (CProver_Void_T),
                              Param_List      => Memcpy_Params,
                              A_Symbol_Table  => Global_Symbol_Table);
      Source_Param : constant Irep :=
        Create_Fun_Parameter (Fun_Name        => Memcpy_Name,
                              Param_Name      => "source",
                              Param_Type      =>
                                Make_Pointer_Type (CProver_Void_T),
                              Param_List      => Memcpy_Params,
                              A_Symbol_Table  => Global_Symbol_Table);
      Num_Param : constant Irep :=
        Create_Fun_Parameter (Fun_Name        => Memcpy_Name,
                              Param_Name      => "num",
                              Param_Type      => CProver_Size_T,
                              Param_List      => Memcpy_Params,
                              A_Symbol_Table  => Global_Symbol_Table);
      Memcpy_Type : constant Irep :=
        Make_Code_Type (Parameters  => Memcpy_Params,
                        Ellipsis    => False,
                        Return_Type => Make_Pointer_Type (Make_Void_Type),
                        Inlined     => False,
                        Knr         => False);
      Memcpy_Symbol : Symbol;
   begin
      if Kind (Destination_Param) = I_Code_Parameter and then
        Kind (Source_Param) = I_Code_Parameter and then
        Kind (Num_Param) = I_Code_Parameter
      then
         Memcpy_Symbol :=
           New_Function_Symbol_Entry (Name           => Memcpy_Name,
                                      Symbol_Type    => Memcpy_Type,
                                      Value          => Ireps.Empty,
                                      A_Symbol_Table => Global_Symbol_Table);
         pragma Assert (Kind (Memcpy_Symbol.SymType) = I_Code_Type);
      end if;
   end Add_Memcpy_Symbol;

   procedure GNAT_To_Goto (GNAT_Root : Node_Id)
   is
   begin
      Translate_Standard_Types;
      Add_CProver_Internal_Symbols;
      Add_Malloc_Symbol;
      Add_Memcpy_Symbol;
      Translate_Compilation_Unit (GNAT_Root);
   end GNAT_To_Goto;

   procedure Initialize_CProver_Internal_Variables (Start_Body : Irep) is
      Int_32_T : constant Irep := Make_Signedbv_Type (32);
      Bool_T   : constant Irep := Make_Unsignedbv_Type (CProver_Bool_Width);

      procedure Declare_Missing_Global (Symbol_Expr : Irep)
        with Pre => Kind (Symbol_Expr) = I_Symbol_Expr;

      procedure Declare_Missing_Global (Symbol_Expr : Irep) is
         Id : constant Symbol_Id := Intern (Get_Identifier (Symbol_Expr));
         Has_Symbol : constant Boolean := Global_Symbol_Table.Contains (Id);
      begin
         if not Has_Symbol then
            declare
               New_Sym : constant Symbol :=
                 (SymType => Get_Type (Symbol_Expr),
                  Value => Make_Nil (Get_Source_Location (Symbol_Expr)),
                  Mode => Intern ("C"),
                  Name => Id,
                  BaseName => Id,
                  PrettyName => Id,
                  IsStaticLifetime => True,
                  IsLValue => True,
                  others => <>);
            begin
               Global_Symbol_Table.Insert
                 (Key => Id,
                  New_Item => New_Sym);
            end;
         end if;
      end Declare_Missing_Global;

      procedure Initialize_CProver_Alloca_Object;
      procedure Initialize_CProver_Alloca_Object is
         Alloca_Object_Type : constant Irep := Make_Pointer_Type
           (I_Subtype => Make_Void_Type,
            Width => Pointer_Type_Width);
         Alloca_Object_Sym : constant Irep := Make_Symbol_Expr
           (I_Type => Alloca_Object_Type,
            Identifier => "__CPROVER_alloca_object",
            Source_Location => Internal_Source_Location);
         Alloca_Object_Val : constant Irep := Integer_Constant_To_Expr
           (Value => Uint_0,
            Expr_Type => Alloca_Object_Type,
            Source_Location => Internal_Source_Location);
      begin
         Declare_Missing_Global (Alloca_Object_Sym);
         Append_Op
           (Start_Body,
            Make_Code_Assign
              (Lhs => Alloca_Object_Sym,
               Rhs => Alloca_Object_Val,
               Source_Location => Internal_Source_Location));
      end Initialize_CProver_Alloca_Object;

      procedure Initialize_CProver_Dead_Object;
      procedure Initialize_CProver_Dead_Object is
         Dead_Object_Type : constant Irep :=
           Make_Pointer_Type (Base => Make_Void_Type);
         Dead_Object_Sym : constant Irep := Make_Symbol_Expr
           (I_Type => Dead_Object_Type,
            Identifier => "__CPROVER_dead_object",
            Source_Location => Internal_Source_Location);
         Dead_Object_Val : constant Irep := Integer_Constant_To_Expr
           (Value => Uint_0,
            Expr_Type => Dead_Object_Type,
            Source_Location => Internal_Source_Location);
      begin
         Declare_Missing_Global (Dead_Object_Sym);
         Append_Op
           (Start_Body,
            Make_Code_Assign
             (Lhs => Dead_Object_Sym,
              Rhs => Dead_Object_Val,
              Source_Location => Internal_Source_Location));
      end Initialize_CProver_Dead_Object;

      procedure Initialize_CProver_Deallocated;
      procedure Initialize_CProver_Deallocated is
         Deallocated_Type : constant Irep := Make_Pointer_Type
           (I_Subtype => Make_Void_Type,
            Width => Pointer_Type_Width);
         Deallocated_Sym : constant Irep := Make_Symbol_Expr
           (I_Type => Deallocated_Type,
            Identifier => "__CPROVER_deallocated",
            Source_Location => Internal_Source_Location);
         Deallocated_Val : constant Irep := Integer_Constant_To_Expr
           (Value => Uint_0,
            Expr_Type => Deallocated_Type,
            Source_Location => Internal_Source_Location);
      begin
         Declare_Missing_Global (Deallocated_Sym);
         Append_Op
           (Start_Body,
            Make_Code_Assign
              (Lhs => Deallocated_Sym,
               Rhs => Deallocated_Val,
               Source_Location => Internal_Source_Location));
      end Initialize_CProver_Deallocated;

      procedure Initialize_CProver_Malloc_Failure_Mode;
      procedure Initialize_CProver_Malloc_Failure_Mode is
         Malloc_Failure_Mode_Type : constant Irep :=
           Int_32_T;
         Malloc_Failure_Mode_Sym : constant Irep :=
           Make_Symbol_Expr
           (I_Type => Malloc_Failure_Mode_Type,
            Identifier => "__CPROVER_malloc_failure_mode",
            Source_Location => Internal_Source_Location);
         Malloc_Failure_Mode_Val : constant Irep :=
           Integer_Constant_To_Expr
           (Value => Uint_0,
            Expr_Type => Malloc_Failure_Mode_Type,
            Source_Location => Internal_Source_Location);
      begin
         Declare_Missing_Global (Malloc_Failure_Mode_Sym);
         Append_Op
           (Start_Body,
            Make_Code_Assign
              (Lhs => Malloc_Failure_Mode_Sym,
               Rhs => Malloc_Failure_Mode_Val,
               Source_Location => Internal_Source_Location));
      end Initialize_CProver_Malloc_Failure_Mode;

      procedure Initialize_CProver_Malloc_Failure_Mode_Assert_Then_Assume;
      procedure Initialize_CProver_Malloc_Failure_Mode_Assert_Then_Assume is
         Malloc_Failure_Mode_Assert_Then_Assume_Type : constant Irep :=
           Int_32_T;
         Malloc_Failure_Mode_Assert_Then_Assume_Sym : constant Irep :=
           Make_Symbol_Expr
           (I_Type => Malloc_Failure_Mode_Assert_Then_Assume_Type,
            Identifier => "__CPROVER_malloc_failure_mode_assert_then_assume",
            Source_Location => Internal_Source_Location);
         Malloc_Failure_Mode_Assert_Then_Assume_Val : constant Irep :=
           Integer_Constant_To_Expr
           (Value => Uint_2,
            Expr_Type => Malloc_Failure_Mode_Assert_Then_Assume_Type,
            Source_Location => Internal_Source_Location);
      begin
         Declare_Missing_Global (Malloc_Failure_Mode_Assert_Then_Assume_Sym);
         Append_Op
           (Start_Body,
            Make_Code_Assign
              (Lhs => Malloc_Failure_Mode_Assert_Then_Assume_Sym,
               Rhs => Malloc_Failure_Mode_Assert_Then_Assume_Val,
               Source_Location => Internal_Source_Location));
      end Initialize_CProver_Malloc_Failure_Mode_Assert_Then_Assume;

      procedure Initialize_CProver_Malloc_Failure_Mode_Return_Null;
      procedure Initialize_CProver_Malloc_Failure_Mode_Return_Null is
         Malloc_Failure_Mode_Return_Null_Type : constant Irep := Int_32_T;
         Malloc_Failure_Mode_Return_Null_Sym : constant Irep :=
           Make_Symbol_Expr
           (I_Type => Malloc_Failure_Mode_Return_Null_Type,
            Identifier => "__CPROVER_malloc_failure_mode_return_null",
            Source_Location => Internal_Source_Location);
         Malloc_Failure_Mode_Return_Null_Val : constant Irep :=
           Integer_Constant_To_Expr
           (Value => Uint_1,
            Expr_Type => Malloc_Failure_Mode_Return_Null_Type,
            Source_Location => Internal_Source_Location);
      begin
         Declare_Missing_Global (Malloc_Failure_Mode_Return_Null_Sym);
         Append_Op
           (Start_Body,
            Make_Code_Assign
              (Lhs => Malloc_Failure_Mode_Return_Null_Sym,
               Rhs => Malloc_Failure_Mode_Return_Null_Val,
               Source_Location => Internal_Source_Location));
      end Initialize_CProver_Malloc_Failure_Mode_Return_Null;

      procedure Initialize_CProver_Malloc_Object;
      procedure Initialize_CProver_Malloc_Object is
         Malloc_Object_Type : constant Irep := Make_Pointer_Type
           (I_Subtype => Make_Void_Type,
            Width => Pointer_Type_Width);
         Malloc_Object_Sym : constant Irep := Make_Symbol_Expr
           (I_Type => Malloc_Object_Type,
            Identifier => "__CPROVER_malloc_object",
            Source_Location => Internal_Source_Location);
         Malloc_Object_Val : constant Irep := Integer_Constant_To_Expr
           (Value => Uint_0,
            Expr_Type => Malloc_Object_Type,
            Source_Location => Internal_Source_Location);
      begin
         Declare_Missing_Global (Malloc_Object_Sym);
         Append_Op
           (Start_Body,
            Make_Code_Assign
              (Lhs => Malloc_Object_Sym,
               Rhs => Malloc_Object_Val,
               Source_Location => Internal_Source_Location));
      end Initialize_CProver_Malloc_Object;

      procedure Initialize_CProver_Max_Malloc_Size;
      procedure Initialize_CProver_Max_Malloc_Size is
         Max_Malloc_Size_Type : constant Irep := CProver_Size_T;
         Max_Malloc_Size_Sym : constant Irep :=
           Make_Symbol_Expr
           (I_Type => Max_Malloc_Size_Type,
            Identifier => "__CPROVER_max_malloc_size",
            Source_Location => Internal_Source_Location);
         Value_Hex : constant String :=
           Convert_Uint_To_Hex (Value => Uint_2 ** 23,
                                Bit_Width => Size_T_Width);
         Max_Malloc_Size_Val : constant Irep :=
           Make_Constant_Expr (Source_Location => Internal_Source_Location,
                               I_Type          => Max_Malloc_Size_Type,
                               Range_Check     => False,
                               Value           => Value_Hex);
      begin
         Declare_Missing_Global (Max_Malloc_Size_Sym);
         Append_Op
           (Start_Body,
            Make_Code_Assign
              (Lhs => Max_Malloc_Size_Sym,
               Rhs => Max_Malloc_Size_Val,
               Source_Location => Internal_Source_Location));
      end Initialize_CProver_Max_Malloc_Size;

      procedure Initialize_CProver_Rounding_Mode;
      procedure Initialize_CProver_Rounding_Mode is
         Rounding_Mode_Sym : constant Irep := Make_Symbol_Expr
           (I_Type => Int_32_T,
            Identifier => "__CPROVER_rounding_mode",
            Source_Location => Internal_Source_Location);
         Rounding_Mode_Val_Bits : constant String (1 .. 32) := (others => '0');
         Rounding_Mode_Val : constant Irep := Make_Constant_Expr
           (I_Type => Int_32_T,
            Value => Rounding_Mode_Val_Bits,
            Source_Location => Internal_Source_Location);
         Initialization_Statement : constant Irep := Make_Code_Assign
           (Lhs => Rounding_Mode_Sym,
            Rhs => Rounding_Mode_Val,
            Source_Location => Internal_Source_Location);
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
               Source_Location => Internal_Source_Location);
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

      procedure Initialize_Boolean_Values;
      procedure Initialize_Boolean_Values is
         True_Symbol_Expr : constant Irep := Make_Symbol_Expr
           (I_Type => Make_Bool_Type,
            Identifier => "standard__boolean__true",
            Source_Location => Internal_Source_Location);
         True_Val : constant Irep := Make_Op_Typecast
           (Op0 => Make_Constant_Expr
              (I_Type => Bool_T,
               Value => "1",
               Source_Location => Internal_Source_Location),
            I_Type => Make_Bool_Type,
            Source_Location => Internal_Source_Location);
         True_Assign : constant Irep := Make_Code_Assign
           (Lhs => True_Symbol_Expr,
            Rhs => True_Val,
            Source_Location => Internal_Source_Location);

         False_Symbol_Expr : constant Irep := Make_Symbol_Expr
           (I_Type => Make_Bool_Type,
            Identifier => "standard__boolean__false",
            Source_Location => Internal_Source_Location);
         False_Val : constant Irep := Make_Op_Typecast
           (Op0 => Make_Constant_Expr
              (I_Type => Bool_T,
               Value => "0",
               Source_Location => Internal_Source_Location),
            I_Type => Make_Bool_Type,
            Source_Location => Internal_Source_Location);
         False_Assign : constant Irep := Make_Code_Assign
           (Lhs => False_Symbol_Expr,
            Rhs => False_Val,
            Source_Location => Internal_Source_Location);
      begin
         --  True and false only appear in the symbol table if they're actually
         --  being used in the program, hence we need to check so we don't
         --  create assignments to undefined symbols
         if Global_Symbol_Table.Contains (Intern ("standard__boolean__true"))
         then
            Append_Op (Start_Body, True_Assign);
         end if;
         if Global_Symbol_Table.Contains (Intern ("standard__boolean__false"))
         then
            Append_Op (Start_Body, False_Assign);
         end if;
      end Initialize_Boolean_Values;

   begin
      Initialize_CProver_Alloca_Object;
      Initialize_CProver_Dead_Object;
      Initialize_CProver_Deallocated;
      Initialize_CProver_Malloc_Failure_Mode;
      Initialize_CProver_Malloc_Failure_Mode_Assert_Then_Assume;
      Initialize_CProver_Malloc_Failure_Mode_Return_Null;
      Initialize_CProver_Malloc_Object;
      Initialize_CProver_Max_Malloc_Size;
      Initialize_CProver_Rounding_Mode;
      Initialize_Enum_Values;
      Initialize_Boolean_Values;
   end Initialize_CProver_Internal_Variables;

   procedure Translate_Compilation_Unit (GNAT_Root : Node_Id)
   is
      pragma Assert (Nkind (GNAT_Root) = N_Compilation_Unit);

      Program_Symbol : constant Symbol :=
        Do_Compilation_Unit (GNAT_Root);

      The_Unit   : constant Node_Id := Unit (GNAT_Root);

      Unit_Name : constant Symbol_Id :=
        Intern (Unique_Name (Unique_Defining_Entity (The_Unit)));

      --  Only add CPROVER_Start if the unit is subprogram body
      --  and the user did not suppress it (by cmdl option).
      Add_Start : constant Boolean :=
        Nkind (The_Unit) = N_Subprogram_Body and not Suppress_Cprover_Start;

      Sym_Tab_File : File_Type;
      Base_Name  : constant String :=
        File_Name_Without_Suffix
          (Get_Name_String (Unit_File_Name (Main_Unit)));

      Sanitised_Symbol_Table : Symbol_Table;
   begin
      if Nkind (The_Unit) not in  N_Subprogram_Body | N_Package_Body or else
        not Global_Symbol_Table.Contains (Unit_Name)
      then
         --  Only non-generic compilation unit bodies generate a
         --  json symbol table. Generic declarations and their bodies
         --  do not generate json symbol tables because instances of the
         --  generic units are expanded in the AST by the front-end.
         --  Library level generic instantiations, although they are
         --  declarations, appear in the tree as unit bodies, and so
         --  gnat2goto will generate a json symbol for such instantiations
         --  As generic declarations are not translated by Do_Compilation_Unit
         --  they will not have an entry in the symbol table.
         --  Hence a unit body which does not have an entry in the
         --  global symbol table is a generic body and a
         --  json symbol table is not generated.
         return;
      end if;

      --  The compilation unit is a body, a tranlation has been completed
      --  and the symbol table populated.
      --  Now generate the json file from the symbol table
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

      if Add_Start
      then
         declare
            Start_Name : constant Symbol_Id := Intern ("__CPROVER__start");

            Start_Type        : constant Irep := Make_Code_Type
              (Return_Type => CProver_Void_T,
               Parameters => Make_Parameter_List,
               Ellipsis => False,
               Inlined => False,
               Knr => False);
            Start_Body        : constant Irep := Make_Code_Block
              (Source_Location => Internal_Source_Location);
            Start_Symbol      : constant Symbol :=
              (Name | PrettyName | BaseName => Start_Name,
               SymType => Start_Type,
               Value => Start_Body,
               Mode => Intern ("C"),
               others => <>);
            Initial_Call_Args : constant Irep := Make_Argument_List;
            Entry_Procedure : constant Irep := Symbol_Expr (Program_Symbol);
            Program_Args : constant Irep_List :=
              Get_Parameter (Get_Parameters (Get_Type (Entry_Procedure)));
         begin
            Initialize_CProver_Internal_Variables (Start_Body);
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
                     Arg_Symbol : constant Symbol :=
                       (Name | PrettyName | BaseName => Arg_Id,
                        Mode => Intern ("C"),
                        SymType => Arg_Type,
                        IsStateVar | IsLValue | IsAuxiliary => True,
                        others => <>);

                     Arg_Symbol_Expr : constant Irep :=
                       Symbol_Expr (Arg_Symbol);
                     Arg_Decl        : constant Irep := Make_Code_Decl
                       (Symbol => Arg_Symbol_Expr,
                        Source_Location => Internal_Source_Location);
                     Arg_Nondet      : constant Irep :=
                       Make_Side_Effect_Expr_Nondet
                       (I_Type => Arg_Symbol.SymType,
                        Source_Location => Internal_Source_Location);
                     Arg_Assign      : constant Irep := Make_Code_Assign
                       (Lhs => Arg_Symbol_Expr,
                        Rhs => Arg_Nondet,
                        Source_Location => Internal_Source_Location);

                  begin
                     Global_Symbol_Table.Insert (Arg_Id, Arg_Symbol);

                     Append_Argument (Initial_Call_Args, Arg_Symbol_Expr);

                     Append_Op  (Start_Body, Arg_Decl);
                     Append_Op (Start_Body, Arg_Assign);
                  end;
                  C := List_Next (Program_Args, C);
               end loop;
            end;
            --  Catch the call's return value if it has one
            if Kind (Get_Return_Type (Get_Type (Entry_Procedure)))
              /= I_Empty
            then
               declare
                  Return_Id   : constant Symbol_Id := Intern ("return'");
                  Return_Symbol : constant Symbol :=
                    (Name | BaseName | PrettyName => Return_Id,
                     Mode => Intern ("C"),
                     SymType => Get_Return_Type (Get_Type (Entry_Procedure)),
                     IsLValue => True,
                     IsStaticLifetime => True,
                     others => <>);
                  Return_Expr : constant Irep := Symbol_Expr (Return_Symbol);
                  Return_Decl : constant Irep := Make_Code_Decl
                    (Symbol => Return_Expr,
                     Source_Location => Internal_Source_Location);
                  Initial_Call      : constant Irep := Make_Code_Function_Call
                    (Arguments => Initial_Call_Args,
                     I_Function => Entry_Procedure,
                     Source_Location => Internal_Source_Location,
                     Lhs => Return_Expr);
               begin
                  Global_Symbol_Table.Insert (Return_Id, Return_Symbol);
                  Append_Op (Start_Body, Return_Decl);
                  Append_Op (Start_Body, Initial_Call);
               end;
            end if;
            Global_Symbol_Table.Insert (Start_Name, Start_Symbol);
         end;
      end if;

      Sanitise_Type_Declarations (Global_Symbol_Table,
                                  Sanitised_Symbol_Table);
      Put_Line (Sym_Tab_File,
                SymbolTable2Json (Sanitised_Symbol_Table).Write (False));
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
         elsif Switch (First .. Last) = "-no-cprover-start" then
            Suppress_Cprover_Start := True;
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

      procedure Add_CProver_Size_T;
      procedure Add_CProver_Size_T is
         Size_T : constant Irep := Make_Unsignedbv_Type
           (Width => 64);
         Size_T_Symbol   : constant Symbol :=
           (Name | PrettyName | BaseName => Intern ("__CPROVER_size_t"),
            SymType => Size_T,
            IsType => True,
            others => <>);
      begin
         Global_Symbol_Table.Insert (Size_T_Symbol.Name, Size_T_Symbol);
      end Add_CProver_Size_T;

      function Get_Bv_Width (Type_Node : Node_Id) return Integer;
      function Get_Bv_Width (Type_Node : Node_Id) return Integer
        is (Integer (UI_To_Int (Esize (Type_Node))));
      function Translate_Signed_Type (Type_Node : Node_Id)
        return Irep;
      function Translate_Enum_Type (Type_Node : Node_Id)
        return Irep;
      function Translate_Floating_Type (Type_Node : Node_Id)
        return Irep;
      function Translate_Boolean_Type (Type_Node : Node_Id)
        return Irep;
      Translated_Boolean_Type : constant Irep := CProver_Bool_T;

      function Translate_Signed_Type (Type_Node : Node_Id)
                                     return Irep
      is
         Width : constant Integer := Get_Bv_Width (Type_Node);
      begin
         ASVAT.Size_Model.Set_Static_Size (Type_Node, Width);
         if Type_Node in Standard_Natural | Standard_Positive then
            --  It is a bounded integer subtype
            declare
               Lower_Bound  : constant Uint :=
                 (if Type_Node = Standard_Natural then Uint_0 else Uint_1);
               Higher_Bound : constant Uint :=
                Intval (Type_High_Bound (Standard_Integer));
            begin
               return Make_Bounded_Signedbv_Type
                       (Width       => Width,
                        Lower_Bound =>
                          Store_Nat_Bound (Bound_Type_Nat (Lower_Bound)),
                        Upper_Bound =>
                          Store_Nat_Bound (Bound_Type_Nat (Higher_Bound)));
            end;
         else
            --  It is an unbounded integer type
            return Make_Signedbv_Type
              (Width => Width);
         end if;
      end Translate_Signed_Type;

      function Translate_Enum_Type (Type_Node : Node_Id)
                                   return Irep
      is
         Width : constant Integer := Get_Bv_Width (Type_Node);
      begin
         ASVAT.Size_Model.Set_Static_Size (Type_Node, Width);
         return
           Make_Unsignedbv_Type
             (Width => Width);
      end Translate_Enum_Type;

      function Translate_Floating_Type (Type_Node : Node_Id)
                                       return Irep
      is
         Width : constant Integer :=  Get_Bv_Width (Type_Node);
         Mantissa_Size : constant Integer := Float_Mantissa_Size (Width);
      begin
         ASVAT.Size_Model.Set_Static_Size (Type_Node, Width);
         return Make_Floatbv_Type
           (Width => Width,
            F => Mantissa_Size);
      end Translate_Floating_Type;

      function Translate_Boolean_Type (Type_Node : Node_Id)
                                      return Irep is
      begin
         ASVAT.Size_Model.Set_Static_Size (Type_Node, CProver_Bool_Width);
         return Translated_Boolean_Type;
      end Translate_Boolean_Type;

   begin
      --  Add primitive types to the symtab.
      --  It seems as though the parsing of Standard by the front-end does
      --  not generate an AST which is entirely consistent with other units,
      --  so the primitive types are added to the symbol table here.
      for Standard_Type in S_Types'Range loop
         declare
            Builtin_Node : constant Node_Id := Standard_Entity (Standard_Type);
            Translated_Type : constant Irep := (case Ekind (Builtin_Node) is
               when E_Floating_Point_Type =>
                  Translate_Floating_Type (Builtin_Node),
               when E_Signed_Integer_Subtype =>
                  Translate_Signed_Type (Builtin_Node),
               when E_Enumeration_Type =>
                  (if Standard_Type /= S_Boolean
                     then Translate_Enum_Type (Builtin_Node)
                     else Translate_Boolean_Type (Builtin_Node)),
               when others => CProver_Nil_T);
            Type_Symbol : constant Symbol :=
            (Name | PrettyName | BaseName =>
                Intern (Unique_Name (Builtin_Node)),
             SymType => Translated_Type,
             IsType => True,
             others => <>);
         begin
            if Kind (Translated_Type) /= I_Nil_Type then
               Global_Symbol_Table.Insert (Type_Symbol.Name, Type_Symbol);
            end if;
         end;
      end loop;
      Add_Universal_Integer;
      Add_Standard_String;
      Add_CProver_Size_T;
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

      --  must be int type or error when using __CPROVER_rounded_mode
      Int_32_T : constant Irep := Make_Signedbv_Type (32);
   begin
      Add_Global_Sym (Intern ("__CPROVER_rounding_mode"), Int_32_T);
   end Add_CProver_Internal_Symbols;

   --  Comprises a collection of sanitation procedures for cleaning ireps.
   --  Presently, we:
   --  1: follow symbolic types and replace them with concrete types
   --  2: replace bounded_bv types with similar signed_bv types
   --  the latter was introduced because CBMC require exact type equality in
   --  both assignments and ireps-forming relations, e.g. a+b
   procedure Sanitise_Type_Declarations (Old_Table : Symbol_Table;
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
              Remove_Extra_Type_Information
              (Follow_Irep (SymType, Follow_Symbol'Access));

            Modified_Symbol.Value :=
              Remove_Extra_Type_Information
              (Follow_Irep (Value, Follow_Symbol'Access));

            New_Table.Insert
                 (Key      => Symbol_Maps.Key (Sym_Iter),
                  New_Item => Modified_Symbol,
                  Inserted => Unused_Inserted,
                  Position => Unused_Position);
         end;
      end loop;
   end Sanitise_Type_Declarations;
end Driver;
