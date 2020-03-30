with Namet;                   use Namet;
with Nlists;                  use Nlists;
with Uintp;                   use Uintp;
with Einfo;                   use Einfo;
with GOTO_Utils;              use GOTO_Utils;
with Symbol_Table_Info;       use Symbol_Table_Info;
with Tree_Walk;               use Tree_Walk;
with Range_Check;             use Range_Check;
with Binary_To_Hex;           use Binary_To_Hex;
with Text_IO;                 use Text_IO;

package body ASVAT_Modelling.Memcpy_Procedure is

   ---------------------------
   -- Make_Memcpy_Procedure --
   ---------------------------

--  procedure Mem_Copy (E : Entity_Id; S, D : System.Address; Size : Natural);
--  procedure Mem_Copy (E : Entity_Id; S, D : System.Address; Size : Natural)
--     is
--        S_Irep : constant Irep :=
--          Make_Symbol_Expr (Source_Location => Get_Source_Location (E),
--                            I_Type          => Make_Pointer_Type
--                              (I_Subtype => Ireps.Empty,
--                               Width     => 64),
--                            Range_Check     => False,
--                            Identifier      =>
--                              Unique_Name (Defining_Identifier (S)));
--        D_Irep : constant Irep :=
--          Make_Symbol_Expr (Source_Location => Get_Source_Location (E),
--                            I_Type          => Make_Pointer_Type
--                              (I_Subtype => Ireps.Empty,
--                               Width     => 64),
--                            Range_Check     => False,
--                            Identifier      =>
--                              Unique_Name (Defining_Identifier (D)));
--        Size_Irep : constant Irep :=
--          Make_Symbol_Expr (Source_Location => Get_Source_Location (E),
--                         I_Type          => Do_Type_Reference (Etype (Size)),
--                            Range_Check     => False,
--                            Identifier      =>
--                              Unique_Name (Defining_Identifier (Size)));
--   begin
--        Print_Irep (S_Irep);
--        Print_Irep (D);
--        Print_Irep (Size);
--      null;
--   end Mem_Copy;

   function Make_Memcpy_Procedure (E : Entity_Id) return Irep is
      Source_Location : constant Irep := Get_Source_Location (E);
      First_Param : constant Node_Id :=
        First (Parameter_Specifications (Declaration_Node (E)));
      Second_Param : constant Node_Id :=
        (if Present (First_Param) then Next (First_Param) else Types.Empty);
      Third_Param : constant Node_Id :=
        (if Present (Second_Param) then Next (Second_Param) else Types.Empty);
      Only_3_Param : constant Boolean  :=
        Present (Third_Param) and not Present (Next (Third_Param));

      Destination_Loc_Name : constant String :=
        (if Present (First_Param) then
              Get_Name_String (Chars (Defining_Identifier (First_Param)))
         else
            "");
      Destination : constant String :=
        (if Present (First_Param) then
              Unique_Name (Defining_Identifier (First_Param))
         else
            "");
      Source_Loc_Name : constant String :=
        (if Present (Second_Param) then
              Get_Name_String (Chars (Defining_Identifier (Second_Param)))
         else
            "");
      Source : constant String :=
        (if Present (Second_Param) then
              Unique_Name (Defining_Identifier (Second_Param))
         else
            "");

      Count_Loc_Name : constant String :=
        (if Present (Third_Param) then
              Get_Name_String (Chars (Defining_Identifier (Third_Param)))
         else
            "");
      Count : constant String :=
        (if Present (Third_Param) then
              Unique_Name (Defining_Identifier (Third_Param))
         else
            "");
   begin
      if Only_3_Param and then
        Destination_Loc_Name = "destination" and then
        Source_Loc_Name = "source" and then
        Count_Loc_Name = "no_of_bits"
      then
         declare
            Subprog_Id  : constant Symbol_Id := Intern (Unique_Name (E));
            Subprog_Sym : constant Symbol    :=
                Global_Symbol_Table (Subprog_Id);

            Dest_Sym_Id : constant Symbol_Id := Intern (Destination);
            Src_Sym_Id  : constant Symbol_Id := Intern (Source);
            Cnt_Sym_Id  : constant Symbol_Id := Intern (Count);

            Dest_Sym : constant Symbol :=
              Global_Symbol_Table (Dest_Sym_Id);
            Src_Sym  : constant Symbol :=
              Global_Symbol_Table (Src_Sym_Id);
            Cnt_Sym  : constant Symbol :=
              Global_Symbol_Table (Cnt_Sym_Id);

            Dest_Irep : constant Irep :=
              Typecast_If_Necessary
                (Symbol_Expr (Sym => Dest_Sym),
                 New_Type => Make_Pointer_Type (Make_Void_Type),
                 A_Symbol_Table => Global_Symbol_Table);

            Src_Irep : constant Irep :=
              Typecast_If_Necessary
                (Symbol_Expr (Sym => Src_Sym),
                 New_Type => Make_Pointer_Type (Make_Void_Type),
                 A_Symbol_Table => Global_Symbol_Table);

            Cnt_Irep : constant Irep :=
--              Typecast_If_Necessary
                Symbol_Expr (Sym => Cnt_Sym);
--                 New_Type => Make_Signedbv_Type (64),
--                 A_Symbol_Table => Global_Symbol_Table);

            Bits_To_Bytes : constant Irep :=
              Make_Constant_Expr (Source_Location => Source_Location,
                                  I_Type          => CProver_Size_T,
                                  Range_Check     => False,
                                  Value           =>
                                  --  bits to bytes (div by 8)
                                    Convert_Uint_To_Hex
                                      (Value     => UI_From_Int (8),
                                       Bit_Width => 32));

            Byte_Cnt_Irep : constant Irep :=
              Make_Op_Div (Rhs               => Bits_To_Bytes,
                           Lhs               => Cnt_Irep,
                           Div_By_Zero_Check => False,
                           Source_Location   => Source_Location,
                           Overflow_Check    => False,
                           I_Type            => CProver_Size_T,
                           Range_Check       => False);

            Memcpy_Args  : constant Irep := Make_Argument_List;
            Memcpy_Name  : constant String := "memcpy";
            Memcpy_Sym   : constant Symbol :=
              Global_Symbol_Table (Intern (Memcpy_Name));
            Memcpy_Func  : constant Irep :=
              Symbol_Expr (Sym => Memcpy_Sym);

            Memcpy_Proc_Call : constant Irep :=
             Make_Code_Function_Call
              (Arguments       => Memcpy_Args,
               I_Function      => Memcpy_Func,
               Lhs             => CProver_Nil,
               Source_Location => Source_Location,
               I_Type          => CProver_Void_T,
               Range_Check     => False);

            Subprog_Body : constant Irep :=
              Make_Code_Block (Source_Location => Source_Location,
                               I_Type          => CProver_Nil_T);
         begin
            Put_Line (Unintern (Dest_Sym.Name));
            Print_Irep (Dest_Sym.SymType);
            Put_Line (Unintern (Src_Sym.Name));
            Print_Irep (Src_Sym.SymType);
            Put_Line (Unintern (Cnt_Sym.Name));
            Print_Irep (Cnt_Sym.SymType);

            Print_Irep (Memcpy_Func);

            Print_Irep (Dest_Irep);
            Print_Irep (Src_Irep);
            Print_Irep (Cnt_Irep);
            Print_Irep (Bits_To_Bytes);
            Print_Irep (Byte_Cnt_Irep);

            Append_Argument (I     => Memcpy_Args,
                             Value => Dest_Irep);
            Append_Argument (I     => Memcpy_Args,
                             Value => Src_Irep);
            Append_Argument (I     => Memcpy_Args,
                             Value => Byte_Cnt_Irep);

            Print_Irep (Memcpy_Args);

            Print_Irep (Memcpy_Proc_Call);

            Append_Op (Subprog_Body, Memcpy_Proc_Call);

            Print_Irep (Subprog_Body);

            Put_Line (Unintern (Subprog_Id));
            Put_Line (Unintern (Subprog_Sym.Name));

            return Subprog_Body;
         end;
      else
         return Report_Unhandled_Node_Irep (E,
                                            "Make_Memcpy_Procedure",
                                            "Invalid parameters");
      end if;

   end Make_Memcpy_Procedure;

end ASVAT_Modelling.Memcpy_Procedure;
