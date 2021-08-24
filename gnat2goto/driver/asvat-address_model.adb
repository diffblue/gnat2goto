with GOTO_Utils;        use GOTO_Utils;
with Nlists;            use Nlists;
with Tree_Walk;         use Tree_Walk;
with Arrays;            use Arrays;
with Sem_Util;          use Sem_Util;
with Einfo;             use Einfo;
with Namet;             use Namet;
with Follow;            use Follow;
package body ASVAT.Address_Model is

   -------------------------
   -- Do_ASVAT_Address_Of --
   -------------------------

   function Do_ASVAT_Address_Of (N : Node_Id) return Irep is
      Ada_Type : constant Entity_Id := Underlying_Type (Etype (N));
   begin
      if Is_Array_Type (Ada_Type) and then
        not Is_Constrained (Underlying_Type (Etype (N)))
      then
         return Make_Address_Of (Make_Unconstrained_Array_Result (N));
      else
         return Make_Op_Typecast
           (Op0             => Do_Address_Of (N),
            Source_Location => Get_Source_Location (N),
            I_Type          => Make_Pointer_Type
              (I_Subtype => Make_Unsignedbv_Type (8),
               Width     => Pointer_Type_Width),
            Range_Check     => False);
      end if;
   end Do_ASVAT_Address_Of;

   ------------------------------------
   --  Get_Intrinsic_Address_Function -
   ------------------------------------

   function Get_Intrinsic_Address_Function (E : Entity_Id) return
     Address_To_Access_Functions
   is
      Fun_Name : constant String := Get_Name_String (Chars (E));
   begin
      return (if Fun_Name = "to_pointer" then
                 To_Pointer_Function
              elsif Fun_Name = "to_address" then
                 To_Address_Function
              else
                 Not_An_Address_Function);
   end Get_Intrinsic_Address_Function;

   ---------------------
   -- Make_To_Pointer --
   ---------------------

   procedure Make_To_Pointer (E : Entity_Id) is
      Address_Parameter  : constant Node_Id :=
        First (Parameter_Specifications (Declaration_Node (E)));
   begin
      pragma Assert ((Present (Address_Parameter) and
                       not Present (Next (Address_Parameter)))
                     and then not Out_Present (Address_Parameter)
                     and then
                     Nkind (Parameter_Type (Address_Parameter)) in N_Has_Etype
                     and then
                     Unique_Name
                       (Etype (Parameter_Type (Address_Parameter))) =
                       "system__address",
                     "The function To_Pointer must have a single in mode " &
                       "parameter of type System.Address");

      declare
         Source_Location : constant Irep := Get_Source_Location (E);
         Function_Name      : constant String := Unique_Name (E);
         Function_Id        : constant Symbol_Id := Intern (Function_Name);
         Function_Type      : constant Node_Id := Etype (E);

         Param_Name : constant String :=
           Unique_Name (Defining_Identifier (Address_Parameter));

         Param_Type  : constant Irep :=
           Do_Type_Reference (Etype (Parameter_Type (Address_Parameter)));

         Param_Irep : constant Irep :=
           Make_Symbol_Expr
             (Source_Location => Get_Source_Location (Address_Parameter),
              I_Type          => Param_Type,
              Range_Check     => False,
              Identifier      => Param_Name);

         Return_Type : constant Irep := Follow_Symbol_Type
           (Do_Type_Reference (Function_Type), Global_Symbol_Table);

         Return_Statement : constant Irep :=
           Make_Code_Return
             (Return_Value    => Make_Op_Typecast
                (Op0             => Param_Irep,
                 Source_Location => Source_Location,
                 I_Type          => Return_Type),
              Source_Location => Source_Location,
              I_Type          => Return_Type,
              Range_Check     => False);

         Function_Body : constant Irep := Make_Code_Block (Source_Location);

         Function_Symbol : Symbol := Global_Symbol_Table (Function_Id);
      begin
         Append_Op (Function_Body, Return_Statement);

         Function_Symbol.Value := Function_Body;
         Global_Symbol_Table.Replace (Function_Id, Function_Symbol);
      end;
   end Make_To_Pointer;

   ---------------------
   -- To_Address --
   ---------------------

   procedure Make_To_Address (E : Entity_Id) is
      Pointer_Parameter  : constant Node_Id :=
        First (Parameter_Specifications (Declaration_Node (E)));
      Function_Type      : constant Node_Id := Etype (E);
   begin
      pragma Assert ((Present (Pointer_Parameter) and
                       not Present (Next (Pointer_Parameter)))
                     and then not Out_Present (Pointer_Parameter)
                     and then
                     Kind (Do_Type_Reference
                       (Etype (Parameter_Type (Pointer_Parameter)))) =
                       I_Pointer_Type
                     and then Unique_Name (Function_Type) = "system__address",
                     "The function To_Address must have a single in mode " &
                       "parameter of an access type");

      declare
         Source_Location : constant Irep := Get_Source_Location (E);
         Function_Name      : constant String := Unique_Name (E);
         Function_Id        : constant Symbol_Id := Intern (Function_Name);

         Param_Name : constant String :=
           Unique_Name (Defining_Identifier (Pointer_Parameter));

         Param_Type  : constant Irep :=
           Do_Type_Reference (Etype (Parameter_Type (Pointer_Parameter)));

         Param_Irep : constant Irep :=
           Make_Symbol_Expr
             (Source_Location => Get_Source_Location (Pointer_Parameter),
              I_Type          => Param_Type,
              Range_Check     => False,
              Identifier      => Param_Name);

         Return_Type : constant Irep := Make_Pointer_Type
                   (I_Subtype => Make_Unsignedbv_Type (8),
                    Width     => Pointer_Type_Width);

         Return_Statement : constant Irep :=
           Make_Code_Return
             (Return_Value    => Make_Op_Typecast
                (Op0             => Param_Irep,
                 Source_Location => Source_Location,
                 I_Type          => Return_Type,
                 Range_Check     => False),
              Source_Location => Source_Location,
              I_Type          => Return_Type,
              Range_Check     => False);

         Function_Body : constant Irep := Make_Code_Block (Source_Location);

         Function_Symbol : Symbol := Global_Symbol_Table (Function_Id);
      begin
         Append_Op (Function_Body, Return_Statement);

         Function_Symbol.Value := Function_Body;
         Global_Symbol_Table.Replace (Function_Id, Function_Symbol);
      end;

   end Make_To_Address;

end ASVAT.Address_Model;
