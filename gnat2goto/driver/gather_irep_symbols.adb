package body Gather_Irep_Symbols is

   procedure Gather (Table : in out Symbol_Table; Ir : Irep) is
      procedure Visitor (I : Irep);

      procedure Visitor (I : Irep) is
      begin
         if Kind (I) = I_Symbol_Expr then
            declare
               Id_String : constant Symbol_Id := Intern (Get_Identifier (I));

               Inserted  : Boolean;
               New_Entry : Symbol_Maps.Cursor;

            begin
               Table.Insert (Key      => Id_String,
                             Position => New_Entry,
                             Inserted => Inserted);

               if Inserted then
                  declare
                     New_Symbol : Symbol renames Table (New_Entry);
                     Expr_Type  : constant Irep := Get_Type (I);
                  begin
                     New_Symbol.SymType       := Expr_Type;
                     New_Symbol.Value         := Empty;
                     --  ??? TODO: What is a naked irep with 'id=nil'?
                     New_Symbol.Name          := Id_String;
                     New_Symbol.BaseName      := Id_String;
                     New_Symbol.PrettyName    := Id_String;
                     New_Symbol.Mode          := Intern ("C");
                     New_Symbol.IsStateVar    := True;
                     New_Symbol.IsThreadLocal := True;
                     New_Symbol.IsLValue      := True;
                  end;
               end if;
            end;

         end if;
      end Visitor;

   --  Start of processing for Gather

   begin
      Walk_Irep_Tree (Ir, Visitor'Access);
   end Gather;

end Gather_Irep_Symbols;
