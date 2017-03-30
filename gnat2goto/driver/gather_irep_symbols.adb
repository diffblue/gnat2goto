with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Gather_Irep_Symbols is

   procedure Gather (Table : in out Symbol_Table; Ir : Irep) is
      procedure Visitor (I : Irep);

      procedure Visitor (I : Irep) is
         Id_String  : Unbounded_String;
         Expr_Type  : Irep;
         New_Symbol : Symbol;
      begin
         if Kind (I) = I_Symbol_Expr then
            Id_String := To_Unbounded_String (Get_Identifier (I));
            if not Symbol_Maps.Contains (Table, Id_String) then
               Expr_Type := Get_Type (I);

               New_Symbol.SymType       := Expr_Type;
               New_Symbol.Value         := Empty;
               --  ??? TODO: What is a naked irep with 'id=nil'?
               New_Symbol.Name          := Id_String;
               New_Symbol.BaseName      := Id_String;
               New_Symbol.PrettyName    := Id_String;
               New_Symbol.Mode          := To_Unbounded_String ("C");
               New_Symbol.IsStateVar    := True;
               New_Symbol.IsThreadLocal := True;
               New_Symbol.IsLValue      := True;
               Symbol_Maps.Insert (Table, Id_String, New_Symbol);
            end if;
         end if;
      end Visitor;
   begin
      Walk_Irep_Tree (Ir, Visitor'Access);
   end Gather;

end Gather_Irep_Symbols;
