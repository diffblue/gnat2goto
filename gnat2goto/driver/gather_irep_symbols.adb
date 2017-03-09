
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Gather_Irep_Symbols is

   procedure Gather (Table : in out Symbol_Table; Ir : Irep) is
   begin
      if (To_String(Ir.Id) = "symbol") then
        declare
           Id : constant Unbounded_String :=
             Irep_Maps.Element (Ir.Named_Sub, To_Unbounded_String ("identifier")).Id;
        begin
           if not Symbol_Maps.Contains (Table, Id) then
              declare
                 Expr_Type : Irep :=
                   Irep_Maps.Element (Ir.Named_Sub, To_Unbounded_String ("type")).all;
                 New_Symbol : Symbol;
              begin
                 New_Symbol.SymType := Expr_Type;
                 New_Symbol.Value := Trivial.Trivial_Irep ("nil");
                 New_Symbol.Name := Id;
                 New_Symbol.BaseName := Id;
                 New_Symbol.PrettyName := Id;
                 New_Symbol.Mode := To_Unbounded_String ("C");
                 New_Symbol.IsStateVar := True;
                 New_Symbol.IsThreadLocal := True;
                 New_Symbol.IsLValue := True;
                 Symbol_Maps.Insert (Table, Id, New_Symbol);
              end;
           end if;
        end;
      else
         for Sub of Ir.Sub loop
            Gather (Table, Sub.all);
         end loop;
         for Sub_Pair in Ir.Named_Sub.Iterate loop
            Gather (Table, Irep_Maps.Element (Sub_Pair).all);
         end loop;
         for Sub_Pair in Ir.Comment.Iterate loop
            Gather (Table, Irep_Maps.Element (Sub_Pair).all);
         end loop;
      end if;
   end;

end Gather_Irep_Symbols;
