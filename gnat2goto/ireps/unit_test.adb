with Ireps;         use Ireps;
with Output;        use Output;

with GNATCOLL.JSON; use GNATCOLL.JSON;

procedure Unit_Test is
   A : Irep;
   B : Irep;
   C : Irep;
begin
   B := New_Irep (I_Real_Type);

   A := New_Irep (I_Member_Expr);
   Set_Range_Check (A, True);
   Set_Component_Name (A, "wibble");
   Set_Component_Number (A, 42);
   Set_Type (A, B);

   Print_Irep (0);
   Print_Irep (1);
   Print_Irep (2);
   Print_Irep (3);

   C := New_Irep (I_Argument_List);
   Print_Irep (C);

   for I in 1 .. 3 loop
      Append_Argument (C, A);
      Print_Irep (C);
   end loop;

   for Arg of Get_Argument (C) loop
      Print_Irep (Arg);
   end loop;

   Write_Eol;
   Write_Line (Write (To_JSON (C), False));

end Unit_Test;
