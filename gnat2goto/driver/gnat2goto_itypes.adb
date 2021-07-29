with Sem_Util;              use Sem_Util;
with Uintp;                 use Uintp;

with GOTO_Utils;            use GOTO_Utils;

with Range_Check;           use Range_Check;
with Symbol_Table_Info;     use Symbol_Table_Info;
with Tree_Walk;             use Tree_Walk;
with Follow;                use Follow;
with Arrays;                use Arrays;
with ASVAT.Size_Model;

package body Gnat2goto_Itypes is

   function Do_Anonymous_Type_Definition (E : Entity_Id) return Irep;

   function Do_Itype_Anonymous_Access_Type (N : Node_Id) return Irep;

   ----------------------------------
   -- Do_Anonymous_Type_Definition --
   ----------------------------------

   function Do_Anonymous_Type_Definition (E : Entity_Id) return Irep is
   begin
      case Ekind (E) is
         when E_Anonymous_Access_Type =>
            return
              Make_Pointer_Type
                (Base => Do_Type_Reference (Designated_Type (E)));

         when others =>
            return Report_Unhandled_Node_Irep (E,
                                               "Do_Anonymous_Type_Definition",
                                               "Unknown typedef");
      end case;

   end Do_Anonymous_Type_Definition;

   -------------------------------------
   -- Do_Itype_Anonymous_Access_Type --
   ------------------------------------

   function Do_Itype_Anonymous_Access_Type (N : Node_Id) return Irep is
      Typedef : constant Node_Id := Etype (N);

      New_Type : constant Irep := Do_Anonymous_Type_Definition (Typedef);
   begin
      Do_Type_Declaration (New_Type, Typedef);
      ASVAT.Size_Model.Set_Static_Size (Typedef, Pointer_Type_Width);
      return New_Type;
   end Do_Itype_Anonymous_Access_Type;

   ------------------------
   -- Do_Itype_Reference --
   ------------------------

   procedure Do_Itype_Reference (N : Node_Id) is
      Typedef : constant Node_Id := Etype (Itype (N));

      New_Type : constant Irep := Do_Anonymous_Type_Definition (Typedef);
   begin
      Do_Type_Declaration (New_Type, Typedef);
   end Do_Itype_Reference;

   -------------------
   -- Declare_Itype --
   -------------------

   procedure Declare_Itype (Ty : Entity_Id) is
   begin
      if Present (Ty) and then Is_Itype (Ty) then
         declare
            Ty_Name : constant Symbol_Id := Intern (Unique_Name (Ty));
            Ty_Symbol : Symbol;
            Ty_Cursor : Symbol_Maps.Cursor;
            Ty_New : Boolean;
         begin
            Global_Symbol_Table.Insert (Ty_Name, Ty_Symbol, Ty_Cursor, Ty_New);
            if Ty_New then
               declare
                  New_Type : constant Irep := Do_Itype_Definition (Ty);
                  New_Symbol : constant Symbol :=
                    Make_Type_Symbol (Ty_Name, New_Type);
               begin
                  Global_Symbol_Table.Replace_Element (Ty_Cursor, New_Symbol);
               end;
            end if;
         end;
      end if;
   end Declare_Itype;

   -------------------------
   -- Do_Itype_Definition --
   -------------------------

   function Do_Itype_Definition (N : Node_Id) return Irep is
   begin
      --  Most type-defining functions use the defining statement,
      --  e.g. an N_Constrained_Array_Definition. This on the other
      --  hand must reverse-engineer the type from its Entity.
      --  Possibly in the long term, since we need this anyhow, it
      --  might become the only way to get a type definition.
      return (case Ekind (N) is
         when Array_Kind => Do_Itype_Array_Subtype (N),
--           when E_Array_Type => Do_Itype_Array_Type (N),
--      when E_String_Literal_Subtype =>
--                Do_Itype_String_Literal_Subtype (N),
         when E_Signed_Integer_Subtype => Do_Itype_Integer_Subtype (N),
         when E_Record_Subtype => Do_Itype_Record_Subtype (N),
         when E_Signed_Integer_Type => Do_Itype_Integer_Type (N),
         --  An Itype can be an enumeration (sub)type.
         when E_Enumeration_Subtype => Do_Itype_Enumeration_Subtype (N),
         when E_Enumeration_Type    => Do_Itype_Enumeration_Type (N),
         when E_Floating_Point_Type => CProver_Nil,
         when E_Anonymous_Access_Type => Do_Itype_Anonymous_Access_Type (N),
         when E_Modular_Integer_Subtype => Do_Modular_Integer_Subtype (N),
         when others => Report_Unhandled_Node_Irep
          (N,
           "Do_Itype_Definition",
           "Unknown Ekind " & Entity_Kind'Image (Ekind (N))));
   end Do_Itype_Definition;

   ----------------------------
   -- Do_Itype_Array_Type --
   ----------------------------

   function Do_Itype_Array_Type (E : Entity_Id) return Irep is
      Var : constant Node_Id :=
         Object_Definition (Associated_Node_For_Itype (E));
   begin
      case Nkind (Var) is
      when N_Constrained_Array_Definition =>
         return Do_Constrained_Array_Definition (Var);
      when N_Unconstrained_Array_Definition =>
         return Do_Unconstrained_Array_Definition (Var);
      when others =>
         return Report_Unhandled_Node_Irep
           (E, "Do_Itype_Array_Type",
            "Unknown array type " & Node_Kind'Image (Nkind (Var)));
      end case;
   end Do_Itype_Array_Type;

   ----------------------------
   -- Do_Itype_Array_Subtype --
   ----------------------------

   function Do_Itype_Array_Subtype (N : Node_Id) return Irep is
   begin
      return
        Do_Array_Subtype
          (Subtype_Node => N,
           The_Entity   => N);
   end Do_Itype_Array_Subtype;

   ----------------------------------
   -- Do_Itype_Enumeration_Subtype --
   ----------------------------------

   function Do_Itype_Enumeration_Subtype (N : Entity_Id) return Irep is
   begin
      Declare_Itype (Etype (N));
      return Do_Subtype_Indication (Subtype_Indication (N), N);
   end Do_Itype_Enumeration_Subtype;

   -------------------------------
   -- Do_Itype_Enumeration_Type --
   -------------------------------

   function Do_Itype_Enumeration_Type (N : Entity_Id) return Irep is
      (Do_Enumeration_Definition (N));

   -------------------------------------
   -- Do_Itype_String_Literal_Subtype --
   -------------------------------------

   function Do_Itype_String_Literal_Subtype (N : Node_Id) return Irep is
      (Make_Symbol_Type
         (Identifier => Unique_Name (Etype (N))));

   ------------------------------
   -- Do_Itype_Integer_Subtype --
   ------------------------------

   function Do_Itype_Integer_Subtype (N : Entity_Id) return Irep is
      Lower_Bound : constant Node_Id := Low_Bound (Scalar_Range (N));
      Upper_Bound : constant Node_Id := High_Bound (Scalar_Range (N));

      --  Itype_Integer_Subtypes may not have non-static bounds as
      --  they are created for anonymous subtypes for loop variables.
      Lower_Bound_Value : constant Integer :=
        (case Nkind (Lower_Bound) is
            when N_Integer_Literal =>
               Store_Nat_Bound (Bound_Type_Nat (Intval (Lower_Bound))),
            when others =>
               Store_Symbol_Bound (Bound_Type_Symbol (
           Do_Expression (Lower_Bound))));

      Upper_Bound_Value : constant Integer :=
        (case Nkind (Upper_Bound) is
            when N_Integer_Literal =>
               Store_Nat_Bound (Bound_Type_Nat (Intval (Upper_Bound))),
            when others =>
               Store_Symbol_Bound (Bound_Type_Symbol (
           Do_Expression (Upper_Bound))));
      Width             : constant Positive :=
        Positive (UI_To_Int (Esize (N)));
   begin
      ASVAT.Size_Model.Set_Static_Size
        (E          => N,
         Model_Size => Width);
      return
        Make_Bounded_Signedbv_Type (
                       Lower_Bound => Lower_Bound_Value,
                       Upper_Bound => Upper_Bound_Value,
                       Width       => Width);
   end Do_Itype_Integer_Subtype;

   ------------------------------
   -- Do_Itype_Integer_Type --
   ------------------------------

   function Do_Itype_Integer_Type (N : Entity_Id) return Irep is
      Width             : constant Positive :=
        Positive (UI_To_Int (Esize (N)));
   begin
      ASVAT.Size_Model.Set_Static_Size
        (E          => N,
         Model_Size => Width);
      return
        Make_Bounded_Signedbv_Type
          (Lower_Bound => Store_Nat_Bound (Bound_Type_Nat (Intval (
           Low_Bound (Scalar_Range (N))))),
           Upper_Bound => Store_Nat_Bound (Bound_Type_Nat (Intval (
             High_Bound (Scalar_Range (N))))),
           Width       => Width);
   end Do_Itype_Integer_Type;

   -----------------------------
   -- Do_Itype_Record_Subtype --
   -----------------------------

   --  Don't need to record discriminant constraints in the irep
   --  representation (yet), so just an alias for its supertype.
   --  But, ASVAT size of the Itype needs to be recorded as the
   --  same as its supertype.
   function Do_Itype_Record_Subtype (N : Entity_Id) return Irep is
      Supertype : constant Entity_Id := Etype (N);
   begin
      if ASVAT.Size_Model.Has_Size (Supertype) then
         ASVAT.Size_Model.Set_Computed_Size
           (N, ASVAT.Size_Model.Computed_Size (Supertype));
      else
         Report_Unhandled_Node_Empty
           (N        => N,
            Fun_Name => "Do_Itype_Record_Subtype",
            Message  =>
              "Supertype of Itype_Record_Subtype has no ASVAT size");
      end if;
      return Do_Type_Reference (Supertype);
   end Do_Itype_Record_Subtype;

   function Do_Modular_Integer_Subtype (N : Entity_Id) return Irep is
      Modular_Type : constant Irep := Do_Type_Reference (Etype (N));
      Followed_Mod_Type : constant Irep :=
        Follow_Symbol_Type (Modular_Type, Global_Symbol_Table);

      S_Range : constant Node_Id := Scalar_Range (N);
      Lower_Bound : constant Node_Id := Low_Bound (S_Range);
      Upper_Bound : constant Node_Id := High_Bound (S_Range);

      Lower_Bound_Value : constant Integer :=
        (case Nkind (Lower_Bound) is
            when N_Integer_Literal =>
               Store_Nat_Bound (Bound_Type_Nat (Intval (Lower_Bound))),
            when others =>
               Store_Symbol_Bound (Bound_Type_Symbol (
           Do_Expression (Lower_Bound))));

      Upper_Bound_Value : constant Integer :=
        (case Nkind (Upper_Bound) is
            when N_Integer_Literal =>
               Store_Nat_Bound (Bound_Type_Nat (Intval (Upper_Bound))),
            when others =>
               Store_Symbol_Bound (Bound_Type_Symbol (
           Do_Expression (Upper_Bound))));
   begin
      pragma Assert (Kind (Followed_Mod_Type) in I_Unsignedbv_Type
                       | I_Ada_Mod_Type);

      ASVAT.Size_Model.Set_Static_Size (N, Get_Width (Followed_Mod_Type));

      if Kind (Followed_Mod_Type) = I_Ada_Mod_Type then
         return Make_Bounded_Mod_Type (Width       =>
                                         Get_Width (Followed_Mod_Type),
                                       Lower_Bound => Lower_Bound_Value,
                                       Ada_Mod_Max =>
                                         Get_Ada_Mod_Max (Followed_Mod_Type),
                                       Upper_Bound => Upper_Bound_Value);
      else
         return Make_Bounded_Unsignedbv_Type (Width       =>
                                                Get_Width (Followed_Mod_Type),
                                              Lower_Bound => Lower_Bound_Value,
                                             Upper_Bound => Upper_Bound_Value);
      end if;
   end Do_Modular_Integer_Subtype;

end Gnat2goto_Itypes;
