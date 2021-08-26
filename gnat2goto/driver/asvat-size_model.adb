with Opt;
with Snames;                  use Snames;
with Atree;                   use Atree;
with Sinfo;                   use Sinfo;
with Einfo;                   use Einfo;
with Sem_Util;                use Sem_Util;
with Sem_Aux;                 use Sem_Aux;
with System;                  use System;
with Tree_Walk;               use Tree_Walk;
with GOTO_Utils;              use GOTO_Utils;

package body ASVAT.Size_Model is

   procedure Try_Base_Type_Size (N : Node_Id;
                                 E : Entity_Id; The_Size : in out Uint)
   with Pre => Is_Type (E);
   --  If the front-end is unable to provide a value for attribute_size,
   --  try its base-type.  My be the front-end has a attribute_size value
   --  for the base-type.

   function Do_Vanilla_Size (The_Prefix : Node_Id) return Irep;

   function Do_VADS_Size (The_Prefix : Node_Id) return Irep;

   -----------------------
   -- Do_Attribute_Size --
   -----------------------

   function Do_Attribute_Size (N : Node_Id) return Irep
   is
      --  S'Size where S is a scalar subtype is nearly always
      --  known at compile time, and the front-end substitues a literal
      --  into the AST replacing the call to attribute size (and Value_Size).
      --  In such cases, obviously, this function is not called.
      --
      --  As documented in the front-end package declaration, Einfo, the
      --  the value retuned by attribute Size are "peculiar" and they are
      --  further complicated by the pragma Use_VADS_Size which causes the
      --  front-end to take the VADS size attribute iterpretation.
      --  The pragma Use_VADS_Size is applied as part of the Rational profile.
      --
      --  The Ada RM states:
      --  S'Size for every subtype S:
      --    If S is definite, denotes the size (in bits) that the
      --    implementation would choose for the following objects of subtype S:
      --      A record component of subtype S when the record type is packed.
      --      The formal parameter of an instance of Unchecked_Conversion
      --         that converts from subtype S to some other subtype.
      --    If S is indefinite, the meaning is implementation defined.
      --  For a prefix X that denotes an object:
      --    Denotes the size in bits of the representation of the object.
      --
      --  The `'VADS_Size' attribute is intended to make it easier to port
      --  legacy code which relies on the semantics of `'Size' as implemented
      --  by the VADS Ada 83 compiler.  GNAT makes a best effort at
      --  duplicating the same semantic interpretation.
      --  In particular, `'VADS_Size' applied to a predefined or other
      --  primitive type with no Size clause yields the Object_Size
      --  (for example, `Natural'Size' is 32 rather than 31 on
      --  typical machines) [ASVAT-TJJ this is done by the front-end].
      --  In addition `'VADS_Size' applied to an object gives the result that
      --  would be obtained by applying it to the corresponding type.
      --
      --  From Einfo:
      --    The function RM_Size should be used to obtain the value of S'Size.
      --    [ASVAT TJJ: RM_Size has the value 0 if size of the subtype is not
      --     known to the front-end].
      --    The function Esize should be used to obtain the value of X'Size.
      --
      --  Esize can be applied to a subtype in which case it gives the default
      --  size of an object of that type, or an object.
      --  Often, it returns 0 if the size of the type or object is not known
      --  by the front-end.
      --
      --  The size of a definite subtype can be specified using a
      --  size representaion clause and then the front-end knows this value.
      --  Etype (and RM_Type) return 0 even if a size representation clause
      --  is applied to an indefinite type.
      --  A constrained (definite) subtype of an indefinite subtype can have
      --  a Value_Size attribute applied and than the front-end knows this
      --  value.
      --
      --  There are still other occasions when the front end returns 0 for one
      --  or other or both RM_Size and Esize.
      --
      --  The size of an object may be different for the target implementation
      --  and the ASVAT model.  Since the size of the model is important when
      --  analysing the model, when not using VADS_Size, the size of an object
      --  X'Size returns the model object size and is likely to be different
      --  to the target object size.
      --
      --  If either RM_Size or Esize return 0 the size of the ASVAT model
      --  for the entity is returned.

      The_Prefix       : constant Node_Id := Prefix (N);
      Prefix_Etype  : constant Entity_Id := Etype (The_Prefix);

      Use_VADS_Size : constant Boolean :=
        Opt.Use_VADS_Size or else
        Get_Attribute_Id (Attribute_Name (N)) = Attribute_VADS_Size;

      --  The front end size is the size that may be obtained from
      --  the front-end.
      --  The size that is to be returned by the attribute_size.
      Front_End_Size : Uint := Uint_0;
      The_Size      : Irep;

   begin
      if not Is_Definite_Subtype (Prefix_Etype) then
         Report_Unhandled_Node_Empty
           (The_Prefix,
            "Do_Attribute_Size",
            "Size attribute applied to indefinite " &
              "type is implementation defined");
         --  The size returned by the front-end is almost certainly 0
         --  when attribute_size is applied to an indefinite type which has
         --  no size representation clause,
         --  and there is not really anything gnat2goto cand do.
         --  Ada RM states that the result is implementation dependent
         --  but the back-end does put out a sensible value, which of course,
         --  gnat2goto has no access to.
         --  Reporting an unhandled node is the best it can do.
         Front_End_Size := RM_Size (Prefix_Etype);
         The_Size := Integer_Constant_To_Expr
           (Value           => Front_End_Size,
            Expr_Type       => Int32_T,
            Source_Location => Get_Source_Location (N));

      elsif Nkind (The_Prefix) in N_Has_Entity and then
        Is_Type (Entity (The_Prefix)) and then
      --  First check if a size representation clause has been applied
      --  directly to this type
        ASVAT.Size_Model.Has_Size_Rep_Clause (Entity (The_Prefix))
      then
         Front_End_Size := ASVAT.Size_Model.Get_Rep_Size (Entity (The_Prefix));
      elsif Use_VADS_Size then
         --  If this attribute is used or
         --  the option is set by prgma Use_VADS_Size, which is part of
         --  pragma Profile (Rational), for  an object, X, X'Size
         --  is equivalent to Value_Size (except for primitive and basic
         --  types which are handled by the front end). Therfore,
         --  RM_Size can be used for both S'Size and X'Size.
         The_Size := Do_VADS_Size (The_Prefix);
      else
         The_Size := Do_Vanilla_Size (The_Prefix);
      end if;

      --  If the value of Front-End-Size > 0 then a size has been obtained
      --  from the front-end and should be used as the size.
      if Front_End_Size > Uint_0 then
         The_Size := Integer_Constant_To_Expr
           (Value           => Front_End_Size,
            Expr_Type       => Int32_T,
            Source_Location => Get_Source_Location (N));
      end if;

      return The_Size;
   end Do_Attribute_Size;

   -------------------------
   -- Has_Size_Rep_Clause --
   -------------------------

   function Has_Size_Rep_Clause (E : Entity_Id) return Boolean is
      Enty_Id : constant Symbol_Id := Intern (Unique_Name (E));
   begin
      return Extra_Entity_Info.Contains (Enty_Id) and then
        Extra_Entity_Info (Enty_Id).Specified_Rep_Size /= Uint_0;
   end Has_Size_Rep_Clause;

   ------------------
   -- Get_Rep_Size --
   ------------------

   function Get_Rep_Size (E : Entity_Id) return Uint is
      Enty_Id : constant Symbol_Id := Intern (Unique_Name (E));
   begin
      return Extra_Entity_Info (Enty_Id).Specified_Rep_Size;
   end Get_Rep_Size;

   ------------------
   -- Set_Rep_Size --
   ------------------

   procedure Set_Rep_Size (E : Entity_Id; Size : Uint) is
      Enty_Id : constant Symbol_Id := Intern (Unique_Name (E));
      Already_In_Table : constant Boolean :=
        Extra_Entity_Info.Contains (Enty_Id);
      Info : Entity_Info :=
        (if Already_In_Table then
            Extra_Entity_Info (Enty_Id)
         else
            Empty_Entity_Info);
   begin
      Info.Specified_Rep_Size := Size;
      if Already_In_Table then
         Extra_Entity_Info.Replace (Enty_Id, Info);
      else
         Extra_Entity_Info.Insert (Enty_Id, Info);
      end if;
   end Set_Rep_Size;

   -----------------------------------
   -- Has_Component_Size_Rep_Clause --
   -----------------------------------

   function Has_Component_Size_Rep_Clause (E : Entity_Id) return Boolean is
      Enty_Id : constant Symbol_Id := Intern (Unique_Name (E));
   begin
      return Extra_Entity_Info.Contains (Enty_Id) and then
        Extra_Entity_Info (Enty_Id).Specified_Rep_Component_Size /= Uint_0;
   end Has_Component_Size_Rep_Clause;

   ----------------------------
   -- Get_Rep_Component_Size --
   ----------------------------

   function Get_Rep_Component_Size (E : Entity_Id) return Uint is
      Enty_Id : constant Symbol_Id := Intern (Unique_Name (E));
   begin
      return Extra_Entity_Info (Enty_Id).Specified_Rep_Component_Size;
   end Get_Rep_Component_Size;

   ----------------------------
   -- Set_Rep_Component_Size --
   ----------------------------

   procedure Set_Rep_Component_Size (E : Entity_Id; Size : Uint) is
      Enty_Id : constant Symbol_Id := Intern (Unique_Name (E));
      Already_In_Table : constant Boolean :=
        Extra_Entity_Info.Contains (Enty_Id);
      Info : Entity_Info :=
        (if Already_In_Table then
            Extra_Entity_Info (Enty_Id)
         else
            Empty_Entity_Info);
   begin
      Info.Specified_Rep_Component_Size := Size;
      if Already_In_Table then
         Extra_Entity_Info.Replace (Enty_Id, Info);
      else
         Extra_Entity_Info.Insert (Enty_Id, Info);
      end if;
   end Set_Rep_Component_Size;

   -------------------------
   -- Try_Base_Type_Size  --
   -------------------------

   procedure Try_Base_Type_Size (N : Node_Id;
                                 E : Entity_Id; The_Size : in out Uint) is
      Base_T : constant Entity_Id :=
        Implementation_Base_Type (E);
      Base_Type_Size : constant Uint := RM_Size (Base_T);
      pragma Unreferenced (N);
   begin
      if not Is_Base_Type (E) then
         --  If the type of the enitiy is a subtype, check whether the
         --  size of its base type is known.
         if Base_Type_Size /= Uint_0 then
            The_Size := Base_Type_Size;
         elsif ASVAT.Size_Model.Has_Size_Rep_Clause (Base_T) then
            The_Size := ASVAT.Size_Model.Get_Rep_Size (Base_T);
         end if;
      end if;
   end Try_Base_Type_Size;

   -------------------
   -- Do_VADS_Size  --
   -------------------

   function Do_VADS_Size (The_Prefix : Node_Id) return Irep is
      Prefix_Etype  : constant Entity_Id := Etype (The_Prefix);
      Front_End_Size : Uint := Uint_0;
      The_Size : Irep;
   begin
      if Nkind (The_Prefix) = N_Slice and then
        (Is_Packed_Array (Etype (Prefix (The_Prefix))) or
             Has_Size_Clause (Etype (Prefix (The_Prefix))))
      then
         --  When 'Size is applied to a slice, Esize returns 0 and
         --  RM_Size returns the size of the type of the components
         --  times the number of components in the slice.
         --  It does not take account of any packing.  The back-end does,
         --  however , so there could be discrepencies.
         --  Gnat2goto issues a warning report.

         Report_Unhandled_Node_Empty
           (The_Prefix,
            "Do_Attribute_Size",
            "A Size attribute applied to a slice " &
              "may give an inaccurate value when the array is packed");

         Front_End_Size := RM_Size (Prefix_Etype);

      elsif Nkind (The_Prefix) = N_Indexed_Component then
         declare
            The_Array : constant Node_Id := Prefix (The_Prefix);
            The_Array_Type : constant Entity_Id := Etype (The_Array);
            The_Array_Is_Packed : constant Boolean :=
              (Nkind (The_Array) in N_Has_Entity and then
                   (Is_Packed_Array (Entity (The_Array)) or else
                    ASVAT.Size_Model.Has_Size_Rep_Clause
                      (Entity (The_Array))))
              or else
                (Is_Packed_Array (The_Array_Type) or else
                 ASVAT.Size_Model.Has_Size_Rep_Clause
                   (The_Array_Type));
         begin
            if The_Array_Is_Packed then
               Front_End_Size := RM_Size (Prefix_Etype);
            else
               Front_End_Size := Esize (Prefix_Etype);
            end if;
         end;
      else
         Front_End_Size := RM_Size (Prefix_Etype);
      end if;

      if Front_End_Size <= Uint_0 then
         --  The front-end does not give the size of the entity.
         if Nkind (The_Prefix) in N_Has_Entity and then
         --  First check if it is an object and the object size can be
         --  used.
           Is_Object (Entity (The_Prefix)) and then
           Esize (Entity (The_Prefix)) /= Uint_0
         then
            Front_End_Size := Esize (Entity (The_Prefix));
         else
            Try_Base_Type_Size (The_Prefix, Prefix_Etype, Front_End_Size);
         end if;
      end if;

      if Front_End_Size > Uint_0 then
         --  The size has been obtained from the front-end.
         --  Use this value for the size.
         The_Size := Integer_Constant_To_Expr
           (Value           => Front_End_Size,
            Expr_Type       => Int32_T,
            Source_Location => Get_Source_Location (The_Prefix));
      else
         --  As all other possibilities have been tried use the
         --  ASVAT_Model_Size
         if Has_Size (Prefix_Etype) then
            The_Size := Computed_Size (Prefix_Etype);
         else
            Report_Unhandled_Node_Empty
              (The_Prefix,
               "Do_VADS_Size",
               "The size of the composite is not known " &
                 "by the front-end. Use a size " &
                 "representation clause on its declaration");
            The_Size := Get_Int32_T_Zero;
         end if;
      end if;
      return The_Size;
   end Do_VADS_Size;

   ----------------------
   -- Do_Vanilla_Size  --
   ----------------------

   function Do_Vanilla_Size (The_Prefix : Node_Id) return Irep is
      Prefix_Etype  : constant Entity_Id := Etype (The_Prefix);
      Front_End_Size : Uint := Uint_0;
      The_Size       : Irep := Ireps.Empty;
   begin
      if Nkind (The_Prefix) in N_Has_Entity | N_Selected_Component then
         declare
            The_Entity : constant Entity_Id :=
              (if Nkind (The_Prefix) =
                   N_Selected_Component
               then
                  Entity (Selector_Name (The_Prefix))
               else
                  Entity (The_Prefix));
         begin
            if Is_Type (The_Entity) then
               --  For a prefix that is a subtype mark, type RM_Size is used.
               Front_End_Size := RM_Size (The_Entity);
               if Front_End_Size <= Uint_0 then
                  --  Try using the base type of the entity.
                  Try_Base_Type_Size
                    (The_Prefix, Etype (The_Entity), Front_End_Size);
               end if;
               if Front_End_Size <= Uint_0 then
                  --  Unable to obtain the RM Size
                  --  Try using the ASVAT model size
                  if Has_Size (The_Entity) then
                     The_Size := Computed_Size (The_Entity);
                  else
                     --  The size has not been computed try using the
                     --  front-end Esize.
                     Front_End_Size := Esize (The_Entity);
                  end if;
               end if;
            elsif Is_Object (The_Entity) then
               --  The ASVAT model size should be used for an object.
               if Has_Size (The_Entity) then
                  The_Size := Computed_Size (The_Entity);
               else
                  --  Try obtaining the size from the front-end.
                  declare
                     Object_Size : constant Uint :=
                       Esize (The_Entity);
                     Default_Obj_Size : constant Uint :=
                       Esize (Etype (The_Entity));
                     The_Size_To_Use : constant Uint :=
                       (if Object_Size > Uint_0 then
                           Object_Size
                        elsif Default_Obj_Size > Uint_0 then
                           Default_Obj_Size
                        else
                           Make_Byte_Aligned_Size
                          (RM_Size (Etype (The_Entity))));
                  begin
                     Front_End_Size := The_Size_To_Use;

                     if Front_End_Size <= Uint_0 then
                        Try_Base_Type_Size
                          (The_Prefix,
                           Etype (The_Entity), Front_End_Size);
                        if Front_End_Size > Uint_0 then
                           Front_End_Size :=
                             Make_Byte_Aligned_Size (The_Size_To_Use);
                        end if;
                     end if;
                  end;
               end if;
            else
               Report_Unhandled_Node_Empty
                 (The_Prefix,
                  "Do_Vanilla_Size",
                  "The entity is neither a type or an object");
            end if;
         end;
      elsif Nkind (The_Prefix) in N_Indexed_Component | N_Slice then
         declare
            The_Array  : constant Node_Id := Prefix (The_Prefix);
            Array_Type : constant Entity_Id := Etype (The_Array);
            Components : constant Entity_Id := Component_Type (Array_Type);
            Is_Implicitly_Packed : constant Boolean :=
              Opt.Implicit_Packing and then
              ASVAT.Size_Model.Has_Size_Rep_Clause (Array_Type);
         begin
            if Nkind (The_Prefix) = N_Indexed_Component then
               --  An indexed component cannot be a type reference.
               --  Use the ASVAT model size of the array components.
               if Has_Size (Components) then
                  The_Size := Computed_Size (Components);
               else
                  --  The size has not been computed try using the
                  --  front-end size.
                  Front_End_Size :=
                    (if not Is_Implicitly_Packed and then
                     Component_Size (Array_Type) > Uint_0
                     then
                        Component_Size (Array_Type)
                     else
                        RM_Size (Components));
               end if;
            else
               --  It's a slice
               if Has_Size (Prefix_Etype) then
                  The_Size := Computed_Size (Prefix_Etype);
               else
                  --  The size has not been computed try using the
                  --  front-end size.
                  Front_End_Size :=
                    (if not Is_Implicitly_Packed and then
                     Esize (Prefix_Etype) > Uint_0 then
                          Esize (Prefix_Etype)
                     else
                        Make_Byte_Aligned_Size (RM_Size (Prefix_Etype)));
               end if;
            end if;
         end;
      else
         --  The prefix is not an entity so use the ASVAT model
         --  size of the underlying type.
         if Has_Size (Prefix_Etype) then
            The_Size := Computed_Size (Prefix_Etype);
         else
            --  The size has not been computed try using the
            --  front-end size.
            Front_End_Size :=
              (if Esize (Prefix_Etype) /= 0 then
                  Esize (Prefix_Etype)
               else
                  Make_Byte_Aligned_Size (RM_Size (Prefix_Etype)));

         end if;
      end if;

      if Front_End_Size > 0 then
         --  The size has been obtained from the front-end.
         --  Use this value for the size.
         The_Size := Integer_Constant_To_Expr
           (Value           => Front_End_Size,
            Expr_Type       => Int32_T,
            Source_Location => Get_Source_Location (The_Prefix));
      elsif The_Size = Ireps.Empty then
                  Report_Unhandled_Node_Empty
              (The_Prefix,
                  "Do_Vanilla_Size",
                  "The size of the composite is not known " &
                    "by the front-end. Use a size " &
                    "representation clause on its declaration");
         The_Size := Get_Int32_T_Zero;
      end if;

      return The_Size;
   end Do_Vanilla_Size;

   --------------
   -- Has_Size --
   --------------

   function Has_Size (E : Entity_Id) return Boolean is
      (Has_Size (Intern (Unique_Name (E))));

   function Has_Size (Id : Symbol_Id) return Boolean is
      (Extra_Entity_Info.Contains (Id) and then
       Extra_Entity_Info (Id).Computed_Model_Size /= Ireps.Empty);

   ---------------------
   -- Has_Static_Size --
   ---------------------

   function Has_Static_Size (E : Entity_Id) return Boolean is
      (Has_Static_Size (Intern (Unique_Name (E))));

   function Has_Static_Size (Id : Symbol_Id) return Boolean is
     (Extra_Entity_Info.Contains (Id) and then
      Extra_Entity_Info (Id).Model_Size /= 0);

   -----------------------
   -- Set_Computed_Size --
   -----------------------

   procedure Set_Computed_Size (E : Entity_Id; Size_Expr : Irep) is
   begin
      Set_Computed_Size (Intern (Unique_Name (E)), Size_Expr);
   end Set_Computed_Size;

   procedure Set_Computed_Size (Id : Symbol_Id; Size_Expr : Irep) is
      E_In_Table : constant Boolean := Extra_Entity_Info.Contains (Id);
      E_Info     : Entity_Info :=
        (if E_In_Table then
              Extra_Entity_Info (Id)
         else
            Empty_Entity_Info);
   begin
      E_Info.Computed_Model_Size := Size_Expr;
      if E_In_Table then
         Extra_Entity_Info.Replace (Id, E_Info);
      else
         Extra_Entity_Info.Insert (Id, E_Info);
      end if;
   end Set_Computed_Size;

   -------------------
   -- Computed_Size --
   -------------------

   function Computed_Size (E : Entity_Id) return Irep is
   begin
      if Has_Size (E) then
         return
           (Extra_Entity_Info (Intern (Unique_Name (E))).Computed_Model_Size);
      else
         Report_Unhandled_Node_Empty
           (N        => E,
            Fun_Name => "Computed_Size",
            Message  => "Entity does not have ASVAT model size");
         return Integer_Constant_To_Expr
           (Value           => Esize (E),
            Expr_Type       => Make_Signedbv_Type (32),
            Source_Location => Get_Source_Location (E));
      end if;
   end Computed_Size;

   -----------------
   -- Static_Size --
   -----------------

   function Static_Size (E : Entity_Id) return Natural is
   begin
      if Has_Static_Size (E) then
         return
           (Extra_Entity_Info (Intern (Unique_Name (E))).Model_Size);
      else
         Report_Unhandled_Node_Empty
           (N        => E,
            Fun_Name => "Static_Size",
            Message  => "Entity does not have ASVAT static model size");
         return Natural (UI_To_Int (Esize (E)));
      end if;
   end Static_Size;

   ----------------------------
   -- Make_Byte_Aligned_Size --
   ----------------------------

   function Make_Byte_Aligned_Size (S : Uint) return Uint is
      --  Many objects sizes are rounded up to the nearest byte boundary
     (if S > Uint_0 then
         UI_Mul
        (UI_Add (UI_Div (UI_Sub (S, 1), Storage_Unit), 1), Storage_Unit)
      else
         Uint_0);

   function Make_Byte_Aligned_Size (S : Integer) return Integer is
      --  Many objects sizes are rounded up to the nearest byte boundary
     (if S > 0 then
         (((S - 1) / Storage_Unit) + 1) * Storage_Unit
      else
         0);

   function Make_Byte_Aligned_Size (S : Irep) return Irep is
      --  Many objects sizes are rounded up to the nearest byte boundary
      --  ((Bitsize - 1) / 8) + 1
      Byte_Size : constant Irep :=
        Make_Op_Add
          (Rhs             => Get_Int64_T_One,
           Lhs             => Make_Op_Div
             (Rhs               => Integer_Constant_To_Expr
                (Value           => Uint_8,
                 Expr_Type       => Int64_T,
                 Source_Location => Internal_Source_Location),
              Lhs               => Make_Op_Sub
                (Rhs             => Get_Int64_T_One,
                 Lhs             => Typecast_If_Necessary
                   (Expr           => S,
                    New_Type       => Int64_T,
                    A_Symbol_Table => Global_Symbol_Table),
                 Source_Location => Internal_Source_Location,
                 I_Type          => Int64_T),
              Source_Location   => Internal_Source_Location,
              I_Type            => Int64_T,
              Div_By_Zero_Check => False),
           Source_Location => Internal_Source_Location,
           I_Type          => Int64_T);
   begin
      return Byte_Size;
   end Make_Byte_Aligned_Size;

   ---------------------
   -- Set_Static_Size --
   ---------------------

   procedure Set_Static_Size (E : Entity_Id; Model_Size : Natural) is
   begin
      Set_Static_Size (Intern (Unique_Name (E)), Model_Size);
   end Set_Static_Size;

   procedure Set_Static_Size (Id : Symbol_Id; Model_Size : Natural) is
      E_In_Table : constant Boolean := Extra_Entity_Info.Contains (Id);
      E_Info     : Entity_Info :=
        (if E_In_Table then
              Extra_Entity_Info (Id)
         else
            Empty_Entity_Info);
   begin
      E_Info.Model_Size := Model_Size;
      E_Info.Computed_Model_Size :=
        Integer_Constant_To_Expr
          (Value           => UI_From_Int (Int (Model_Size)),
           Expr_Type       => Make_Signedbv_Type (32),
           Source_Location => Internal_Source_Location);
      if E_In_Table then
         Extra_Entity_Info.Replace (Id, E_Info);
      else
         Extra_Entity_Info.Insert (Id, E_Info);
      end if;
   end Set_Static_Size;

   procedure Set_Size_From_Entity (Target, Source : Entity_Id) is
   begin
      if Has_Static_Size (Source) then
         Set_Static_Size (Target, Static_Size (Source));
      else
         Set_Computed_Size (Target, Computed_Size (Source));
      end if;
   end Set_Size_From_Entity;

   procedure Accumumulate_Size (Is_Static     : in out Boolean;
                                Accum_Static  : in out Natural;
                                Accum_Dynamic : in out Irep;
                                Entity_To_Add :        Entity_Id)
   is
      Source_Location : constant Irep := Get_Source_Location (Accum_Dynamic);
      Accum_Type      : constant Irep := Get_Type (Accum_Dynamic);
   begin
      if Is_Static and then Has_Static_Size (Entity_To_Add) then
         Accum_Static  := Accum_Static + Static_Size (Entity_To_Add);
         Accum_Dynamic := Integer_Constant_To_Expr
           (Value           => UI_From_Int (Int (Accum_Static)),
            Expr_Type       => Accum_Type,
            Source_Location => Source_Location);
      else
         Is_Static := False;
         declare
            Entity_Size : constant Irep := Computed_Size (Entity_To_Add);
            RHS : constant Irep :=
              (if Get_Type (Entity_Size) /= Accum_Type then
                    Make_Op_Typecast
                 (Op0             => Entity_Size,
                  Source_Location => Source_Location,
                  I_Type          => Accum_Type)
               else
                  Entity_Size);
         begin
            Accum_Dynamic := Make_Op_Add
              (Rhs             => RHS,
               Lhs             => Accum_Dynamic,
               Source_Location => Source_Location,
               I_Type          => Accum_Type);
         end;
      end if;
   end Accumumulate_Size;

end ASVAT.Size_Model;
