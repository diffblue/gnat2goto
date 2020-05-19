with Opt;
with Stand;
with Snames;                  use Snames;
with Atree;                   use Atree;
with Sinfo;                   use Sinfo;
with Einfo;                   use Einfo;
with Sem_Util;                use Sem_Util;
with Sem_Aux;                 use Sem_Aux;
with Tree_Walk;               use Tree_Walk;
with GOTO_Utils;              use GOTO_Utils;
with System;                  use System;

package body ASVAT.Size_Model is

   function Make_Byte_Aligned_Size (S : Uint) return Uint is
      --  Many objects sizes are rounded up to the nearest byte boundary
     (if S > Uint_0 then
         UI_Mul
        (UI_Add (UI_Div (UI_Sub (S, 1), Storage_Unit), 1), Storage_Unit)
      else
         Uint_0);

   procedure Try_Base_Type_Size (N : Node_Id;
                                 E : Entity_Id; The_Size : in out Uint)
   with Pre => Is_Type (E);
   --  If the front-end is unable to provide a value for attribute_size,
   --  try its base-type.  My be the front-end has a attribute_size value
   --  for the base-type.

   function Do_Vanilla_Size (The_Prefix : Node_Id) return Uint;

   function Do_VADS_Size (The_Prefix : Node_Id) return Uint;

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
      --  or other or both RM_Size and Esize, so Do_Attribute_Size tries to
      --  return a sensible value or generates an unhandled report.

      Constant_Type : constant Irep :=
        Do_Type_Reference (Stand.Universal_Integer);
      The_Prefix    : constant Node_Id := Prefix (N);
      Prefix_Etype  : constant Entity_Id := Etype (The_Prefix);

      Use_VADS_Size : constant Boolean :=
        Opt.Use_VADS_Size or else
        Get_Attribute_Id (Attribute_Name (N)) = Attribute_VADS_Size;

      --  The size that is to be returned by the attribute_size.
      The_Size      : Uint;

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
         The_Size := RM_Size (Prefix_Etype);

      elsif Nkind (The_Prefix) in N_Has_Entity and then
      --  First check if a size representation clause has been applied
      --  directly to this entity
        ASVAT.Size_Model.Has_Size_Rep_Clause (Entity (The_Prefix))
      then
         The_Size := ASVAT.Size_Model.Get_Rep_Size (Entity (The_Prefix));
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
      return Make_Constant_Expr
        (Value =>
           UI_Image (Input  => The_Size,
                     Format => Decimal),
         I_Type => Constant_Type,
         Source_Location => Get_Source_Location (N));
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
   begin
      if not Is_Base_Type (E) then
         --  If the type of the enitiy is a subtype, check whether the
         --  size of its base type is known.
         if Base_Type_Size /= Uint_0 then
            The_Size := Base_Type_Size;
         elsif ASVAT.Size_Model.Has_Size_Rep_Clause (Base_T) then
            The_Size := ASVAT.Size_Model.Get_Rep_Size (Base_T);
         else
            Report_Unhandled_Node_Empty
              (N,
               "Do_Attribute_Size",
               "The size of the composite subtype is " &
                 "not known by the front-end. Use a size " &
                 "representation clause on the base type " &
                 "or a Value_Size clause on the subtype " &
                 "declaration");
         end if;
      else
         Report_Unhandled_Node_Empty
           (N,
            "Do_Attribute_Size",
            "The size of the composite is not known " &
              "by the front-end. Use a size " &
              "representation clause on its declaration");
      end if;
   end Try_Base_Type_Size;

   -------------------
   -- Do_VADS_Size  --
   -------------------

   function Do_VADS_Size (The_Prefix : Node_Id) return Uint is
      Prefix_Etype  : constant Entity_Id := Etype (The_Prefix);
      The_Size : Uint;
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

         The_Size := RM_Size (Prefix_Etype);

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
               The_Size := RM_Size (Prefix_Etype);
            else
               The_Size := Esize (Prefix_Etype);
            end if;
         end;
      else
         The_Size := RM_Size (Prefix_Etype);
      end if;

      if The_Size = Uint_0 then
         --  The front-end does not give the size of the entity.
         if Nkind (The_Prefix) in N_Has_Entity and then
         --  First check if it is an object and the object size can be
         --  used.
           Is_Object (Entity (The_Prefix)) and then
           Esize (Entity (The_Prefix)) /= Uint_0
         then
            The_Size := Esize (Entity (The_Prefix));
         else
            Try_Base_Type_Size (The_Prefix, Prefix_Etype, The_Size);
         end if;
      end if;

      return The_Size;
   end Do_VADS_Size;

   ----------------------
   -- Do_Vanilla_Size  --
   ----------------------

   function Do_Vanilla_Size (The_Prefix : Node_Id) return Uint is
      Prefix_Etype  : constant Entity_Id := Etype (The_Prefix);
      The_Size : Uint;
   begin
      if Nkind (The_Prefix) in
        N_Has_Entity | N_Selected_Component
      then
         declare
            The_Entity : constant Entity_Id :=
              (if Nkind (The_Prefix) =
                   N_Selected_Component
               then
                  Entity (Selector_Name (The_Prefix))
               else
                  Entity (The_Prefix));
         begin
            if Is_Object (The_Entity) then
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
                  The_Size := The_Size_To_Use;

                  if The_Size <= Uint_0 then
                     Try_Base_Type_Size (The_Prefix,
                                         Etype (The_Entity), The_Size);
                     if The_Size > Uint_0 then
                        The_Size :=
                          Make_Byte_Aligned_Size (The_Size_To_Use);
                     end if;
                  end if;
               end;

            elsif Is_Type (The_Entity) then
               --  Since the attribute is applied to
               --  a subtype,
               --  S'Size, RM_Size should be used.
               The_Size := RM_Size (Entity (The_Prefix));

               if The_Size <= Uint_0 then
                  Try_Base_Type_Size (The_Prefix,
                                      Etype (The_Entity), The_Size);
               end if;
            else
               The_Size := Uint_0;
               Report_Unhandled_Node_Empty
                 (The_Prefix,
                  "Do_Attribute_Size",
                  "Size attribute applied to an entity " &
                    "which is not a (sub)type or an object");
            end if;
         end;

      elsif Nkind (The_Prefix) in
        N_Indexed_Component | N_Slice
      then
         declare
            Is_Indexed_Component : constant Boolean :=
              Nkind (The_Prefix) = N_Indexed_Component;
            The_Array : constant Node_Id := Prefix (The_Prefix);
            Array_Type : constant Entity_Id := Etype (The_Array);
            Is_Implicitly_Packed : constant Boolean :=
              Opt.Implicit_Packing and then
              ASVAT.Size_Model.Has_Size_Rep_Clause (Array_Type);

            Obj_Size : constant Uint :=
              (if Is_Indexed_Component then
                  (if not Is_Implicitly_Packed then
                     Component_Size (Array_Type)
                  else
                     RM_Size (Prefix_Etype))
               else
                  (if not Is_Implicitly_Packed then
                       Esize (Prefix_Etype)
                  else
                     Make_Byte_Aligned_Size (RM_Size (Prefix_Etype))));

            Type_Size : constant Uint :=
              (if Is_Indexed_Component and not Is_Implicitly_Packed then
                       Component_Size (Array_Type)
               else
                  RM_Size (Prefix_Etype));

            The_Size_To_Use : constant Uint :=
              (if Obj_Size > Uint_0 then
                  Obj_Size
               else
                  Make_Byte_Aligned_Size (Type_Size));
         begin
            The_Size := The_Size_To_Use;
            if The_Size_To_Use <= Uint_0 then
               Report_Unhandled_Node_Empty
                 (N        => The_Prefix,
                  Fun_Name => "Do_Attribute_Size",
                  Message  => "The Attribute_Size returns a value <= 0");
            end if;
         end;
      else
         --  Return the default size of the object
         The_Size := Esize (Prefix_Etype);

         if The_Size <= Uint_0 then
            Try_Base_Type_Size (The_Prefix, Prefix_Etype, The_Size);
            if The_Size > Uint_0 then
               The_Size :=
                 Make_Byte_Aligned_Size (The_Size);
            end if;
         end if;
      end if;

      return The_Size;
   end Do_Vanilla_Size;

end ASVAT.Size_Model;
