with Uintp;               use Uintp;
with Nlists;              use Nlists;
with Einfo;               use Einfo;
with GOTO_Utils;          use GOTO_Utils;
with Tree_Walk;           use Tree_Walk;
with Follow;              use Follow;
with ASVAT.Size_Model;    use ASVAT.Size_Model;
package body Aggregates is

   -------------------------------
   -- Array_Dynamic_Named_Assoc --
   -------------------------------

   procedure Array_Dynamic_Named_Assoc (Block           : Irep;
                                        Low_Bound       : Irep;
                                        High_Bound      : Irep;
                                        N               : Node_Id;
                                        Target          : Irep;
                                        Comp_Type       : Irep)
   is
      Source_Location : constant Irep := Get_Source_Location (N);
      --  An aggregate with named association may have an others choice
      --  but it will be the last choice.
      Last_Choice_Node : constant Node_Id :=
        Last (Component_Associations (N));
      Has_Others_Choice : constant Boolean :=
        Nkind (First (Choices (Last_Choice_Node))) = N_Others_Choice;
      Others_Expr : constant Irep :=
        (if Has_Others_Choice and then
         Present (Expression (Last_Choice_Node)) then
            Typecast_If_Necessary
           (Expr           => Do_Expression (Expression (Last_Choice_Node)),
            New_Type       => Comp_Type,
            A_Symbol_Table => Global_Symbol_Table)
         else
            Ireps.Empty);

      Next_Comp_Assoc : Node_Id :=
        First (Component_Associations (N));

      --   We only handle 1 dimensional aggregates.
      Ada_Tick_First        : constant Node_Id :=
        Sinfo.Low_Bound (Get_Range (First_Index (Etype (N))));
      Ada_Tick_First_Irep   : constant Irep := Do_Expression (Ada_Tick_First);

   begin
      if Has_Others_Choice then
         if Others_Expr /= Ireps.Empty then
            --  It is complex to compute which elements are
            --  set by name association so, if there is an others =>
            --  choice which is not <>, all elements of
            --  the array are set to the others => expression.
            --  Those elements indexed by named association will be
            --  overwritten with the value associated with the index.
            --  Goto arrays are inexded from 0.
            declare
               First_Idx : constant Irep :=
                 Typecast_If_Necessary
                   (Expr           => Low_Bound,
                    New_Type       => Index_T,
                    A_Symbol_Table => Global_Symbol_Table);
               Last_Idx  : constant Irep :=
                 Typecast_If_Necessary
                   (Expr           => High_Bound,
                    New_Type       => Index_T,
                    A_Symbol_Table => Global_Symbol_Table);
            begin
               Assign_Value_To_Dynamic_Array_Components
                 (Block            => Block,
                  The_Array        => Target,
                  Zero_Based_First => First_Idx,
                  Zero_Based_Last  => Last_Idx,
                  Value_Expr       => Others_Expr,
                  I_Type           => Comp_Type,
                  Location         => Source_Location);
            end;
         else
            --  others => <>.
            --  For the moment there is nothing to be done.
            --  If the aspect Default_Component_Value is
            --  supported in the future and is applied to the array
            --  all of the elements of the array should first be
            --  initialised to the default value.
            null;
         end if;
      end if;
      --  Now assign the expressions for each choice list.
      --  Iterate through the component associations.
      while Present (Next_Comp_Assoc) loop
         --  Get the associated expression and iterate through the choices
         --  specifying this expression.
         declare
            Assoc_Expr : constant Irep :=
              Typecast_If_Necessary
                (Expr           =>
                   Do_Expression (Expression (Next_Comp_Assoc)),
                 New_Type       => Comp_Type,
                 A_Symbol_Table => Global_Symbol_Table);
            Next_Choice : Node_Id :=
              First (Choices (Next_Comp_Assoc));
         begin
            --  The others => choice has already been handled.
            while Present (Next_Choice) and then
              Nkind (Next_Choice) /= N_Others_Choice
            loop
               --  A choice may be a range of indices.
               if Nkind (Next_Choice) in
                 N_Range | N_Subtype_Indication or else
                 (Nkind (Next_Choice) = N_Identifier and then
                  Is_Type (Entity (Next_Choice)))
               then
                  declare
                     Bounds : constant Dimension_Bounds :=
                       Get_Bounds_From_Index (Next_Choice);
                  begin
                     Assign_Value_To_Dynamic_Array_Components
                       (Block            => Block,
                        The_Array        => Target,
                        Zero_Based_First =>
                          Make_Zero_Index
                            (Index    => Bounds.Low,
                             First    => Ada_Tick_First_Irep,
                             Location => Source_Location),
                        Zero_Based_Last =>
                          Make_Zero_Index
                            (Index    => Bounds.High,
                             First    => Ada_Tick_First_Irep,
                             Location => Source_Location),
                        Value_Expr      => Assoc_Expr,
                        I_Type          => Comp_Type,
                        Location        => Source_Location);
                  end;
               else
                  --  Assign expression to the indexed element.
                  Assign_To_Array_Component
                    (Block      => Block,
                     The_Array  => Target,
                     Zero_Index => Make_Zero_Index
                       (Index    => Do_Expression (Next_Choice),
                        First    => Ada_Tick_First_Irep,
                        Location => Source_Location),
                     Value_Expr => Assoc_Expr,
                     I_Type     => Comp_Type,
                     Location   => Source_Location);
               end if;
               --  Get the next choice.
               Next_Choice := Next (Next_Choice);
            end loop;
         end;
         --  Get the next component association.
         Next_Comp_Assoc := Next (Next_Comp_Assoc);
      end loop;
   end Array_Dynamic_Named_Assoc;

   ------------------------------
   -- Array_Dynamic_Positional --
   ------------------------------

   procedure Array_Dynamic_Positional (Block      : Irep;
                                       Low_Bound  : Irep;
                                       High_Bound : Irep;
                                       N          : Node_Id;
                                       Target     : Irep;
                                       Comp_Type  : Irep)
   is
      --  The aggregate has positional association and its bounds
      --  are not known by the front-end.  A goto loop has to be used to
      --  iterate through the expressions in the aggregate and assign them to
      --  the appropriate element of the array.
      Source_Location : constant Irep := Get_Source_Location (N);
      --  A positional associated aggregate may have an others =>
      --  as the last entry in the aggregate.
      Others_Present : constant Boolean :=
        Present (Component_Associations (N));
      Others_Expr : constant Irep :=
        (if Others_Present and then
         Present (Expression (First (Component_Associations (N))))
         then
            Typecast_If_Necessary
           (Expr           => Do_Expression
                (Expression (First (Component_Associations (N)))),
            New_Type       => Comp_Type,
            A_Symbol_Table => Global_Symbol_Table)
         else
         --  If others is followed by <> then all other
         --  elements of the array are unchanged.
            Ireps.Empty);

      Low_Bound_Index_T  : constant Irep :=
        Typecast_If_Necessary
          (Expr           => Low_Bound,
           New_Type       => Index_T,
           A_Symbol_Table => Global_Symbol_Table);

      High_Bound_Index_T : constant Irep :=
        Typecast_If_Necessary
          (Expr           => High_Bound,
           New_Type       => Index_T,
           A_Symbol_Table => Global_Symbol_Table);

      --  Iterator for expressions in the aggregate
      Next_Expr   : Node_Id := First (Expressions (N));

      --  All goto arrays are indexed from 0
      I : Int := 0;
   begin
      --  First do the positional arguments.
      while Present (Next_Expr) loop
         Assign_To_Array_Component
           (Block      => Block,
            The_Array  => Target,
            Zero_Index =>
              Make_Op_Add
                (Rhs             =>
                     Integer_Constant_To_Expr
                     (Value           => UI_From_Int (I),
                      Expr_Type       => Index_T,
                      Source_Location => Source_Location),
                 Lhs             => Low_Bound_Index_T,
                  Source_Location => Source_Location,
                 Overflow_Check  => False,
                 I_Type          => Index_T,
                 Range_Check     => False),
            Value_Expr => Typecast_If_Necessary
              (Expr           => Do_Expression (Next_Expr),
               New_Type       => Comp_Type,
               A_Symbol_Table => Global_Symbol_Table),
            I_Type     => Comp_Type,
            Location   => Source_Location);
         I := I + 1;
         Next_Expr := Next (Next_Expr);
      end loop;

      --  Now check for an others => expression.
      if Others_Present then
         if Others_Expr /= Ireps.Empty then
            --  Set the remaining elements (if any) to the others expression.
            declare
               First_Idx : constant Irep :=
                 Make_Op_Add
                   (Rhs             => Low_Bound_Index_T,
                    Lhs             =>
                      Integer_Constant_To_Expr
                        (Value           => UI_From_Int (I),
                         Expr_Type       => Index_T,
                         Source_Location => Source_Location),
                    Source_Location => Source_Location,
                    Overflow_Check  => False,
                    I_Type          => Index_T,
                    Range_Check     => False);

               Last_Idx  : constant Irep :=
                 Make_Zero_Index
                   (Index    => High_Bound_Index_T,
                    First    => Low_Bound_Index_T,
                    Location => Source_Location);
            begin
               Assign_Value_To_Dynamic_Array_Components
                 (Block            => Block,
                  The_Array        => Target,
                  Zero_Based_First => First_Idx,
                  Zero_Based_Last  => Last_Idx,
                  Value_Expr       => Others_Expr,
                  I_Type           => Comp_Type,
                  Location         => Source_Location);
            end;
         else
            --  others => <> present
            --  Currently nothing to be done but if default component
            --  values are supported in the future the remaining aggregate
            --  array elements should be set to the default value if it
            --  is specified.
            null;
         end if;
      end if;
   end Array_Dynamic_Positional;

   ------------------------------
   -- Array_Static_Named_Assoc --
   ------------------------------

   procedure Array_Static_Named_Assoc (Block      : Irep;
                                       Low_Bound  : Int;
                                       High_Bound : Int;
                                       N          : Node_Id;
                                       Target     : Irep;
                                       Comp_Type  : Irep)
   is
      Source_Location : constant Irep := Get_Source_Location (N);
      --  An aggregate with named association may have an others choice
      --  but it will be the last choice.
      Last_Choice_Node : constant Node_Id :=
        Last (Component_Associations (N));
      Has_Others_Choice : constant Boolean :=
        Nkind (First (Choices (Last_Choice_Node))) = N_Others_Choice;
      Others_Expr : constant Irep :=
        (if Has_Others_Choice and then
         Present (Expression (Last_Choice_Node)) then
            Typecast_If_Necessary
           (Expr           => Do_Expression (Expression (Last_Choice_Node)),
            New_Type       => Comp_Type,
            A_Symbol_Table => Global_Symbol_Table)
         else
            Ireps.Empty);

      Next_Comp_Assoc : Node_Id :=
        First (Component_Associations (N));

      --   We only handle 1 dimensional aggregates.
      Ada_Tick_First        : constant Node_Id :=
        Sinfo.Low_Bound (Get_Range (First_Index (Etype (N))));
      Ada_Tick_First_Static : constant Uint := Expr_Value (Ada_Tick_First);
      Ada_Tick_First_Irep   : constant Irep := Do_Expression (Ada_Tick_First);

   begin
      if Has_Others_Choice then
         if Others_Expr /= Ireps.Empty then
            --  It is complex to compute which elements are
            --  set by name association so, if there is an others =>
            --  choice which is not <>, all elements of
            --  the array are set to the others => expression.
            --  Those elements indexed by named association will be
            --  overwritten with the value associated with the index.
            --  Goto arrays are inexded from 0.
            Assign_Value_To_Static_Array_Components
              (Block            => Block,
               The_Array        => Target,
               Zero_Based_First => Low_Bound,
               Zero_Based_Last  => High_Bound,
               Value_Expr       => Others_Expr,
               I_Type           => Comp_Type,
               Location         => Source_Location);
         else
            --  others => <>.
            --  For the moment there is nothing to be done.
            --  If the aspect Default_Component_Value is
            --  supported in the future and is applied to the array
            --  all of the elements of the array should first be
            --  initialised to the default value.
            null;
         end if;
      end if;

      --  Now assign the expressions for each choice list.
      --  Iterate through the component associations.
      while Present (Next_Comp_Assoc) loop
         --  Get the associated expression and iterate through the choices
         --  specifying this expression.
         declare
            Assoc_Expr : constant Irep :=
              Typecast_If_Necessary
                (Expr           =>
                   Do_Expression (Expression (Next_Comp_Assoc)),
                 New_Type       => Comp_Type,
                 A_Symbol_Table => Global_Symbol_Table);
            Next_Choice : Node_Id :=
              First (Choices (Next_Comp_Assoc));
         begin
            --  The others => choice has already been handled.
            while Present (Next_Choice) and then
              Nkind (Next_Choice) /= N_Others_Choice
            loop
               --  A choice may be a range of indices.
               if Nkind (Next_Choice) in
                 N_Range | N_Subtype_Indication or else
                 (Nkind (Next_Choice) = N_Identifier and then
                  Is_Type (Entity (Next_Choice)))
               then
                  --  At the moment only static ranges are handled.
                  if Is_OK_Static_Range (Get_Range (Next_Choice))
                  then
                     declare
                        The_Range : constant Node_Id :=
                          Get_Range (Next_Choice);
                        Start : constant Int := UI_To_Int
                          (Expr_Value (Sinfo.Low_Bound (The_Range)) -
                           Ada_Tick_First_Static);
                        Finish : constant Int := UI_To_Int
                          (Expr_Value (Sinfo.High_Bound (The_Range)) -
                          Ada_Tick_First_Static);
                     begin
                        Assign_Value_To_Static_Array_Components
                          (Block            => Block,
                           The_Array        => Target,
                           Zero_Based_First => Start - Low_Bound,
                           Zero_Based_Last  => Finish - Low_Bound,
                           Value_Expr       => Assoc_Expr,
                           I_Type           => Comp_Type,
                           Location         => Source_Location);
                     end;
                  else
                     declare
                        Bounds : constant Dimension_Bounds :=
                          Get_Bounds_From_Index (Next_Choice);
                     begin
                        Assign_Value_To_Dynamic_Array_Components
                          (Block            => Block,
                           The_Array        => Target,
                           Zero_Based_First =>
                             Make_Zero_Index
                               (Index    => Bounds.Low,
                                First    => Ada_Tick_First_Irep,
                                Location => Source_Location),
                           Zero_Based_Last =>
                             Make_Zero_Index
                               (Index    => Bounds.High,
                                First    => Ada_Tick_First_Irep,
                                Location => Source_Location),
                          Value_Expr      => Assoc_Expr,
                           I_Type          => Comp_Type,
                           Location        => Source_Location);
                     end;
                  end if;
               else
                  --  Assign expression to the indexed element.
                  Assign_To_Array_Component
                    (Block      => Block,
                     The_Array  => Target,
                     Zero_Index => Make_Zero_Index
                       (Index    => Typecast_If_Necessary
                            (Expr           => Do_Expression (Next_Choice),
                             New_Type       => Index_T,
                             A_Symbol_Table => Global_Symbol_Table),
                        First    => Ada_Tick_First_Irep,
                        Location => Source_Location),
                     Value_Expr => Assoc_Expr,
                     I_Type     => Comp_Type,
                     Location   => Source_Location);
               end if;
               --  Get the next choice.
               Next_Choice := Next (Next_Choice);
            end loop;
         end;
         --  Get the next component association.
         Next_Comp_Assoc := Next (Next_Comp_Assoc);
      end loop;
   end Array_Static_Named_Assoc;

   -----------------------------
   -- Array_Static_Positional --
   -----------------------------

   procedure Array_Static_Positional (Block      : Irep;
                                      Low_Bound  : Int;
                                      High_Bound : Int;
                                      N          : Node_Id;
                                      Target     : Irep;
                                      Comp_Type  : Irep)
   is
      --  The aggregate has positional association and its bounds
      --  are known by the front-end.  A simple Ada loop can iterate
      --  through the expressions in the aggregate and assign them to
      --  the appropriate element of the target array.
      Source_Location : constant Irep := Get_Source_Location (N);
      --  A positional associated aggregate may have an others =>
      --  as the last entry in the aggregate.
      Others_Expr : constant Irep :=
        (if Present (Component_Associations (N)) and then
         Present (Expression (First (Component_Associations (N))))
         then
            Typecast_If_Necessary
           (Expr           => Do_Expression
                (Expression (First (Component_Associations (N)))),
            New_Type       => Comp_Type,
            A_Symbol_Table => Global_Symbol_Table)
         else
         --  If others is followed by <> then all other
         --  elements of the array are unchanged.
            Ireps.Empty);

      --  Iterator for expressions in the aggregate
      Next_Expr   : Node_Id := First (Expressions (N));
   begin
      for I in Low_Bound .. High_Bound loop
         --  If the list of expressions becomes exhuasted before the
         --  the loop index reaches the value of the zero-based last index
         --  there must be an others => as the last entry in the aggregate.
         if not Present (Next_Expr) then
            --  if the others=> expression is non empty,
            --  fill the remainder of the array with the others expression.
            if Others_Expr /= Ireps.Empty then
               Assign_Value_To_Static_Array_Components
                 (Block            => Block,
                  The_Array        => Target,
                  Zero_Based_First => I,
                  Zero_Based_Last  => High_Bound,
                  Value_Expr       => Others_Expr,
                  I_Type           => Comp_Type,
                  Location         => Source_Location);
            else
               --  If the others expression is Ireps.Empty an
               --  others <> has been reached in the aggregate list.
               --  There is no more to be done!
               --  This will change if default values for array
               --  elements are implemented.  The remaining
               --  elements would be set to the default value.
               --  Currently they could be set to nondet but that
               --  is probably not necessary.
               null;
            end if;

            exit;
         else
            --  Otherwise assign the next expression to the i'th
            --  element of the aggregate array object.
            Assign_To_Array_Component
              (Block      => Block,
               The_Array  => Target,
               Zero_Index =>
                 Integer_Constant_To_Expr
                   (Value           => UI_From_Int (I),
                    Expr_Type       => Index_T,
                    Source_Location => Source_Location),
               Value_Expr => Typecast_If_Necessary
                 (Expr           => Do_Expression (Next_Expr),
                  New_Type       => Comp_Type,
                  A_Symbol_Table => Global_Symbol_Table),
               I_Type     => Comp_Type,
               Location   => Source_Location);

            Next_Expr := Next (Next_Expr);
         end if;
      end loop;
   end Array_Static_Positional;

   procedure Update_Array_From_Aggregate
           (Block        : Irep;
            Agg          : Node_Id;
            N_Dimensions : Pos;
            Dest_Bounds  : Static_And_Dynamic_Bounds;
            Dest_Array   : Irep)
   is
      Positional_Assoc : constant Boolean := Present (Expressions (Agg));
      Aggregate_Subtype : constant Entity_Id := Etype (Agg);
      Component_Pre     : constant Irep :=
        Do_Type_Reference (Component_Type (Aggregate_Subtype));
      Component_Irep    : constant Irep :=
        (if Kind (Follow_Symbol_Type (Component_Pre,
         Global_Symbol_Table)) = I_C_Enum_Type
         then
            Make_Unsignedbv_Type
           (ASVAT.Size_Model.Static_Size (Component_Type (Aggregate_Subtype)))
         else
            Component_Pre);
   begin
      if N_Dimensions > 1 then
         Report_Unhandled_Node_Empty
           (Agg,
            "Update_Array_From_Aggregate",
            "Multi-dimensional aggregates unsupported");
      elsif not (Is_Scalar_Type (Component_Type (Aggregate_Subtype)) or else
                 Is_Access_Type (Component_Type (Aggregate_Subtype)))
      then
         Report_Unhandled_Node_Empty
           (Agg,
            "Update_Array_From_Aggregate",
            "Aggregates with non-scalar components are unsupported");

      elsif Dest_Bounds.Has_Static_Bounds and
        All_Dimensions_Static (Aggregate_Subtype)
      then
         --  The target array and
         --  The aggregate have static bounds.
         if Positional_Assoc then
            Array_Static_Positional
              (Block      => Block,
               Low_Bound  => Dest_Bounds.Low_Static,
               High_Bound => Dest_Bounds.High_Static,
               N          => Agg,
               Target     => Dest_Array,
               Comp_Type  => Component_Irep);
         elsif Present (Component_Associations (Agg)) then
            --  Named associations.
            Array_Static_Named_Assoc
              (Block      => Block,
               Low_Bound  => Dest_Bounds.Low_Static,
               High_Bound => Dest_Bounds.High_Static,
               N          => Agg,
               Target     => Dest_Array,
               Comp_Type  => Component_Irep);
         else
            Report_Unhandled_Node_Empty
              (N        => Agg,
               Fun_Name => "Initialse_Array_From_Aggregate",
               Message  =>
                 "Aggregate has neither Positional or Named arguments");
         end if;
      else
         if Positional_Assoc then
            Array_Dynamic_Positional
              (Block      => Block,
               Low_Bound  => Dest_Bounds.Low_Dynamic,
               High_Bound => Dest_Bounds.High_Dynamic,
               N          => Agg,
               Target     => Dest_Array,
               Comp_Type  => Component_Irep);
         else
            Array_Dynamic_Named_Assoc
              (Block      => Block,
               Low_Bound  => Dest_Bounds.Low_Dynamic,
               High_Bound => Dest_Bounds.High_Dynamic,
               N          => Agg,
               Target     => Dest_Array,
               Comp_Type  => Component_Irep);
         end if;
      end if;
   end Update_Array_From_Aggregate;

end Aggregates;
