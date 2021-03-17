--  ASVAT Modelling
--  This package implements the features for modelling interfaces to
--  subsystems which are external to the system subject to ASVAT analysis.
--  There are currently 2 sorts of model:
--  Nondet_Function and In_Type_Function.
--  Other sorts of model are not yet implemented, for example,
--  Nondet_Vars, Nondet_In_Type_Vars and Represents.
--
--  In_Type_Function is a model which returns Boolean True if the scalar object
--  passed as a parameter is in the range of values specified for its subtype.
--  The use of a model rather than implementing the test in Ada ensures that
--  the test is not removed by optimisation.
--
--  Nondet_Function is a model is a parameterless function which returns an
--  ASVAT non-deterministic value of the base type of the return subtype.
--  This means that the value returned may not be in the range specified for
--  the return subtype.
--
--  Nondet_Vars is modelled by a subprogram.
--  A subprogram which is annotated as Nondet_Vars sets all of the scalar
--  components of its out, in out and result objects to a non-deterministic
--  value as described for a Nondet_Function return.
--  If the subprogram has outputs which are global variables these must be
--  specified using the SPARK 2014 Global aspect or Global pragma as either
--  mode In_Out or Out.
--  If the subprogram has no global outputs then it should have Global
--  aspect or pragma with Null or only Mode In variables.
--
--  Nondet_In_Type_Vars model is as similar to Nondet_Vars except that the
--  non-deterministic values of scalar components are within theis subtype.
--
--  Represents is used to model non-visible varaiables (and their type if this
--  is also not visible).
--
--  The ASVAT models are associated with Ada subprograms (or objects, in
--  the case of Represents) by an Annotate aspect specifications.
--
--  Generally modelling subprograms will not have a body as they represent
--  features of the system that are not being directly analysed by ASVAT,
--  this is the very reason that a model of their behaviour is requred.
--  When an ASVAT model is associated with an entity by an Annotation
--  aspect specification then usually an Import => True will be included in
--  the aspect specification.  If Import => True is not in the
--  aspect specification the subprogram, Ada rules require the subprogram
--  to have a body but it will be ignored by ASVAT.
--
--  Associating Represents model with a declaration of a local variable
--  indicates that the variable represents a more global variable that is
--  not visible in the context of the local declaration.
--  Furthermore, if the type of the non-visible variable a
--  local type declaration can be used to representthis too.
--
--  The local declaration of the variable (and its type, if the type of
--  the non-visible variable is also not visible) must exactly match the
--  declaration of the non-visible variable (and its type if necessary).
--  The purpose of these declarations is to represent global variables of
--  the subprogram which are indirectly updated by the subprogram but not
--  directly visible to it.
--
--  The ASVAT models are associated with an entity by an ASVAT annotation.
--
--  Examples:
--  type My_Int is range 1 .. 100;
--
--  X : Integer;  --  A normal, visible variable.
--
--  I : Integer   --  Represents a variable that is not visible.
--  with Annotate => (ASVAT, Represents, "Hidden_Vars.I");
--
--  J : My_Int    --  Represents a non-visible variable whose type is also
--                -- not visible.
--  with Annotate => (ASVAT, Represents,
--                           "Hidden_Vars.J",      --  The non-visible variable
--                           "Hidden_Type.My_Int"  --  Its non-visible type
--                   );
--
--  The name of a subprogram associated with an ASVAT model can be any legal
--  Ada subprogram name except operator names.
--
--  function In_Type (N : My_Int) return Boolean
--  with Annotate => (ASVAT, In_Type_Function),
--       Import   => True;
--
--  function Nondet_Integer return Integer
--  with Annotate => (ASVAT, Nondet_Function),
--       Import   => True;
--
--  procedure Update
--  with Global   => (In_Out => (I, J, X)),
--       Annotate => (ASVAT, Nondet_Vars,
--       Import   => True;
--
--  procedure Read (I : out Integer)
--  with Global   => null,
--       Annotate => (ASVAT, Nondet_In_Type_Vars),
--       Import   => True;
--
--  Current limitations:
--  Only Annotate aspect specifications are supported.
--  Pragma Annotate is not currently supported.

with Atree;                   use Atree;
with Sinfo;                   use Sinfo;

package ASVAT.Modelling is
   type Model_Sorts is
     (Not_A_Model, Nondet_Function, In_Type_Function,
      Nondet_Vars, Nondet_In_Type_Vars, Represents);
   subtype Valid_Model is Model_Sorts range
     Nondet_Function .. Model_Sorts'Last;

--     function Get_Annotation_Name (N : Node_Id) return String
--     with Pre => Nkind (N) = N_Pragma and then
--                 Get_Pragma_Id (N) = Pragma_Annotate;

   function Get_Model_From_Anno (N : Node_Id) return Model_Sorts
   with Pre => Nkind (N) = N_Aspect_Specification;

   function Get_Model_Sort (E : Entity_Id) return Model_Sorts;

   function Is_Model (Model : Model_Sorts) return Boolean is
     (Model /= Not_A_Model);

   procedure Make_Model (E : Entity_Id; Model : Model_Sorts);

private
   Print_Message : constant Boolean := False;

   procedure Print_Modelling_Message (Mess : String; Loc : Source_Ptr);

end ASVAT.Modelling;
