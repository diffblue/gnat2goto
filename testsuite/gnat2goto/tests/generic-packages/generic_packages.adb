procedure Generic_Packages is

  generic
    type T is private;
  package Generic_Store is
    procedure Store (Val : T);
    function Retrieve return T;
  end Generic_Store;

  package body Generic_Store is
    Storage : T;
    procedure Store (Val : T) is
    begin
      Storage := Val;
    end Store;

    function Retrieve return T is (Storage);
  end Generic_Store;

  package Int_Store is new Generic_Store (Integer);
  package Float_Store is new Generic_Store (Float);

  pragma Warnings (Off, "is read but never assigned");
  A, B : Integer;
  F1, F2 : Float;
begin
  Int_Store.Store (A);
  pragma Assert (Int_Store.Retrieve = A);
  Int_Store.Store (B);
  pragma Assert (Int_Store.Retrieve = B);

  Float_Store.Store (F1);
  pragma Assert (Float_Store.Retrieve = F1);
  Float_Store.Store (F2);
  pragma Assert (Float_Store.Retrieve = F2);
  pragma Assert (False, "Reachability check");
end Generic_Packages;
