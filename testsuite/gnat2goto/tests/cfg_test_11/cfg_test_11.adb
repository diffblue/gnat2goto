procedure CFG_Test_11 (X : in Integer; Y : in out Integer) is
   I : Integer;
   J : Integer;
   K : Integer;
begin
   Y := 0;

   I := 0;
   Outer :
   while I <= X loop

      J := 0;
      Middle :
      while J <= X loop

         K := 0;
         Inner :
         while K <= X loop

            exit Inner when I - J - K = 17;

            Y := Y + I - J + K;

            exit Middle when I + J - K = 23;

            K := K + 1;

            exit Outer when I + J + K = 42;
         end loop Inner;

         exit Outer when J - X - I = 19;

         J := J + 1;
      end loop Middle;

      I := I + 1;
   end loop Outer;

   pragma Assert (I /= 0);
end CFG_Test_11;
