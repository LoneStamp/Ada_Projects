with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Prime_Two is
   Number : Positive;

   -- This functions return True if N is prime; False otherwisw --
function Is_Prime(N : Positive) return Boolean is
begin
      for I in 2 .. (N + 1) loop
         if N mod I = 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Prime;

begin
   Put("Enter an positive integer: ");
   Get(Number);
   if Number < 2 then
      Put("The value "); Put(Number, 0); Put_Line("is bad");
   else
      Put("The value "); Put(Number, 0);
      if Is_Prime(Number) then
         Put_Line(" is prime.");
      else
         Put_Line(" is not prime.");
      end if;
   end if;

   null;
end Prime_Two;
