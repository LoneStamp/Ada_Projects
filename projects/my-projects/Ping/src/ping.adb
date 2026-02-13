with Ada.Text_IO;   use Ada.Text_IO;
with GNAT.OS_Lib;   use GNAT.OS_Lib;

package body Ping is

   procedure Send_Ping (Host : String; Count : Positive := 4) is

      Args    : Argument_List (1 .. 3);
      Success : Boolean;

   begin
      Args (1) := new String'("-n");  -- Windows flag
      Args (2) := new String'(Positive'Image (Count));
      Args (3) := new String'(Host);

      Put_Line ("--------------------------------------");
      Put_Line ("Pinging " & Host & "...");
      Put_Line ("--------------------------------------");

      Spawn
        (Program_Name => "ping",
         Args         => Args,
         Success      => Success);

      if not Success then
         Put_Line ("Ping failed.");
      end if;

      -- Free memory
      for I in Args'Range loop
         Free (Args (I));
      end loop;

   end Send_Ping;

end Ping;
