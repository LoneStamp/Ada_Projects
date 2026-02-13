with Ada.Text_IO; use Ada.Text_IO;
with Ping;

procedure Main is

   Host_Input : String (1 .. 255);
   Last       : Natural;
   Count      : Positive := 4;

begin
   Put_Line ("==== Ada Terminal Ping Tool ====");
   New_Line;

   Put ("Enter host or IP: ");
   Get_Line (Host_Input, Last);

   Put ("Number of packets (default 4): ");
   declare
      Temp : String (1 .. 10);
      Len  : Natural;
   begin
      Get_Line (Temp, Len);
      if Len > 0 then
         Count := Positive'Value (Temp (1 .. Len));
      end if;
   exception
      when others =>
         Count := 4;
   end;

   New_Line;
   Ping.Send_Ping (Host_Input (1 .. Last), Count);

end Main;
