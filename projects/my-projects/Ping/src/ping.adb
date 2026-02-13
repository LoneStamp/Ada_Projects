with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Sockets; use GNAT.Sockets;

package body Ping is

  procedure Send_Ping (Host : String; Count : Positive := 4) is
     Socket : Socket_Type;
     Address : Sock_Addr_Type;
     Request : String (1 .. 64);
     Response : String (1 .. 64);
     Length : Integer;
     Success : Boolean;
  begin
     -- Create a raw
     Create_Socket (Socket, Family_Inet, Socket_Datagram);
     -- Resolve the hostname
     Address.Addr := Addresses (Get_Host_By_Name (Host), 1);
     Address.Port := 1;

     -- Send ICMP echo request
     for I in 1 .. Count loop
         Request := (others => ' ');
         Send_Socket (Socket, Request, Length, Address, Success);

         -- Recieve ICMP echo response
         Recieve_Socket (Socket, Response, Length, Address, Success);
         if Success then
             Put_Line("Recived response from " & Image (Address.Addr));
         else
            Put_Line("No response from " & Host);
         end if;
      delay 1.0;
   end loop;
   Close_Socket (Socket);
 end Send_Ping;
end Ping;
