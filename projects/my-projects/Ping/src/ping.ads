with GNAT.Sockets; use GNAT.Sockets;
 package Ping is
   
 procedure Send_Ping(Host : String; Count : Positive := 4);

   
 end Ping;