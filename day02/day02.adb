-- adbr 2021-12-02

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;

procedure Day02 is
   
   type Command_Type is (Forward, Down, Up);
   
   procedure Get_Positions (File_Name  : String;
                            Horizontal : out Integer;
                            Depth      : out Integer) is
      
      package Command_IO is new Enumeration_IO (Command_Type);
      use Command_IO;
      
      File    : File_Type;
      Command : Command_Type;
      Value   : Integer;
   begin
      Horizontal := 0;
      Depth      := 0;
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         Get (File, Command);
         Get (File, Value);
         case Command is
            when Forward =>
               Horizontal := Horizontal + Value;
            when Down =>
               Depth := Depth + Value;
            when Up =>
               Depth := Depth - Value;
         end case;
      end loop;
      Close (File);
   end Get_Positions;
   
   procedure Get_Positions_Aim (File_Name  : String;
                                Horizontal : out Integer;
                                Depth      : out Integer) is
      
      package Command_IO is new Enumeration_IO (Command_Type);
      use Command_IO;
      
      File    : File_Type;
      Command : Command_Type;
      Value   : Integer;
      Aim     : Integer;
   begin
      Horizontal := 0;
      Depth      := 0;
      Aim        := 0;
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         Get (File, Command);
         Get (File, Value);
         case Command is
            when Forward =>
               Horizontal := Horizontal + Value;
               Depth := Depth + (Aim * Value);
            when Down =>
               Aim := Aim + Value;
            when Up =>
               Aim := Aim - Value;
         end case;
      end loop;
      Close (File);
   end Get_Positions_Aim;
   
   Horizontal, Depth : Integer;
begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day02 <filename>");
      Set_Exit_Status (Failure);
      return;
   end if;
   
   Get_Positions (Argument (1), Horizontal, Depth);
   Put_Line ("Part 1:");
   Put_Line ("  Horizontal position:" & Integer'Image (Horizontal));
   Put_Line ("  Depth position:" & Integer'Image (Depth));
   Put_Line ("  Multiplication:" & Integer'Image (Horizontal * Depth));
   
   Get_Positions_Aim (Argument (1), Horizontal, Depth);
   Put_Line ("Part 2:");
   Put_Line ("  Horizontal position:" & Integer'Image (Horizontal));
   Put_Line ("  Depth position:" & Integer'Image (Depth));
   Put_Line ("  Multiplication:" & Integer'Image (Horizontal * Depth));
end Day02;
