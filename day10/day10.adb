-- adbr 2021-12-17

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Containers.Vectors; use Ada.Containers;

procedure Day10 is
   
   type Token_Type is ('(', '[', '{', '<', ')', ']', '}', '>');
   subtype Open_Token  is Token_Type range '(' .. '<';
   subtype Close_Token is Token_Type range ')' .. '>';
   
   package Token_Stack is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Token_Type);
   
   function To_Token (C : Character) return Token_Type is
   begin
      return Token_Type'Value (Character'Image (C));
   end To_Token;
   
   function To_Character (T : Token_Type) return Character is
   begin
      return Character'Value (Token_Type'Image (T));
   end To_Character;
   
   procedure Check_Line (Line : String;
                         Error : out Boolean;
                         Token : out Token_Type)
   is
      To_Close : array (Open_Token) of Close_Token :=
        ('(' => ')', '[' => ']', '{' => '}', '<' => '>');
      
      To_Open : array (Close_Token) of Open_Token :=
        (')' => '(', ']' => '[', '}' => '{', '>' => '<');
        
      T     : Token_Type;
      Stack : Token_Stack.Vector;
   begin
      for I in Line'Range loop
         T := To_Token (Line (I));
         case T is
            when Open_Token =>
               Stack.Append (T);
            when Close_Token =>
               if Stack.Last_Element = To_Open (T) then
                  Stack.Delete_Last;
               else
                  Error := True;
                  Token := T;
                  return;
               end if;
         end case;
      end loop;
      Error := False;
   end Check_Line;

   procedure Part_1 (File_Name : String) is
      Result : Natural := 0;
      File : File_Type;
      Error_Value : array (Close_Token) of Positive :=
        (')' => 3, ']' => 57, '}' => 1197, '>' => 25137);
   begin
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         declare
            Line  : String := Get_Line (File);
            Error : Boolean;
            Token : Token_Type;
         begin
            Check_Line (Line, Error, Token);
            if Error then
               Result := Result + Error_Value (Token);
            end if;
         end;
      end loop;
      Close (File);
      
      Put_Line ("Part 1:");
      Put_Line ("  Syntax error score:" & Result'Img);
   end Part_1;

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day10 <inputfile>");
      return;
   end if;
   Part_1 (Argument (1));
end Day10;
