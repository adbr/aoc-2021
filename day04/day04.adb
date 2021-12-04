-- adbr 2021-12-04

--  Part 1:
--    Score of winning board: 65325

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;

procedure Day04 is
   
   type Board_Index is range 1 .. 5;
   type Number is new Natural;
   type Board_Entry is
      record
         Num  : Number;
         Mark : Boolean := False;
      end record;
   type Board_Type is array (Board_Index, Board_Index) of Board_Entry;
   
   package Number_IO is new Ada.Text_IO.Integer_IO (Number);
   use Number_IO;
   
   package Number_Vector is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Number);
   
   package Board_Vector is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Board_Type);
   
   function Board_Entry_Image (E : Board_Entry) return String is
   begin
      return E.Num'Img & (if E.Mark then "*" else " ");
   end Board_Entry_Image;
   
   procedure Print_Board (B : Board_Type) is
   begin
      Put_Line ("[");
      for I in B'Range (1) loop
         for J in B'Range (2) loop
            Put (Tail (Board_Entry_Image (B (I, J)), 4));
         end loop;
         New_Line;
      end loop;
      Put_Line ("]");
   end Print_Board;
      
   function Score (File_Name : String) return Integer is
      
      procedure Read_Data (File_Name : String;
                           Numbers : out Number_Vector.Vector;
                           Boards  : out Board_Vector.Vector) is
         File : File_Type;
      begin
         Open (File, In_File, File_Name);
         
         -- read numbers
         declare
            C   : Character;
            Eol : Boolean;
            Num : Number;
         begin
            while not End_Of_Line (File) loop
               Look_Ahead (File, C, Eol);
               if C = ',' then
                  Get (File, C); -- skip delimiter
               end if;
               Get (File, Num);
               Numbers.Append (Num);
            end loop;
         end;
         
         -- read boards
         declare
            Num   : Number;
            Board : Board_Type;
         begin
            while not End_Of_File (File) loop
               for I in Board_Index loop
                  for J in Board_Index loop
                     Get (File, Num);
                     Board (I, J) := Board_Entry'(Num => Num, Mark => False);
                  end loop;
               end loop;
               Boards.Append (Board);
            end loop;
         end;
         
         Close (File);
      end Read_Data;
      
      procedure Mark_Number (Boards : in out Board_Vector.Vector;
                             Num : Number) is
      begin
         for B of Boards loop
            for I in B'Range (1) loop
               for J in B'Range (2) loop
                  if B (I, J).Num = Num then
                     B (I, J).Mark := True;
                  end if;
               end loop;
            end loop;
         end loop;
      end Mark_Number;
      
      procedure Search_Winner (Boards    : in Board_Vector.Vector;
                              Winner    : out Board_Type;
                              Is_Winner : out Boolean) is
      begin
         for B of Boards loop
            -- check rows
            for I in B'Range (1) loop
               declare
                  Mark : Boolean := True;
               begin
                  for J in B'Range (2) loop
                     Mark := Mark and B(I, J).Mark;
                  end loop;
                  if Mark then
                     Is_Winner := True;
                     Winner    := B;
                     return;
                  end if;
               end;
            end loop;
            
            -- check columns
            for I in B'Range (1) loop
               declare
                  Mark : Boolean := True;
               begin
                  for J in B'Range (2) loop
                     Mark := Mark and B(J, I).Mark;
                  end loop;
                  if Mark then
                     Is_Winner := True;
                     Winner    := B;
                     return;
                  end if;
               end;
            end loop;
         end loop;
         Is_Winner := False;
      end Search_Winner;
      
      Numbers : Number_Vector.Vector;
      Boards  : Board_Vector.Vector;
   begin
      Read_Data (File_Name, Numbers, Boards);
      for Num of Numbers loop
         Mark_Number (Boards, Num);
         declare
            Winner    : Board_Type;
            Is_Winner : Boolean;
            Score     : Integer := 0;
         begin
            Search_Winner (Boards, Winner, Is_Winner);
            if Is_Winner then
               for E of Winner loop
                  if not E.Mark then
                     Score := Score + Integer (E.Num);
                  end if;
               end loop;
               Score := Score * Integer (Num);
               return Score;
            end if;
         end;
      end loop;
      return 0;
   end Score;
   
begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day04 <filename>");
      Set_Exit_Status (Failure);
      return;
   end if;
   
   Put_Line ("Part 1:");
   Put_Line ("  Score of winning board:" & Score (Argument (1))'Img);
end Day04;
