-- adbr 2021-12-04

--  Part 1:
--    First winner score: 65325
--  Part 2:
--    Last winner score: 4624

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
   
   Value_Error : exception;
   
   package Number_IO is new Ada.Text_IO.Integer_IO (Number);
   use Number_IO;
   
   package Number_Vector is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Number);
   
   package Board_Vector is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Board_Type);
   
   procedure Print_Board (B : Board_Type) is
   
      function Board_Entry_Image (E : Board_Entry) return String is
      begin
         return E.Num'Img & (if E.Mark then "*" else " ");
      end Board_Entry_Image;
      
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
      
   procedure Read_Data (File_Name : String;
                        Numbers   : out Number_Vector.Vector;
                        Boards    : out Board_Vector.Vector) is
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
                          Num    : Number) is
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
   
   function Is_Winner_Board (B : Board_Type) return Boolean is
   begin
      -- check rows
      for I in B'Range (1) loop
         declare
            Mark : Boolean := True;
         begin
            for J in B'Range (2) loop
               Mark := Mark and B(I, J).Mark;
            end loop;
            if Mark then
               return True;
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
               return True;
            end if;
         end;
      end loop;
      
      return False;
   end Is_Winner_Board;
   
   function Score (B : Board_Type; Num : Number) return Integer is
      Result : Integer := 0;
   begin
      for E of B loop
         if not E.Mark then
            Result := Result + Integer (E.Num);
         end if;
      end loop;
      Result := Result * Integer (Num);
      return Result;
   end Score;
   
   procedure Winner_Score (File_Name          : String;
                           First_Winner_Score : out Integer;
                           Last_Winner_Score  : out Integer) is
      Numbers        : Number_Vector.Vector;
      Boards         : Board_Vector.Vector;
      Winner_Boards  : Board_Vector.Vector;
      Winner_Numbers : Number_Vector.Vector;
      Lose_Boards    : Board_Vector.Vector;
   begin
      Read_Data (File_Name, Numbers, Boards);
      
      for Num of Numbers loop
         Mark_Number (Boards, Num);
         for B of Boards loop
            if Is_Winner_Board (B) then
               Winner_Boards.Append (B);
               Winner_Numbers.Append (Num);
            else
               Lose_Boards.Append (B);
            end if;
         end loop;
         Boards.Assign (Lose_Boards);
         Lose_Boards.Clear;
      end loop;
      
      if Winner_Boards.Length = 0 then
         raise Value_Error with "Missing winner board";
      end if;
      
      First_Winner_Score := Score (Winner_Boards.First_Element,
                                   Winner_Numbers.First_Element);
      
      Last_Winner_Score := Score (Winner_Boards.Last_Element,
                                  Winner_Numbers.Last_Element);
   end Winner_Score;
   
   First_Winner_Score : Integer;
   Last_Winner_Score  : Integer;
begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day04 <filename>");
      Set_Exit_Status (Failure);
      return;
   end if;
   
   Winner_Score (Argument (1), First_Winner_Score, Last_Winner_Score);
   
   Put_Line ("Part 1:");
   Put_Line ("  First winner score:" & First_Winner_Score'Img);
   
   Put_Line ("Part 2:");
   Put_Line ("  Last winner score:" & Last_Winner_Score'Img);
end Day04;
