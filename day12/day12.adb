-- adbr 2021-12-24

-- Part 1: OK
-- Part 2: unfinished

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Containers.Vectors;   use Ada.Containers;
with Ada.Characters.Handling;  use Ada.Characters.Handling;

procedure Day12 is
   
   Value_Error : exception;
   
   type Node_Type;
   type Node_Access is access Node_Type;
   
   package Node_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Node_Access);
   
   type Node_Type is
      record
         Name      : Unbounded_String;
         Small     : Boolean;
         Visited   : Natural := 0;
         Neighbors : Node_Vector.Vector;
      end record;
   
   procedure Print_Nodes (Nodes : Node_Vector.Vector) is
   begin
      for N of Nodes loop
         Put_Line (N.Name);
         Put ("  ");
         for E of N.Neighbors loop
            Put (E.Name);
            Put (", ");
         end loop;
         New_Line;
      end loop;
   end Print_Nodes;
         
   function Contains_Node (Nodes : Node_Vector.Vector;
                           Name : Unbounded_String) return Boolean is
   begin
      for N of Nodes loop
         if N.Name = Name then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Node;
   
   function Find_Node (Nodes : Node_Vector.Vector;
                       Name : Unbounded_String) return Node_Access is
   begin
      for N of Nodes loop
         if N.Name = Name then
            return N;
         end if;
      end loop;
      raise Value_Error with "Missing node: " & To_String (Name);
   end Find_Node;
         
   function Is_Small_Case (Name : Unbounded_String) return Boolean is
   begin
      for I in 1 .. Length (Name) loop
         if Is_Upper (Element (Name, I)) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Small_Case;

   procedure Read_Data (File_Name : String; Nodes : out Node_Vector.Vector) is
      
      procedure Node_Names (Line : String;
                            Name1, Name2 : out Unbounded_String)
      is
         Separator : constant String := "-";
         I : Natural := Index (Line, Separator);
      begin
         if I = 0 then
            raise Value_Error with "Missing node separator: '" & Line & "'";
         end if;
         Name1 := To_Unbounded_String
           (Line (Line'First .. I - 1));
         Name2 := To_Unbounded_String
           (Line (I + Separator'Length .. Line'Last));
      end Node_Names;
      
      procedure Add_Edge (Nodes : in out Node_Vector.Vector;
                          Name1, Name2 : Unbounded_String) is

         Node1, Node2 : Node_Access;
      begin
         if Contains_Node (Nodes, Name1) then
            Node1 := Find_Node (Nodes, Name1);
         else
            Node1 := new Node_Type;
            Node1.Name := Name1;
            Node1.Small := Is_Small_Case (Name1);
            Nodes.Append (Node1);
         end if;
         
         if Contains_Node (Nodes, Name2) then
            Node2 := Find_Node (Nodes, Name2);
         else
            Node2 := new Node_Type;
            Node2.Name := Name2;
            Node2.Small := Is_Small_Case (Name2);
            Nodes.Append (Node2);
         end if;
         
         Node1.Neighbors.Append (Node2);
         Node2.Neighbors.Append (Node1);
      end Add_Edge;
      
   begin
      declare
         File : File_Type;
      begin
         Open (File, In_File, File_Name);
         while not End_Of_File (File) loop
            declare
               Line   : String := Get_Line (File);
               Name1, Name2 : Unbounded_String;
            begin
               Node_Names (Line, Name1, Name2);
               Add_Edge (Nodes, Name1, Name2);
            end;
         end loop;
         Close (File);
      end;
   end Read_Data;
   
   procedure Search_Paths_1 (Nodes : Node_Vector.Vector;
                             Node : Node_Access;
                             Paths : in out Natural) is
      
      function Is_Start (Node : Node_Access) return Boolean is
      begin
         return To_String (Node.Name) = "start";
      end Is_Start;
      
      function Is_End (Node : Node_Access) return Boolean is
      begin
         return To_String (Node.Name) = "end";
      end Is_End;

   begin
      if Is_End (Node) then
         Paths := Paths + 1;
         return;
      end if;
      
      if Node.Small then
         if Node.Visited = 1 then
            return;
         else
            Node.Visited := 1;
         end if;
      end if;
      
      for N of Node.Neighbors loop
         Search_Paths_1 (Nodes, N, Paths);
      end loop;
      
      Node.Visited := 0;
   end Search_Paths_1;
   
   procedure Search_Paths_2 (Nodes : Node_Vector.Vector;
                             Node : Node_Access;
                             Paths : in out Natural) is
      
      function Is_Start (Node : Node_Access) return Boolean is
      begin
         return To_String (Node.Name) = "start";
      end Is_Start;
      
      function Is_End (Node : Node_Access) return Boolean is
      begin
         return To_String (Node.Name) = "end";
      end Is_End;

   begin
      if Is_End (Node) then
         Paths := Paths + 1;
         return;
      end if;
      
      if Node.Small then
         if Node.Visited >= 2 then
            return;
         else
            Node.Visited := Node.Visited + 1;
         end if;
      end if;
      
      for N of Node.Neighbors loop
         Put (Node.Name); Put ("-"); Put (N.Name); Put (", ");
         Search_Paths_2 (Nodes, N, Paths);
      end loop;
      
      if Node.Small then
         Node.Visited := Node.Visited - 1;
      end if;
   end Search_Paths_2;

   procedure Part_1 (File_Name : String) is
      Nodes : Node_Vector.Vector;
      Paths : Natural := 0;
   begin
      Read_Data (File_Name, Nodes);
      declare
         Start_Node : Node_Access :=
           Find_Node (Nodes, To_Unbounded_String ("start"));
      begin
         Search_Paths_1 (Nodes, Start_Node, Paths);
      end;
      
      Put_Line ("Part 1:");
      Put_Line ("  Number of paths:" & Paths'Img);
   end Part_1;

   procedure Part_2 (File_Name : String) is
      Nodes : Node_Vector.Vector;
      Paths : Natural := 0;
   begin
      Read_Data (File_Name, Nodes);
      declare
         Start_Node : Node_Access :=
           Find_Node (Nodes, To_Unbounded_String ("start"));
      begin
         Search_Paths_2 (Nodes, Start_Node, Paths);
      end;
      
      Put_Line ("Part 2:");
      Put_Line ("  Number of paths:" & Paths'Img);
   end Part_2;

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day12 <inputfile>");
   end if;
   
   Part_1 (Argument (1));
   Part_2 (Argument (1));
end Day12;

--  Part 1:
--    Number of paths: 4912
