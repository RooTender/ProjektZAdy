with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;


procedure Simulation is
   Number_Of_Products: constant Integer := 4;
   Number_Of_Assemblies: constant Integer := 3;
   Number_Of_Consumers: constant Integer := 2;

   subtype Product_Type is Integer range 1 .. Number_Of_Products;
   subtype Assembly_Type is Integer range 1 .. Number_Of_Assemblies;
   subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;

   Product_Name: constant array (Product_Type) of String(1 .. 6)
     := ("boards", "glass ", "handle", "nails ");

   Assembly_Name: constant array (Assembly_Type) of String(1 .. 9)
     := ("table    ", "dresser  ", "window   ");

   package Random_Assembly is new Ada.Numerics.Discrete_Random(Assembly_Type);


   -- Producer (ID, ProductionTime)
   task type Producer_task is

      entry Start(Product: in Product_Type;
                  Production_Time: in Integer);
   end Producer_task;


   -- Consumer (ID, ConsumptionTime)
   task type Consumer_task is

      entry Start(Consumer_Number: in Consumer_Type;
                  Consumption_Time: in Integer);
   end Consumer_task;


   -- Buffer [
   --    Take(Procuct, SerialNum),
   --    Deliver(Assembly, SerialNum)
   -- ]
   task type Buffer_task is
      entry Take(Product: in Product_Type; Number: in Integer);
      entry Deliver(Assembly: in Assembly_Type; Is_Assembly_Ready: out Boolean);
   end Buffer_task;

   Producer: array ( 1 .. Number_Of_Products ) of Producer_task;
   Consumer: array ( 1 .. Number_Of_Consumers ) of Consumer_task;
   Buffer: Buffer_task;



   -- PRODUCENT
   task body Producer_task is
      subtype Production_Time_Range is Integer range 3 .. 6;
      package Random_Production_Time is new Ada.Numerics.Discrete_Random(Production_Time_Range);

      Number_Generator: Random_Production_Time.Generator;
      Product_Type_Number: Integer;
      Product_Number: Integer;
      Production: Integer;

   begin

      -- generate product data
      accept Start(Product: in Product_Type; Production_Time: in Integer) do
         Random_Production_Time.Reset(Number_Generator);
         Product_Number := 0;
         Product_Type_Number := Product;
         Production := Production_Time;
      end Start;

      Put_Line("Started producer => " & Product_Name(Product_Type_Number));


      -- production simulation
      loop
         delay Duration(Random_Production_Time.Random(Number_Generator));
         Put_Line("Produced product => " & Product_Name(Product_Type_Number) & Integer'Image(Product_Number));

         delay Duration(5.0);

         loop
            select
               -- Accept for storage
               delay Duration(10.0);
               Put_Line("Product " & Product_Name(Product_Type_Number) & Integer'Image(Product_Number) & " is in queue...");

            then abort
               Buffer.Take(Product_Type_Number, Product_Number);
               Product_Number := Product_Number + 1;
            end select;
         end loop;
      end loop;

   end Producer_task;


   -- CLIENT
   task body Consumer_task is
      subtype Consumption_Time_Range is Integer range 4 .. 8;
      package Random_Consumption is new Ada.Numerics.Discrete_Random(Consumption_Time_Range);

      Time_Generator: Random_Consumption.Generator;
      Assembly_Time_Generator: Random_Assembly.Generator;

      Consumer_Type_Number: Consumer_Type;
      Assembly_Number: Integer;
      Assembly_Type: Integer;

      Consumer_Name: constant array (1 .. Number_Of_Consumers) of String(1 .. 6)
        := ("IKEA  ", "Bodzio");

      Assembly_Ready : Boolean;

   begin
      accept Start(Consumer_Number: in Consumer_Type; Consumption_Time: in Integer) do
         Random_Consumption.Reset(Time_Generator);
         Random_Assembly.Reset(Assembly_Time_Generator);
         Consumer_Type_Number := Consumer_Number;
         Assembly_Number := 0;
      end Start;

      Put_Line("Started consumer " & Consumer_Name(Consumer_Type_Number));
      loop
         delay Duration(Random_Consumption.Random(Time_Generator)); --  simulate consumption
         Assembly_Type := Random_Assembly.Random(Assembly_Time_Generator);

         loop
            select
               -- take an assembly for consumption
               Buffer.Deliver(Assembly_Type, Assembly_Ready);

               if(Assembly_Ready) then
                  Put_Line(Consumer_Name(Consumer_Type_Number) &
                             " => taken assembly => " &
                             Assembly_Name(Assembly_Type) &
                             " number " &
                             Integer'Image(Assembly_Number));
                  Assembly_Number := Assembly_Number + 1;
               end if;
            else
               Put_Line("Consumer " & Consumer_Name(Consumer_Type_Number) & " is waiting for order: " & Assembly_Name(Assembly_Type) &
                          Integer'Image(Assembly_Number));
            end select;

            delay Duration(4.0);
         end loop;
      end loop;

   end Consumer_task;


   -- BUFFER
   task body Buffer_task is

      Storage_Capacity: constant Integer := 15;

      type Storage_type is array (Product_Type) of Integer;
      Storage: Storage_type
        := (0, 0, 0, 0);

      Max_Product_In_Assembly: array(Product_Type) of Integer;
      Max_Other_In_Assembly: array(Product_Type) of Integer
        := (0, 0, 0, 0);

      Assembly_Content: array(Assembly_Type, Product_Type) of Integer
        := ((5, 0, 0, 1),
            (4, 0, 2, 3),
            (2, 1, 1, 1));

      Assembly_Number: array(Assembly_Type) of Integer
        := (0, 0, 0);

      In_Storage: Integer := 0;

      procedure Set_Max_Product_In_Assembly is
      begin

         for p in Product_Type loop
            Max_Product_In_Assembly(p) := 0;

            for a in Assembly_Type loop
               if Assembly_Content(a, p) > Max_Product_In_Assembly(p) then
                  Max_Product_In_Assembly(p) := Assembly_Content(a, p);
               end if;
            end loop;

         end loop;
      end Set_Max_Product_In_Assembly;

      procedure Set_Max_Other_In_Assembly is
         Max_Of_Others_In_Assembly : Integer;

      begin
         for product in Product_Type loop
            Max_Other_In_Assembly(product) := 0;

            for a in Assembly_Type loop
               Max_Of_Others_In_Assembly := 0;

               for i in Product_Type loop
                  if (i /= product) then
                     Max_Of_Others_In_Assembly := Max_Of_Others_In_Assembly + Assembly_Content(a, i);
                  end if;
               end loop;

               Max_Other_In_Assembly(product) := Integer'Max(Max_Other_In_Assembly(product), Max_Of_Others_In_Assembly);
            end loop;
         end loop;
      end Set_Max_Other_In_Assembly;


      function Can_Accept(Product: Product_Type) return Boolean is
         Free_Space: Integer;
         Min_Requirements : Boolean;

      begin

         if In_Storage >= Storage_Capacity then
            return False;
         end if;

         -- if exactly this product lacks
         if Integer'Max(0, Max_Product_In_Assembly(Product) - Storage(Product)) > 0 then
            return True;
         end if;

         -- There is free room in the storage
         Free_Space := Storage_Capacity - In_Storage;

         if Free_Space > Max_Other_In_Assembly(Product) then

            Min_Requirements := True;
            for i in Product_Type loop
               if Storage(i) < Max_Product_In_Assembly(i) then
                  Min_Requirements := False;
               end if;
            end loop;

            if Min_Requirements then
               return True;
            end if;

         end if;

         -- no room for this product
         return False;
      end Can_Accept;


      -- check if there is enough products for assembly
      function Can_Deliver(Assembly: Assembly_Type) return Boolean is
      begin

         for product in Product_Type loop
            if Storage(product) < Assembly_Content(Assembly, product) then
               return False;
            end if;
         end loop;

         return True;
      end Can_Deliver;


      procedure Storage_Contents is
      begin

         for product in Product_Type loop
            Put_Line("Storage contents: " & Integer'Image(Storage(product)) & " " & Product_Name(product));
         end loop;
         Put_Line("");

      end Storage_Contents;

   begin
      Put_Line("Buffer started");
      Set_Max_Product_In_Assembly;
      Set_Max_Other_In_Assembly;

      loop
         accept Take(Product: in Product_Type; Number: in Integer) do
            if Can_Accept(Product) then
               Put_Line("Accepted product " & Product_Name(Product) & " number " & Integer'Image(Number));

               -- update buffer storage
               Storage(Product) := Storage(Product) + 1;
               In_Storage := In_Storage + 1;
            else
               Put_Line("Storage full. Product: " & Product_Name(Product)  & Integer'Image(Number) & " is in queue...");
               delay(20.0);
            end if;
            Storage_Contents;
         end Take;

         --Storage_Contents;
         accept Deliver(Assembly: in Assembly_Type; Is_Assembly_Ready: out Boolean) do
            Is_Assembly_Ready := False;

            if Can_Deliver(Assembly) then
               Is_Assembly_Ready := True;

               Put_Line("Delivered assembly " & Assembly_Name(Assembly) & " number " & Integer'Image(Assembly_Number(Assembly) + 1));

               -- remove used products from storage
               for product in Product_Type loop
                  Storage(product) := Storage(product) - Assembly_Content(Assembly, product);
                  In_Storage := In_Storage - Assembly_Content(Assembly, product);
               end loop;

               Is_Assembly_Ready := True;

               Storage_Contents;
            end if;

         end Deliver;
      end loop;

   end Buffer_task;

begin
   for I in 1 .. Number_Of_Products loop
      Producer(I).Start(I, 10);
   end loop;
   for J in 1 .. Number_Of_Consumers loop
      Consumer(J).Start(J,12);
   end loop;
end Simulation;
