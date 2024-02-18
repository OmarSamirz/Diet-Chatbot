open Random
#load "str.cma";;

type diet_foods={ name : string;calories : float }

type food_record = { food_item : string; calories : float }

type user = {
             mutable name : string;
             mutable calories : float;
             mutable age : float;
             mutable height : float;
             mutable weight : float;
             mutable bmi : float;
             mutable gender : string;
             mutable activity_level : string;
             mutable goal : string; 
             mutable my_list_of_food : string list;
             mutable chatbot_list_of_food : string list;
             }

let current_user = {
                    name = "None"; 
                    calories = -1.;
                    age = -1.;
                    height = -1.;
                    weight = -1.;
                    bmi = -1.;
                    gender = "None";
                    activity_level = "None";
                    goal = "None";
                    my_list_of_food = [];
                    chatbot_list_of_food = [];
                   }

let current_diet_foot = {
                          name = "None";
                          calories = -1.
                        }

let calculate_bmr (gender : string list) (weight : float) (height : float) (age : float) : float =
  match gender with
  | ["male"] | ["m"] -> 88.362 +. (13.397 *. weight) +. (4.799 *. height) -. (5.677 *. age)
  | ["female"] | ["f"] -> 447.593 +. (9.247 *. weight) +. (3.098 *. height) -. (4.330 *. age)
  | _ -> failwith "Invalid gender"
;;

let calculate_tdee (bmr : float) (activity_level : string list) : float =
  match activity_level with
  | ["sedentary"] | ["s"] -> bmr *. 1.2
  | ["lightly_active"] | ["lightly"; "active"] | ["lightly"] | ["l"] -> bmr *. 1.375
  | ["moderately_active"] | ["moderately"; "active"] | ["moderately"] | ["m"] -> bmr *. 1.55
  | ["very_active"] | ["very"; "active"] | ["very"] | ["v"] -> bmr *. 1.725
  | ["extra_active"] | ["extra"; "active"] | ["extra"] | ["e"] -> bmr *. 1.9
  | _ -> failwith "Invalid activity level"
;;

let calculate_calorie_needs (gender : string list) (weight : float) (height : float) (age : float) (activity_level : string list) (goal : string list) : float =
  let bmr = calculate_bmr gender weight height age in
  let tdee = calculate_tdee bmr activity_level in
  match goal with
  | ["lose_weight"] | ["lose"; "weight"] | ["l"] | ["lose"] -> tdee -. 500.
  | ["gain_weight"]| ["gain"; "weight"] | ["g"] | ["gain"]-> tdee +. 250.
  | _ -> tdee
;;

let pick_random_answer lst =
  let len = List.length lst in
  let index = Random.int len in
  List.nth lst index
;;

let greet_user u () =
  let massege = "\nHello " ^ u.name ^ " Welcome to our diet and nutrition chatbot!\n How can I help you?" in
  print_string massege;
;;

let doing_well_answers =
  let random_lst = ["I'm doing well, thanks for asking. How about you?"; "I'm greet, what about you?"] in
  
  pick_random_answer random_lst
;;

let what_can_do_answers =
  let random_lst = ["I can provide you with general information and advice on diet and food regulation."] in
  
  pick_random_answer random_lst
;;

let user_information prompt user =
  match prompt with
  | "fat" -> let output =
    let output_string = "According to your height and weight you are " in
    match user.bmi with
    | a when a < 18.5 -> output_string ^ "thin."
    | a when a > 18.5 && a < 24.9 -> output_string ^ "healthy."
    | a when a > 25. && a < 29.9 -> output_string ^ "fat."
    | _ -> output_string ^ "very fat."
  in
  output
  | "tall" -> let output =
    let output_string = "According to your height you are " in
    match user.gender with
    | "male" | "m" -> 
      (match user.height with
      | a when a < 175. && a > 170. -> output_string ^ "short."
      | a when a < 179. && a > 175. -> output_string ^ "average."
      | a when a < 194. && a > 180. -> output_string ^ "tall."
      | a when a > 195. -> output_string ^ "vary tall."
      | _ -> output_string ^ "very short.")
    | _ -> 
      (match user.height with
      | a when a < 162. && a > 155. -> output_string ^ "short."
      | a when a < 170. && a > 162. -> output_string ^ "average."
      | a when a < 175. && a > 170. -> output_string ^ "tall."
      | a when a > 180. -> output_string ^ "vary tall."
      | _ -> output_string ^ "very short.")
  in
  output
  | "calories" -> "Needed calories per day: " ^ string_of_float user.calories
  | _ -> "Invalid prompt"
;;

let user_info_return (input_lst : string list) (u : user) (input_string : string) =
  match u.name with
  | "None" -> "You need to enter your information"
  | _ -> (
          match input_string with
          | "tall" | "height" | "long" -> "Your height is " ^ string_of_float u.height
          | "fat" | "thin" | "skinny" | "weight" | "heavy" | "obese" | "overweight" | "underweight" -> "Your weight is " ^ string_of_float u.weight
          | _ -> "Invalid input"
        )
;;

let print_food_of_lst lst =
  if (List.length lst) = 0 then "your list of food is empty"
  else
    List.fold_left (fun acc food -> acc ^ "\n" ^ "- " ^ food) ("") lst
;;

let parse_input (input : string) : string list =
  let remove_chars str =
    let chars_to_remove = ['?'; ','; '.'; '!'] in
    let should_remove c = List.mem c chars_to_remove in
    let filtered_chars = String.to_seq str |> Seq.filter (fun c -> not (should_remove c)) |> String.of_seq in
    filtered_chars
  in
  remove_chars input
  |> String.lowercase_ascii
  |> Str.split (Str.regexp "[ \t]+")
  |> List.map String.trim 
  |> List.filter (fun s -> s <> "") 
;;

let find_calories food_name =
  let file = open_in "foodc" in
  let rec loop () =
    try
      let line = input_line file in
      let columns = String.split_on_char ',' line in
      match columns with
      | [name; other; calories] when String.lowercase_ascii(name) = String.lowercase_ascii(food_name) || String.lowercase_ascii(other) = String.lowercase_ascii(food_name) ->
          close_in file;
          Some (float_of_string calories)
      | _ -> loop ()
    with End_of_file ->
      close_in file;
      None
  in
  loop ()
;;

let read_file foodc =
  let input_channel = open_in foodc in
  let rec loop () =
    try
      let line = input_line input_channel in
      let fields = String.split_on_char ',' line in
      let name = List.nth fields 0 in
      Printf.printf "%s\n" name ;
      loop ()
    with End_of_file ->
      []
  in
  let data = loop () in
  close_in input_channel;
  data
;;

let find_calories_user_input (calorie_needs : float) () =
  let _ = read_file "foodc" in
  Printf.printf "\nRemaining calories: %.2f\n" calorie_needs;
  print_string "\nEnter a food item: ";
  let food_name = read_line () in
  print_string "\nEnter how many grams would you like: ";
  let food_grams = read_line () in
  let food_grams = float_of_string food_grams in
  match find_calories food_name with
  | Some c ->(food_name,((food_grams/.100.0)*.c))
  | None -> print_string "Food item not found\n";("",0.0);
;;

let rec choice (calorie_needs: float) (chosen_foods: string list) (u : user) : string =
  print_string "Would you like to enter a food item? ";
  let response = parse_input (read_line ()) in

  match response with
  | ["yes"] | ["y"] | ["agree"] | ["ok"] | ["sure"] ->
    let food_name, calorie = find_calories_user_input calorie_needs () in
    let remaining_calories = (calorie_needs -. calorie) in
    if remaining_calories < 0.0 then (
      Printf.printf "Calorie limit exceeded. Please pick another option within %.2f \n" calorie_needs;
      choice calorie_needs chosen_foods u;
    ) else
      choice remaining_calories (chosen_foods@[food_name]) u
  | ["no"] ->
    print_endline "Chosen foods:";
    u.my_list_of_food <- chosen_foods;
    List.fold_left (fun acc food -> "\n" ^ "- " ^ food ^ acc) ("") chosen_foods
  | _ ->
    print_endline "Invalid reponse, please enter a valid option, [yes/no]";
    choice calorie_needs chosen_foods u;
;;

let read_data_breakfast filename =
  let file = open_in filename in
  let rec loop n acc =
    if n = 1 then
      List.rev acc
    else
      let line = input_line file in
      let fields = String.split_on_char ',' line in
      let food_item = List.hd fields in (*Takes out the food item*)
      let calories = float_of_string (List.nth fields 1) in (*Takes out calories then converts to string*)
      let record = { food_item; calories } in
      loop (n - 1) (record :: acc)
  in
  ignore (input_line file);  (* Skip the first line *)
  let data = loop 14 [] in
  close_in file;
  data
;;

let filename = "orderedfood.csv";;  (* Replace with your actual file name *)
let breakfast_list = read_data_breakfast filename;;

let print_food_record (record : food_record) =
  Printf.printf "Food Item: %s, Calories: %.2f\n" record.food_item record.calories
;;
let print_data (data : food_record list) =
  print_string "\nBreakfast Menu: \n";
  List.iter print_food_record breakfast_list
;;

let read_data_lunch filename =
  let file = open_in filename in
  let rec loop n acc =
    if n = 0 then
      List.rev acc
    else
      let line = input_line file in
      let fields = String.split_on_char ',' line in
      let food_item = List.hd fields in
      let calories = float_of_string (List.nth fields 1) in
      let record = { food_item; calories } in
      loop (n - 1) (record :: acc)
  in
  ignore (input_line file);  (* Skip the first line *)
  ignore (loop 13 []);  (* Skip lines 2 to 14 *)
  let data = loop 6 [] in  (* Read lines 15 to 20 *)
  close_in file;
  data
;;

let filename = "orderedfood.csv";;  (* Replace with your actual file name *)
let lunch_list = read_data_lunch filename;;

let print_food_record (record : food_record) =
  Printf.printf "Food Item: %s, Calories: %.2f\n" record.food_item record.calories
;;
let print_data_lunch (data : food_record list) =
  print_string "\nLunch Menu: \n";
  List.iter print_food_record lunch_list
;;

let read_data_dinner filename =
  let file = open_in filename in
  let rec loop n acc =
    if n = 0 then
      List.rev acc
    else
      let line = input_line file in
      let fields = String.split_on_char ',' line in
      let food_item = List.hd fields in
      let calories = float_of_string (List.nth fields 1) in
      let record = { food_item; calories } in
      loop (n - 1) (record :: acc)
  in
  ignore (input_line file);  (* Skip the first line *)
  ignore (loop 19 []);  (* Skip lines 2 to 20 *)
  (* ignore (loop 6 []); *)
  let data = loop 11 [] in  (* Read lines 20 to 31*)
  close_in file;
  data
;;

let filename = "orderedfood.csv";;  (* Replace with your actual file name *)
let dinner_list = read_data_dinner filename;;

let print_food_record (record : food_record) =
  Printf.printf "Food Item: %s, Calories: %.2f\n" record.food_item record.calories
;;
let print_data_dinner (data : food_record list) =
  print_string "\nDinner Menu: \n";
  List.iter print_food_record dinner_list
;;

print_data_dinner dinner_list;;

let random_break_fast = ref ((Random.int 50) mod 14);;
let random_lunch =  ref ((Random.int 50) mod 6);;
let random_dinner =  ref ((Random.int 50)mod 11);;

let create_diet (calories : float) (breakfast_list : food_record list) (lunch_list : food_record list) (dinner_list : food_record list) =
  let percentages = [|0.15; 0.10; 0.15; 0.15; 0.10; 0.20; 0.15|] in

  let break1 =  
    let food = List.nth breakfast_list !random_break_fast in
    random_break_fast := (!random_break_fast + 1) mod 14;
    {food_item = food.food_item; calories = ((percentages.(0) *. calories) *. 100.) /. food.calories}
  in

  let break2 = 
    let food = List.nth breakfast_list (if (!random_break_fast + 1) >= 14 then 0 else (!random_break_fast + 1)) in
    random_break_fast := (!random_break_fast + 2) mod 14; 
    {food_item = food.food_item; calories = ((percentages.(1) *. calories) *. 100.) /. food.calories}
  in

  let lunch1 =
    let food = List.nth lunch_list !random_lunch in
    random_lunch := (!random_lunch + 1) mod 6;
    {food_item = food.food_item; calories = ((percentages.(2) *. calories) *. 100.) /. food.calories}
  in

  let lunch2 =
    let food = List.nth lunch_list (if (!random_lunch + 1) >= 6 then 1 else (!random_lunch + 1)) in
    random_lunch := (!random_lunch + 2) mod 6;
    {food_item = food.food_item; calories = ((percentages.(3) *. calories) *. 100.) /. food.calories}
  in
  
  let lunch3 =
    let food = List.nth lunch_list (if (!random_lunch + 2) >= 6 then 2 else (!random_lunch + 2)) in
    random_lunch := (!random_lunch + 3) mod 6;
    {food_item = food.food_item; calories = ((percentages.(4) *. calories) *. 100.) /. food.calories}
  in

  let dinner1 =
    let food = List.nth dinner_list (!random_dinner) in
    random_dinner := (!random_dinner + 1) mod 11;
    {food_item = food.food_item; calories = ((percentages.(5) *. calories) *. 100.) /. food.calories}
  in

  let dinner2 =
    let food = List.nth dinner_list (if (!random_dinner + 1) >= 11 then 0 else (!random_dinner + 1)) in
    random_dinner := (!random_dinner + 2) mod 11;
    {food_item = food.food_item; calories = ((percentages.(6) *. calories) *. 100.) /. food.calories}
  in

  let lst = break1::break2::lunch1::lunch2::lunch3::dinner1::dinner2::[] in
  List.fold_left (fun acc food -> "\n- " ^ food.food_item ^ " " ^ string_of_float food.calories ^ acc) ("") lst
;;


let handle_user_input (input : string list) (u : user) (input_string : string) : string =
  match input with
  | ["i"; "am"; "i'm"; "feel"; "sad"; "bad"; "do"; "not"; "don't"; "good"] -> "What can i do to make you feel better"
  | ["how"; "what"; "are"; "you"; "doing"; "do"; "feel";"?"] -> doing_well_answers
  | ["what"; "can"; "are"; "you"; "Your"; "do"; "capabilities";"?"] -> what_can_do_answers
  | ["what"; "how"; "long"; "i"; "is"; "my"; "height"; "tall"; "fat"; "thin"; "skinny"; "weight"] -> user_info_return input u input_string
  | ["am"; "i"; "short"; "tall"; "long"] -> user_information "tall" u
  | ["am"; "i"; "fat"; "thin"; "skinny"; "heavy"; "obese"; "overweight"; "underweight"] -> user_information "fat" u
  | ["i"; "want"; "make"; "my"; "list"; "food"; "of"; "diet"; "plan"; "menu"; "create"] -> choice (u.calories) ([]) u
  | ["create"; "list"; "for"; "me"; "a"; "food"; "of"; "diet"; "plan"; "menu"] -> create_diet u.calories breakfast_list lunch_list dinner_list
  | ["show"; "me"; "calories"; "calorie"; "what"; "is"; "are"; "my"] -> user_information "calories" u
  | ["show"; "my"; "list"; "food"; "of"] -> print_food_of_lst u.my_list_of_food
  | ["show"; "list"; "you"; "made"; "food"; "of"; "created"] -> "Not finished yet"
  | _ -> "I'm sorry, I didn't understand. Could you please rephrase that?"
;;

let rec compare lst1 lst2 =
  let rec loop lst1 lst2 count = 
    match lst2 with
    | [] -> count 
    | h::t when List.hd lst1 = h -> loop lst1 t (count + 1)
    | _::t -> loop lst1 t (count)
  in
  match lst1 with
  | [] -> 0
  | h::t -> loop lst1 lst2 0 + compare t lst2
;;

let rec compare_input (lst : string list) (u : user) = 
  let lst_of_lst_ask = [["i"; "am"; "i'm"; "feel"; "sad"; "bad"; "do"; "not"; "don't"; "good"];
                        ["how"; "what"; "are"; "you"; "doing"; "do"; "feel";"?"];
                        ["what"; "can"; "are"; "you"; "Your"; "do"; "capabilities";"?"]; 
                        ["what"; "how"; "long"; "i"; "is"; "my"; "height"; "tall"; "fat"; "thin"; "skinny"; "weight"];
                        ["am"; "i"; "short"; "tall"; "long"];
                        ["am"; "i"; "fat"; "thin"; "skinny"; "heavy"; "obese"; "overweight"; "underweight"];
                        ["i"; "want"; "make"; "my"; "list"; "food"; "of"; "diet"; "plan"; "menu"; "create"];
                        ["create"; "list"; "for"; "me"; "a"; "food"; "of"; "diet"; "plan"; "menu"];
                        ["show"; "me"; "calories"; "calorie"; "what"; "is"; "are"; "my"];
                        ["show"; "list"; "you"; "made"; "food"; "of"; "created"];
                        ["show"; "my"; "list"; "food"; "of"] ] 
  in 

  let rec lst_contain l =
    match l with
    | [] -> "Nothing"
    | h::t -> (
        match h, t with
        | "tall", _ | "height", _ | "long", _ -> h
        | "fat", _ | "thin", _ | "skinny", _ | "weight", _ -> h
        | _, t -> lst_contain t 
    )
  in

  let rec compare_inside lst1 lst2 count output=
    match lst1 with
    | [] -> output
    | h::t -> let a = compare h lst in 
        if a > count then 
          compare_inside t lst2 a h
        else 
          compare_inside t lst2 count output
  in

  let result = compare_inside lst_of_lst_ask lst 2 [""] in 

  handle_user_input result u (lst_contain lst)
;;

let handle (input : string list) (u : user) =
  match input with
  | ["hi"] | ["hello"] | ["hey"] | ["hola"] -> "Hello!"
  | ["calories"] | ["my"; "calories"] -> user_information "calories" u
  | ["height"] | ["my"; "height"] -> user_info_return input u "tall"
  | ["weight"] | ["my"; "weight"] -> user_info_return input u "fat"
  | ["thank"; "you"] | ["thanks"] -> "You are welcome"
  | ["good"] | ["excellent"] | ["great"] | ["fine"] | ["superb"] | ["outstanding"] | ["remarkable"] | ["admirable"] | ["commendable"] | ["worthy"] | ["decent"] | ["satisfactory"] -> "Happy to hear that! How can I assist you today?"
  | _ -> (compare_input input u)
;;

let generate_response (input : string) (u : user) : string =
  let tokens = parse_input input in
  handle tokens u
;;

let rec get_information user () =
  let rec get_name_input () =
    print_string "What is your name? ";
    let input = read_line () in
    if String.length input > 0 && not (Str.string_match (Str.regexp "^[0-9]+$") input 0) then
      input
    else (
      print_string "Invalid input. Please enter a valid name.\n";
      get_name_input ()
    )
  in

  let rec get_age_input () =
    print_string "What is your age? ";
    try 
      let input = float_of_string (read_line ()) in
      if input <= 0.0 then (
        print_string "Invalid input. Please enter a positive number.\n";
        get_age_input ()
      ) else input
    with
    | _ -> (
        print_string "Invalid input. Please enter a number.\n";
        get_age_input ()
      )
  in

  let rec get_weight_input () =
    print_string "What is your weight? ";
    try 
      let input = float_of_string (read_line ()) in
      if input <= 0.0 then (
        print_string "Invalid input. Please enter a positive number.\n";
        get_weight_input ()
      ) else input
    with
    | _ -> (
        print_string "Invalid input. Please enter a number.\n";
        get_weight_input ()
      )
  in

  let rec get_height_input () =
    print_string "What is your height? ";
    try 
      let input = float_of_string (read_line ()) in
      if input <= 0.0 then (
        print_string "Invalid input. Please enter a positive number.\n";
        get_height_input ()
      ) else input
    with
    | _ -> (
        print_string "Invalid input. Please enter a number.\n";
        get_height_input ()
      )
  in

  let name = get_name_input () in
  user.name <- name;
  let rec get_gender_input () =
    print_string "What is your gender? ";
    let gender = parse_input (read_line ()) in
    match gender with
    | ["male"] | ["m"] | ["female"] | ["f"] ->
      user.gender <- List.hd gender;
      let age = get_age_input () in
      user.age <- age;
      let height = get_height_input () in
      user.height <- height;
      let weight = get_weight_input () in
      user.weight <- weight;
      let bmi = (weight /. ((height /. 100.) *. (height /. 100.))) in
      user.bmi <- bmi;
      let rec get_activity_level_input () =
        print_string "What is your activity level (sedentary/lightly_active/moderately_active/very_active/extra_active)? ";
        let activity_level = parse_input (read_line ()) in
        match activity_level with
        | ["sedentary"] | ["s"] | ["lightly_active"] | ["lightly"; "active"] | ["lightly"] | ["l"] 
        | ["moderately_active"] | ["moderately"; "active"] | ["moderately"] | ["m"] 
        | ["very_active"] | ["very"; "active"] | ["very"] | ["v"] | ["extra_active"] | ["extra"; "active"] | ["extra"] | ["e"] ->
          user.activity_level <- List.hd activity_level;
          let rec get_goal_input () =
            print_string "What is your weight goal (lose_weight/gain_weight/maintain_weight)? ";
            let goal = parse_input (read_line ()) in
            match goal with
            | ["lose_weight"] | ["lose"; "weight"] | ["l"] | ["lose"]
            | ["gain_weight"]| ["gain"; "weight"] | ["g"]  | ["gain"]
            | ["maintain_weight"] | ["maintain"; "weight"] |["m"] | ["maintain"] ->
              user.goal <- List.hd goal;
              let calorie_needs = calculate_calorie_needs gender weight height age activity_level goal in
              user.calories <- calorie_needs;
              Printf.printf "Your estimated daily calorie needs are: %.2f\n" calorie_needs;
              calorie_needs
            | _ ->
              print_string "Invalid goal. Please enter your weight goal.\n";
              get_goal_input ()
          in
          get_goal_input ()
        | _ ->
          print_string "Invalid activity level. Please enter your activity level.\n";
          get_activity_level_input ()
      in
      get_activity_level_input ()
    | _ ->
      print_string "Invalid gender. Please enter your gender.\n";
      get_gender_input ()
  in
  get_gender_input ()
;;

let rec user_input (u : user) () =
  Printf.printf "\nYou: ";
  let input = read_line () in
  match input with
  | "bye" | "goodbye" | "see you" -> Printf.printf "Goodbye! Have a nice day.\n"
  | _ ->
    let response = generate_response input u in
    Printf.printf "\nBot: ";
    print_endline response;
    user_input u ()
;;

print_newline();;
print_string "\nWe need your information first\n";;
get_information current_user ();;
greet_user current_user ();;
user_input current_user ();;