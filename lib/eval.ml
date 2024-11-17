open Format

type pterm =
  | Var of string
  | App of pterm * pterm
  | Abs of string * pterm

  | Int of int
  | Add of pterm * pterm
  | Sub of pterm * pterm
  | Mul of pterm * pterm

  | TList of pterm list
  | Cons of pterm * pterm
  | Nil
  | Hd of pterm
  | Tl of pterm

  | IfZero of pterm * pterm * pterm
  | IfEmpty of pterm * pterm * pterm
  | Fix of pterm
  | Let of string * pterm * pterm

  | Region of int
  | Unit

  | Ref of pterm
  | Deref of pterm
  | Assign of pterm * pterm


let rec show_pterm_simp t =
  match t with
  | Var id -> id
  | Abs (id, t) -> sprintf "\\%s . %s" id (show_pterm_simp t)
  | App (t1, t2) -> sprintf "(%s %s)" (show_pterm_simp t1) (show_pterm_simp t2)

  | Int n -> string_of_int n
  | Add (t1, t2) -> sprintf "(%s + %s)" (show_pterm_simp t1) (show_pterm_simp t2)
  | Sub (t1, t2) -> sprintf "(%s - %s)" (show_pterm_simp t1) (show_pterm_simp t2)
  | Mul (t1, t2) -> sprintf "(%s * %s)" (show_pterm_simp t1) (show_pterm_simp t2)

  | TList l -> sprintf "[%s]" (String.concat "; " (List.map show_pterm_simp l))
  | Cons (h, t) -> sprintf "(%s :: %s)" (show_pterm_simp h) (show_pterm_simp t)
  | Nil -> "[]"
  | Hd l -> sprintf "hd %s" (show_pterm_simp l)
  | Tl l -> sprintf "tl %s" (show_pterm_simp l)

  | IfZero (c, t, e) -> sprintf "ifz (%s) (%s) (%s)" (show_pterm_simp c) (show_pterm_simp t) (show_pterm_simp e)
  | IfEmpty (c, t, e) -> sprintf "ife (%s), (%s), (%s)" (show_pterm_simp c) (show_pterm_simp t) (show_pterm_simp e)
  | Fix t -> sprintf "fix (%s)" (show_pterm_simp t)
  | Let (id, t1, t2) -> sprintf "let (%s, %s, %s)" id (show_pterm_simp t1) (show_pterm_simp t2)

  | Region i -> sprintf "{{%d}}" i
  | Unit -> "unit"

  | Ref t -> sprintf "ref (%s)" (show_pterm_simp t)
  | Deref t -> sprintf "deref (%s)" (show_pterm_simp t)
  | Assign (t1, t2) -> sprintf "assign (%s, %s)" (show_pterm_simp t1) (show_pterm_simp t2)
;;

let show_pterm t =
  let handle_abs was_abs str = 
    if was_abs then " . " ^ str else str
  in
  let rec aux t was_abs =
    match t with
    | Var id when was_abs -> sprintf " . %s" id
    | Var id -> id
    | App (t1, t2) when was_abs -> sprintf ". (%s %s)" (aux t1 false) (aux t2 false)
    | App (t1, t2) -> sprintf "(%s %s)" (aux t1 false) (aux t2 false)
    | Abs (id, t) when was_abs -> sprintf "%s%s" id (aux t true)
    | Abs (id, t) -> sprintf "(\\%s%s)" id (aux t true)

    | Int n -> string_of_int n |> handle_abs was_abs
    | Add (t1, t2) -> (sprintf "(%s + %s)" (aux t1 false) (aux t2 false)) |> handle_abs was_abs
    | Sub (t1, t2) -> (sprintf "(%s - %s)" (aux t1 false) (aux t2 false)) |> handle_abs was_abs
    | Mul (t1, t2) -> (sprintf "(%s * %s)" (aux t1 false) (aux t2 false)) |> handle_abs was_abs

    | TList l -> (sprintf "[%s]" (String.concat "; " (List.map (fun x -> aux x false) l))) |> handle_abs was_abs
    | Cons (h, t) -> (sprintf "(%s :: %s)" (aux h false) (aux t false)) |> handle_abs was_abs
    | Nil -> handle_abs was_abs "[]"
    | Hd l -> (sprintf "hd %s" (aux l false)) |> handle_abs was_abs
    | Tl l -> (sprintf "tl %s" (aux l false)) |> handle_abs was_abs

    | IfZero (c, t, e) -> (sprintf "ifz %s then %s else %s" (aux c false) (aux t false) (aux e false)) |> handle_abs was_abs
    | IfEmpty (c, t, e) -> (sprintf "ife %s then %s else %s" (aux c false) (aux t false) (aux e false)) |> handle_abs was_abs
    | Fix t -> (sprintf "fix %s" (aux t false)) |> handle_abs was_abs
    | Let (id, t1, t2) -> (sprintf "let %s = %s in %s" id (aux t1 false) (aux t2 false)) |> handle_abs was_abs

    | Region i -> sprintf "{{%d}}" i |>handle_abs was_abs
    | Unit -> "()" |> handle_abs was_abs

    | Ref t -> (sprintf "ref %s" (aux t false)) |> handle_abs was_abs
    | Deref t -> (sprintf "!%s" (aux t false)) |> handle_abs was_abs
    | Assign (t1, t2) -> (sprintf "%s := %s" (aux t1 false) (aux t2 false)) |> handle_abs was_abs
  in
  aux t false
;;

let pp_pterm fmt t = fprintf fmt "%s" (show_pterm t)
let pp_pterm_simp fmt t = fprintf fmt "%s" (show_pterm_simp t)

let rec equal_pterm t1 t2 =
  match t1, t2 with
  | Var _, Var _ -> true
  | App (lt1, lt2), App (rt1, rt2) -> equal_pterm lt1 rt1 && equal_pterm lt2 rt2
  | Abs (_, t1), Abs (_, t2) -> equal_pterm t1 t2

  | Int n1, Int n2 -> n1 = n2
  | Add (lt1, lt2), Add (rt1, rt2) -> (equal_pterm lt1 rt1 && equal_pterm lt2 rt2) || (equal_pterm lt1 rt2 && equal_pterm lt2 rt1)
  | Sub (lt1, lt2), Sub (rt1, rt2) -> equal_pterm lt1 rt1 && equal_pterm lt2 rt2

  | TList l1, TList l2 -> List.length l1 = List.length l2 && List.for_all2 equal_pterm l1 l2
  | Cons (lh1, lt1), Cons (lh2, lt2) -> equal_pterm lh1 lh2 && equal_pterm lt1 lt2
  | Nil, Nil -> true
  | Hd l1, Hd l2 -> equal_pterm l1 l2
  | Tl l1, Tl l2 -> equal_pterm l1 l2

  | IfZero (c1, t1, e1), IfZero (c2, t2, e2) -> equal_pterm c1 c2 && equal_pterm t1 t2 && equal_pterm e1 e2
  | IfEmpty (c1, t1, e1), IfEmpty (c2, t2, e2) -> equal_pterm c1 c2 && equal_pterm t1 t2 && equal_pterm e1 e2
  | Fix t1, Fix t2 -> equal_pterm t1 t2
  | Let (_, t1, tin1), Let (_, t2, tin2) -> equal_pterm t1 t2 && equal_pterm tin1 tin2

  | Region i1, Region i2 -> i1 = i2
  | Unit, Unit -> true

  | Ref t1, Ref t2 -> equal_pterm t1 t2
  | Deref t1, Deref t2 -> equal_pterm t1 t2
  | Assign (t1, t2), Assign (t3, t4) -> equal_pterm t1 t3 && equal_pterm t2 t4
  | _ -> false
;;

(* created by chatgpt *)
let create_var_generator () =
  let counter = ref 1 in
  let current_char = ref 'a' in
  fun () ->
    let result =
      if !counter = 1 then
        Printf.sprintf "%c" !current_char
      else
        Printf.sprintf "%c%d" !current_char !counter
    in
    (* Move to the next character *)
    if !current_char = 'z' then (
      current_char := 'a';
      incr counter
    ) else
      current_char := Char.chr (Char.code !current_char + 1);
    result
;;
let new_var = create_var_generator ()
let region_counter = ref 0
let next_region () = incr region_counter; !region_counter

let rec alphaconv term =
  let rec aux cur_var repl_var = function
    | Var v -> if cur_var = v then Var repl_var else Var v
    | Abs (id, t) ->
      let nvar = new_var () in
      let first = aux id nvar t in
      Abs (nvar, aux cur_var repl_var first)
    | App (t1, t2) -> App (aux cur_var repl_var t1, aux cur_var repl_var t2)

    | Int _ as n -> n
    | Add (t1, t2) -> Add (aux cur_var repl_var t1, aux cur_var repl_var t2)
    | Sub (t1, t2) -> Sub (aux cur_var repl_var t1, aux cur_var repl_var t2)
    | Mul (t1, t2) -> Mul (aux cur_var repl_var t1, aux cur_var repl_var t2)

    | TList terms -> TList (List.map (aux cur_var repl_var) terms)
    | Cons (h, t) -> Cons (aux cur_var repl_var h, aux cur_var repl_var t)
    | Nil -> Nil
    | Hd l -> Hd (aux cur_var repl_var l)
    | Tl l -> Tl (aux cur_var repl_var l)

    | IfZero (cond, t, e) -> IfZero (aux cur_var repl_var cond, aux cur_var repl_var t, aux cur_var repl_var e)
    | IfEmpty (cond, t, e) -> IfEmpty (aux cur_var repl_var cond, aux cur_var repl_var t, aux cur_var repl_var e)
    | Fix t -> Fix (aux cur_var repl_var t)
    | Let (id, t1, t2) -> Let (id, aux cur_var repl_var t1, aux cur_var repl_var t2)

    | Region _ as r -> r
    | Unit -> Unit

    | Ref t -> Ref (aux cur_var repl_var t)
    | Deref t -> Deref (aux cur_var repl_var t)
    | Assign (t1, t2) -> Assign (aux cur_var repl_var t1, aux cur_var repl_var t2)
  in
  match term with
  | Var _ -> Var (new_var ())
  | Abs (id, t) ->
    let nvar = new_var () in
    Abs (nvar, aux id nvar t)
  | App (t1, t2) ->
    (match t1, t2 with
     | Var id1, Var id2 when id1 = id2 -> let nvar = new_var () in App (Var nvar, Var nvar)
     | t1, t2 -> App (alphaconv t1, alphaconv t2))

  | Int _ -> term
  | Add (t1, t2) -> Add (alphaconv t1, alphaconv t2)
  | Sub (t1, t2) -> Sub (alphaconv t1, alphaconv t2)
  | Mul (t1, t2) -> Mul (alphaconv t1, alphaconv t2)

  | TList terms -> TList (List.map alphaconv terms)
  | Cons (h, t) -> Cons (alphaconv h, alphaconv t)
  | Nil -> Nil
  | Hd l -> Hd (alphaconv l)
  | Tl l -> Tl (alphaconv l)

  | IfZero (cond, tthen, telse) -> IfZero (alphaconv cond, alphaconv tthen, alphaconv telse)
  | IfEmpty (cond, tthen, telse) -> IfEmpty (alphaconv cond, alphaconv tthen, alphaconv telse)
  | Fix t -> Fix (alphaconv t)
  | Let (id, t1, t2) ->
    let nvar = new_var () in
    Let (nvar, aux id nvar t1, aux id nvar t2)

  | Region _ as r -> r
  | Unit -> Unit

  | Ref t -> Ref (alphaconv t)
  | Deref t -> Deref (alphaconv t)
  | Assign (t1, t2) -> Assign (alphaconv t1, alphaconv t2)
;;

let rec subst vid to_put = function
  | Var v -> if v = vid then to_put else Var v
  | Abs (v, t) -> Abs (v, subst vid to_put t)
  | App (t1, t2) -> App (subst vid to_put t1, subst vid to_put t2)

  | Int _ as n -> n
  | Add (t1, t2) -> Add (subst vid to_put t1, subst vid to_put t2)
  | Sub (t1, t2) -> Sub (subst vid to_put t1, subst vid to_put t2)
  | Mul (t1, t2) -> Mul (subst vid to_put t1, subst vid to_put t2)

  | TList terms -> TList (List.map (fun x -> subst vid to_put x) terms)
  | Cons (h, t) -> Cons (subst vid to_put h, subst vid to_put t)
  | Nil -> Nil
  | Hd l -> Hd (subst vid to_put l)
  | Tl l -> Tl (subst vid to_put l)

  | IfZero (cond, tthen, telse) -> IfZero ((subst vid to_put cond), (subst vid to_put tthen), (subst vid to_put telse))
  | IfEmpty (cond, tthen, telse) -> IfEmpty ((subst vid to_put cond), (subst vid to_put tthen), (subst vid to_put telse))
  | Fix t -> Fix (subst vid to_put t)
  | Let (lid, teq, tin) -> Let (lid, subst vid to_put teq, subst vid to_put tin)

  | Region _ as r -> r
  | Unit -> Unit

  | Ref t -> Ref (subst vid to_put t)
  | Deref t -> Deref (subst vid to_put t)
  | Assign (t1, t2) -> Assign (subst vid to_put t1, subst vid to_put t2)
;;

let rec is_value = function
  | App (Var _, n) -> is_value n
  | App (_, _) -> false
  | _ -> true
;;

let rec ltr_ctb_step (state: 'a list) t =
  let step_lr state left right =
    match ltr_ctb_step state left with
    | Some (nstate, t) -> Some (nstate, (t, right))
    | None -> match ltr_ctb_step state right with
      | Some (nstate, t)  -> Some (nstate, (left, t))
      | None -> None
  in
  match t with
  | Var _ | Int _ | Region _ | Unit -> None

  | Abs (x, t) ->
    (match ltr_ctb_step state t with
     | Some (nstate, t') -> Some (nstate, Abs (x, t'))
     | None -> None)

  | App (Abs (x, t), targ) when is_value targ -> Some (state, subst x targ t)
  | App (tfun, targ) ->
    (match step_lr state tfun targ with
     | Some (nstate, (l, r)) -> Some (nstate, App (l, r))
     | None -> None)


  | Add (Int x, Int y) -> Some (state, Int (x + y))
  | Add (t1, t2) -> Option.map (fun (nstate, (l, r)) -> (nstate, Add (l, r))) (step_lr state t1 t2)

  | Sub (Int x, Int y) -> Some (state, Int (x - y))
  | Sub (t1, t2) -> Option.map (fun (nstate, (l, r)) -> (nstate, Sub (l, r))) (step_lr state t1 t2)

  | Mul (Int x, Int y) -> Some (state, Int (x * y))
  | Mul (t1, t2) -> Option.map (fun (nstate, (l, r)) -> (nstate, Mul (l, r))) (step_lr state t1 t2)

  | TList [] -> None
  | TList terms ->
    (match (List.hd terms) |> ltr_ctb_step state with
     | None -> None
     (* we assume that if one element is reduceable then all of the elements
        of the list are reduceable as they are all of the same type *)
     | _ -> Some (state, TList (List.map (fun x -> x |> ltr_ctb_step state |> Option.get |> snd) terms)))

  | Cons (h, t) ->
    (match step_lr state h t with
     | Some (nstate, (l, r)) -> Some (nstate, Cons (l, r))
     | None -> match t with
       | Nil -> Some (state, TList [h])
       | TList l -> Some (state, TList (h :: l))
       | _ -> None)

  | Nil -> Some (state, TList [])

  | Hd l ->
    (match ltr_ctb_step state l with
     | Some (nstate, t) -> Some (nstate, Hd t)
     | None -> match l with
       |  TList (h :: _)-> Some (state, h)
       |  _ -> None)

  | Tl l ->
    (match ltr_ctb_step state l with
     | Some (nstate, t) -> Some (nstate, Tl t)
     | None -> match l with
       |  TList (_ :: tl)-> Some (state, TList tl)
       |  _ -> None)

  | IfZero (cond, tthen, telse) ->
    (match ltr_ctb_step state cond with
     | Some (nstate, t) -> Some (nstate, IfZero (t, tthen, telse))
     | None -> match cond with
       | Int 0 -> Some (state, tthen)
       | Int _ -> Some (state, telse)
       | _ -> None)

  | IfEmpty (cond, tthen, telse) ->
    (match ltr_ctb_step state cond with
     | Some (nstate, t) -> Some (nstate, IfEmpty (t, tthen, telse))
     | None -> match cond with
       | TList [] -> Some (state, tthen)
       | TList _ -> Some (state, telse)
       | _ -> None)

  | Fix (Abs (x, tbody)) ->
    let t = subst x (Fix (Abs (x, tbody))) tbody in 
    Some (state, alphaconv t)
  | Fix t -> Option.map (fun (nstate, t) -> (nstate, Fix t)) (ltr_ctb_step state t)

  | Let (id, te, tin) ->
    (match ltr_ctb_step state te with
     | Some (nstate, t) -> Some (nstate, Let (id, t, tin))
     (* "_" cannot a variable name  *)
     | None -> if id = "_" then Some (state, tin) else Some (state, subst id te tin))

  | Ref t when is_value t -> let i = next_region () in Some ((i, t)::state, Region i)
  | Ref t -> Option.map (fun (nstate, t) -> (nstate, Ref t)) (ltr_ctb_step state t)

  | Deref (Region i) -> Option.map (fun t -> (state, t)) (List.assoc_opt i state)
  | Deref t -> Option.map (fun (nstate, t) -> (nstate, Deref t)) (ltr_ctb_step state t)

  | Assign (t1, t2) ->
    (match step_lr state t1 t2 with
     | Some (nstate, (l, r)) -> Some (nstate, Assign (l, r))
     | None -> match t1 with
       | Region i -> Some ((i, t2)::(List.filter (fun (k, _) -> k <> i) state), Unit)
       | _ -> None)
;;

let ltr_cbv_norm t =
  region_counter := 0;
  let rec aux state t =
    match ltr_ctb_step state t with
    | Some (nstate, x) -> aux nstate x
    | None -> t
  in aux [] t
;;

let ltr_cbv_norm_timeout n state t =
  region_counter := 0;
  let rec aux n state t =
    if n > 0
    then match ltr_ctb_step state t with
      | Some (nstate, x) -> aux (n - 1) nstate x
      | None -> t
    else t
  in aux n state t
;;

