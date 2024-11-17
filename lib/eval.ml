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

  | IfZero (c, t, e) -> sprintf "ifz %s then %s else %s" (show_pterm_simp c) (show_pterm_simp t) (show_pterm_simp e)
  | IfEmpty (c, t, e) -> sprintf "ife %s then %s else %s" (show_pterm_simp c) (show_pterm_simp t) (show_pterm_simp e)
  | Fix t -> sprintf "fix %s" (show_pterm_simp t)
  | Let (id, t1, t2) -> sprintf "let %s = %s in \n\t%s" id (show_pterm_simp t1) (show_pterm_simp t2)
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
    | Let (id, t1, t2) -> (sprintf "let %s = %s in \n\t%s" id (aux t1 false) (aux t2 false)) |> handle_abs was_abs
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
;;

let rec subst id to_put = function
  | Var v -> if v = id then to_put else Var v
  | Abs (v, t) -> Abs (v, subst id to_put t)
  | App (t1, t2) -> App (subst id to_put t1, subst id to_put t2)

  | Int _ as n -> n
  | Add (t1, t2) -> Add (subst id to_put t1, subst id to_put t2)
  | Sub (t1, t2) -> Sub (subst id to_put t1, subst id to_put t2)
  | Mul (t1, t2) -> Mul (subst id to_put t1, subst id to_put t2)

  | TList terms -> TList (List.map (fun x -> subst id to_put x) terms)
  | Cons (h, t) -> Cons (subst id to_put h, subst id to_put t)
  | Nil -> Nil
  | Hd l -> Hd (subst id to_put l)
  | Tl l -> Tl (subst id to_put l)

  | IfZero (cond, tthen, telse) -> IfZero ((subst id to_put cond), (subst id to_put tthen), (subst id to_put telse))
  | IfEmpty (cond, tthen, telse) -> IfEmpty ((subst id to_put cond), (subst id to_put tthen), (subst id to_put telse))
  | Fix t -> Fix (subst id to_put t)
  | Let (id, teq, tin) -> Let (id, subst id to_put teq, subst id to_put tin)
;;

let rec is_value = function
  | App (Var _, n) -> is_value n
  | App (_, _) -> false
  | _ -> true
;;

let rec ltr_ctb_step t =
  print_endline (show_pterm t);
  let step_lr left right =
    match ltr_ctb_step left with
    | Some t -> Some (t, right)
    | None -> match ltr_ctb_step right with
      | Some t  -> Some (left, t)
      | None -> None
  in
  match t with
  | Abs (x, t) -> Option.map (fun t' -> Abs (x, t')) (ltr_ctb_step t)
  | App (Abs (x, t), targ) when is_value targ -> Some (subst x targ t)
  | App (tfun, targ) -> Option.map (fun (l, r) -> App (l, r)) (step_lr tfun targ)

  | Var _ -> None
  | Int _ -> None

  | Add (Int x, Int y) -> Some (Int (x + y))
  | Add (t1, t2) -> Option.map (fun (l, r) -> Add (l, r)) (step_lr t1 t2)

  | Sub (Int x, Int y) -> Some (Int (x - y))
  | Sub (t1, t2) -> Option.map (fun (l, r) -> Sub (l, r)) (step_lr t1 t2)

  | Mul (Int x, Int y) -> Some (Int (x * y))
  | Mul (t1, t2) -> Option.map (fun (l, r) -> Mul (l, r)) (step_lr t1 t2)

  | TList [] -> None
  | TList terms ->
    (match (List.hd terms) |> ltr_ctb_step with
     | None -> None
     (* we assume that if one element is reduceable then all of the elements
        of the list are reduceable as they are all of the same type *)
     | _ -> Some (TList (List.map (fun x -> x |> ltr_ctb_step |> Option.get) terms)))

  | Cons (h, t) ->
    (match step_lr h t with
     | Some (l, r) -> Some (Cons (l, r))
     | None -> match t with
       | Nil -> Some (TList [h])
       | TList l -> Some (TList (h :: l))
       | _ -> None)

  | Nil -> Some (TList [])

  | Hd (TList []) -> None
  | Hd (TList (h :: _)) -> Some h
  | Hd l -> Option.map (fun t -> Hd t) (ltr_ctb_step l)

  | Tl (TList []) -> None
  | Tl (TList (_ :: tl)) -> Some (TList tl)
  | Tl l -> Option.map (fun t -> Tl t) (ltr_ctb_step l)

  | IfZero (cond, tthen, telse) ->
    (match ltr_ctb_step cond with
     | Some t -> Some (IfZero (t, tthen, telse))
     | None -> match cond with
       | Int 0 -> Some tthen
       | Int _ -> Some telse
       | _ -> None)

  | IfEmpty (cond, tthen, telse) ->
    (match ltr_ctb_step cond with
     | Some t -> Some (IfEmpty (t, tthen, telse))
     | None -> match cond with
       | TList [] -> Some tthen
       | TList _ -> Some telse
       | _ -> None)

  | Fix (Abs (x, tbody)) ->
    let t = subst x (Fix (Abs (x, tbody))) tbody in 
    Some (alphaconv t)
  | Fix t -> Option.map (fun t -> Fix t) (ltr_ctb_step t)

  | Let (id, te, tin) ->
    (match ltr_ctb_step te with
     | Some t -> Some (Let (id, t, tin))
     | None -> Some (subst id te tin))
;;

let ltr_cbv_norm t =
  let rec aux t =
    match ltr_ctb_step t with
    | Some x -> aux x
    | None -> t
  in aux (alphaconv t)
;;

let rec ltr_cbv_norm_timeout n t =
  if n > 0
  then match ltr_ctb_step t with
    | Some x -> ltr_cbv_norm_timeout (n - 1) x
    | None -> t
  else t
;;

