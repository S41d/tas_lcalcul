open Format

type ptype =
  | Var of string
  | Arr of ptype * ptype
  | Nat
  | Cons of ptype * ptype
  | Nil

let rec show_ptype = function
  | Var x -> x
  | Arr (t1, t2) -> sprintf "(%s -> %s)" (show_ptype t1) (show_ptype t2)
  | Nat -> "Nat"
  | Cons (t1, t2) -> sprintf "(%s :: %s)" (show_ptype t1) (show_ptype t2)
  | Nil -> "[]"
;;

let rec pp_ptype fmt = function
  | Var v -> fprintf fmt "%s" v
  | Arr (t1, t2) -> fprintf fmt "%a -> %a" pp_ptype t1 pp_ptype t2
  | Nat -> fprintf fmt "Nat"
  | Cons (t1, t2) -> fprintf fmt "%a :: %a" pp_ptype t1 pp_ptype t2
  | Nil -> fprintf fmt "[]"
;;

let rec equal_ptype t1 t2 =
  match t1, t2 with
  | Var _, Var _ -> true
  | Arr (t1, t2), Arr (t1', t2') -> equal_ptype t1 t1' && equal_ptype t2 t2'
  | Nat, Nat -> true
  | Cons (t1, t2), Cons (t1', t2') -> equal_ptype t1 t1' && equal_ptype t2 t2'
  | Nil, Nil -> true
  | _ -> false
;;

let counter_t = ref 0

let new_type () =
  counter_t := succ !counter_t;
  "T" ^ string_of_int !counter_t
;;

type equa = (ptype * ptype) list
type env = (string * ptype) list

exception VarNotFound
exception Timeout

let rec search_type v env =
  match env with
  | [] -> raise VarNotFound
  | (v', typ) :: _ when v = v' -> typ
  | _ :: tl -> search_type v tl
;;

let rec generate_equa term typ env =
  print_endline "in generate_equa";
  print_endline (Eval.show_pterm term);
  List.iter (fun (v, t) -> printf "(%s : %s) ; " v (show_ptype t)) env;
  print_newline ();
  match term with
  | Eval.Var var -> [(typ, search_type var env)]
  | Eval.Abs (var, term) ->
    let ta = Var (new_type ()) in
    let tr = Var (new_type ()) in
    let tbody = generate_equa term tr ((var, ta) :: env) in
    List.iter (fun (t, t') -> printf "(%s = %s) ; " (show_ptype t) (show_ptype t')) tbody;
    print_newline ();
    tbody @ [(typ, Arr (ta, tr))]
  | Eval.App (tfun, targ) ->
    let ta = Var (new_type ()) in
    let tr = typ in
    generate_equa tfun (Arr (ta, tr)) env @ generate_equa targ ta env
  | Eval.Int _ -> [(typ, Nat)]
  | _ -> print_endline "in generate_equa _"; print_endline (Eval.show_pterm term); []
;;

let occur_check x t =
  let rec aux x = function
    | Var v -> x = v
    | Arr (t1, t2) -> aux x t1 || aux x t2
    | Nat -> false
    | Cons (t1, t2) -> aux x t1 || aux x t2
    | Nil -> false
  in aux x t
;;

let rec subst x t = function
  | Var v -> if x = v then t else Var v
  | Arr (t1, t2) -> Arr (subst x t t1, subst x t t2)
  | Nat -> Nat
  | Cons (t1, t2) -> Cons (subst x t t1, subst x t t2)
  | Nil -> Nil
;;

let rec subst_global x t = function
  | [] -> []
  | (t1, t2)::tl -> ((subst x t t1), (subst x t t2))::(subst_global x t tl)
;;

let unify_step equations =
  match equations with
  | [] -> Ok []
  | (t1, t2) :: rest when t1 = t2 -> Ok rest
  | (Var x, t) :: rest when not (occur_check x t) -> Ok (subst_global x t rest)
  | (t, Var x) :: rest when not (occur_check x t) -> Ok (subst_global x t rest)
  | (Arr (t1, t2), Arr (t3, t4)) :: rest -> Ok ((t1, t3) :: (t2, t4) :: rest)
  | (t1, t2) :: _ -> Error ("Cannot unify " ^ show_ptype t1 ^ " with " ^ show_ptype t2)
;;

let rec unify equations timeout =
  if timeout = 0 then Error "Timeout"
  else match unify_step equations with
    | Ok [] -> Ok []
    | Ok (t::[]) -> Ok [t]
    | Ok eqs -> unify eqs (pred timeout)
    | Error msg -> Error msg
;;

let infer_type env term =
  let res_type = Var (new_type ()) in
  let equations = generate_equa term res_type env in
  print_string "equations: ";
  List.iter (fun (t1, t2) -> printf "%s = %s ; " (show_ptype t1) (show_ptype t2)) equations;
  print_newline ();
  match equations with
  | [] -> None
  | h::[] -> Some (snd h)
  | _ -> match unify equations 100 with
    | Ok [] -> Some res_type
    | Ok [(t1, t2)] -> t1 |> show_ptype |> print_endline ; Some t2
    | Ok equations -> List.assoc_opt res_type equations
    | Error msg -> print_endline msg; None
;;

