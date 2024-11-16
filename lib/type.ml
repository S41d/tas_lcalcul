open Format

type ptype =
  | Var of string
  | Arr of ptype * ptype
  | Nat
  | TList of ptype
  | Poly of string * ptype

let rec show_ptype = function
  | Var x -> x
  | Arr (t1, t2) -> sprintf "%s -> %s" (show_ptype t1) (show_ptype t2)
  | Nat -> "Nat"
  | TList t -> sprintf "%s list" (show_ptype t)
  | Poly (s, t) -> sprintf "/%s %s" s (show_ptype t)

let rec pp_ptype fmt = function
  | Var v -> fprintf fmt "%s" v
  | Arr (t1, t2) -> fprintf fmt "%a -> %a" pp_ptype t1 pp_ptype t2
  | Nat -> fprintf fmt "Nat"
  | TList t -> fprintf fmt "%a list" pp_ptype t
  | Poly (s, t) -> fprintf fmt "/%s %a" s pp_ptype t

let rec equal_ptype t1 t2 =
  match t1, t2 with
  | Var _, Var _ -> true
  | Arr (t1, t2), Arr (t1', t2') -> equal_ptype t1 t1' && equal_ptype t2 t2'
  | Nat, Nat -> true
  | TList t1, TList t2 -> equal_ptype t1 t2
  | Poly (s, t1), Poly (s', t2) -> s = s' && equal_ptype t1 t2
  | _ -> false

let counter_t = ref 0

let new_type () =
  counter_t := succ !counter_t;
  "T" ^ string_of_int !counter_t

type equa = (ptype * ptype) list
type env = (string * ptype) list

exception VarNotFound
exception Timeout

let rec search_type v env =
  match env with
  | [] -> raise VarNotFound
  | (v', typ) :: _ when v = v' -> typ
  | _ :: tl -> search_type v tl

let rec generate_equa term typ env =
  match term with
  | Eval.Var var -> [(typ, search_type var env)]

  | Eval.Abs (var, term) ->
    let ta = Var (new_type ()) in
    let tr = Var (new_type ()) in
    let tbody = generate_equa term tr ((var, ta) :: env) in
    tbody @ [(typ, Arr (ta, tr))]

  | Eval.App (tfun, targ) ->
    let ta = Var (new_type ()) in
    let tr = typ in
    generate_equa tfun (Arr (ta, tr)) env @ generate_equa targ ta env

  | Eval.Int _ -> [(typ, Nat)]

  | Eval.Add (t1, t2) | Eval.Sub (t1, t2) ->
    generate_equa t1 Nat env @ generate_equa t2 Nat env @ [(typ, Nat)]

  | Eval.TList l ->
    let typ_el = Var (new_type ()) in
    List.fold_left (fun acc x -> acc @ generate_equa x typ_el env) [] l @ [(typ, TList typ_el)]

  | Eval.Cons (h, t) ->
    let typ_el = Var (new_type ()) in
    generate_equa h typ_el env @ generate_equa t (TList typ_el) env @ [(typ, TList typ_el)]

  | Eval.Nil -> [(typ, TList (Var (new_type ())))]
  | Eval.Let (var, t1, t2) ->
    let tbody = infer_type env t1 in
    (match tbody with
    | Some tbody -> generate_equa t2 typ ((var, tbody) :: env)
    | None -> failwith "Cannot infer type")
  | _ -> []

and occur_check x t =
  let rec aux x = function
    | Var v -> x = v
    | Arr (t1, t2) -> aux x t1 || aux x t2
    | Nat -> false
    | TList t' -> aux x t'
    | Poly (_, t') -> aux x t'
  in aux x t

and subst x t = function
  | Var v -> if x = v then t else Var v
  | Arr (t1, t2) -> Arr (subst x t t1, subst x t t2)
  | Nat -> Nat
  | TList t' -> TList (subst x t t')
  | Poly (s, t') -> Poly (s, subst x t t')

and subst_global x t = function
  | [] -> []
  | (t1, t2)::tl -> ((subst x t t1), (subst x t t2))::(subst_global x t tl)


and unify_step equations =
  match equations with
  | [] -> Ok []
  | (t1, t2) :: rest when t1 = t2 -> Ok rest
  | (TList t1, TList t2) :: rest -> unify_step ((t1, t2) :: rest)
  | (Var x, t) :: rest when not (occur_check x t) -> Ok (subst_global x t rest)
  | (t, Var x) :: rest when not (occur_check x t) -> Ok (subst_global x t rest)
  | (Arr (t1, t2), Arr (t3, t4)) :: rest -> Ok ((t1, t3) :: (t2, t4) :: rest)
  | (t1, t2) :: _ -> Error ("Cannot unify " ^ show_ptype t1 ^ " with " ^ show_ptype t2)

and unify equations timeout =
  if timeout = 0 then Error "Timeout"
  else match unify_step equations with
    | Ok [] -> Ok []
    | Ok (t::[]) -> Ok [t]
    | Ok eqs -> unify eqs (pred timeout)
    | Error msg -> Error msg

and infer_type env term =
  let res_type = Var (new_type ()) in
  let equations = generate_equa term res_type env in
  match equations with
  | [] -> None
  | h::[] -> Some (snd h)
  | _ -> match unify equations 100 with
    | Ok [] -> Some res_type
    | Ok [(_, t2)] ->  Some t2
    | Ok equations -> List.assoc_opt res_type equations
    | Error msg -> print_endline msg; None

