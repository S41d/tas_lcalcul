open Eval
open Format

type ptype =
  | Var of string
  | Arr of ptype * ptype
  | Nat

let rec show_ptype = function
  | Var x -> x
  | Arr (t1, t2) -> sprintf "(%s -> %s)" (show_ptype t1) (show_ptype t2)
  | Nat -> "N"
;;

let rec pp_ptype fmt = function
  | Var v -> fprintf fmt "%s" v
  | Arr (t1, t2) -> fprintf fmt "%a -> %a" pp_ptype t1 pp_ptype t2
  | Nat -> fprintf fmt "N"
;;

let counter_t = ref 0

let new_var_t () =
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
  match term with
  | Eval.Var var -> (search_type var env, typ) :: []
  | Abs (var, term) ->
    let ta = Var (new_var_t ()) in
    let tr = Var (new_var_t ()) in
    let nequa = generate_equa term tr ((var, ta) :: env) in
    (typ, Arr (ta, tr)) :: nequa
  | App (tfun, targ) ->
    let ta = Var (new_var_t ()) in
    let tf_t = generate_equa tfun (Arr (ta, typ)) env in
    let targ_t = generate_equa targ ta env in
    targ_t @ tf_t
  | _ -> []
;;

let occur_check x t =
  let rec aux x = function
    | Var v -> x = v
    | Arr (t1, t2) -> aux x t1 || aux x t2
    | Nat -> false
  in aux x t
;;

let rec subst x t = function
  | Var v -> if x = v then t else Var v
  | Arr (t1, t2) -> Arr (subst x t t1, subst x t t2)
  | Nat -> Nat
;;

let rec subst_global x t = function
  | [] -> []
  | (t1, t2)::tl -> ((subst x t t1), (subst x t t2))::(subst_global x t tl)
;;

let unify_step equations =
  match equations with
  | [] -> Ok []
  | eq::eqs -> match eq with
    | (t1, t2) when t1 = t2 -> Ok eqs
    | (Var x, t) when not (occur_check x t) -> Ok (subst_global x t equations)
    | (t, Var x) when not (occur_check x t) -> Ok (subst_global x t equations)
    | (Arr (ga, gr), Arr (da, dr)) -> Ok ((Arr (ga, da), Arr(gr, dr))::eqs)
    | _ -> Error ("Cannot unify " ^ show_ptype (fst eq) ^ " with " ^ show_ptype (snd eq))
;;

let rec unify equations timeout =
  if timeout = 0 then raise Timeout
  else
    match equations with
    | [] -> []
    | _ -> 
      match unify_step equations with
      | Ok eqs -> unify eqs (timeout - 1)
      | Error msg -> raise (Failure msg)
;;

let rec infer_type term env =
  let res_type = Var (new_var_t ()) in
  let equations = generate_equa term res_type env in
  match unify equations 100 with
  | equations ->
    List.fold_left
      (fun ty (t1, t2) ->
         match t1 with
         | Var x -> subst x t2 ty
         | _ -> ty)
      res_type
      equations
  | exception Failure msg -> failwith msg
;;
