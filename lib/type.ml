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
  | Nat -> "N"
  | Cons (t1, t2) -> sprintf "(%s :: %s)" (show_ptype t1) (show_ptype t2)
  | Nil -> "[]"
;;

let rec pp_ptype fmt = function
  | Var v -> fprintf fmt "%s" v
  | Arr (t1, t2) -> fprintf fmt "%a -> %a" pp_ptype t1 pp_ptype t2
  | Nat -> fprintf fmt "N"
  | Cons (t1, t2) -> fprintf fmt "%a :: %a" pp_ptype t1 pp_ptype t2
  | Nil -> fprintf fmt "[]"
;;

let rec equal_ptype t1 t2 =
  match t1, t2 with
  | Var v1, Var v2 -> v1 = v2
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
  match term with
  | Eval.Var var -> [(typ, search_type var env)]
  | Eval.Abs (var, term) ->
      let ta = Var (new_type ()) in
      let tr = Var (new_type ()) in
      (Arr (ta, tr), typ) :: generate_equa term tr ((var, ta) :: env)
  | Eval.App (tfun, targ) ->
      let ta = Var (new_type ()) in
      let tr = typ in
      generate_equa tfun (Arr (ta, tr)) env @ generate_equa targ ta env
  | Eval.Int _ -> [(typ, Nat)]
  | _ -> []
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
    | Ok eqs -> unify eqs (pred timeout)
    | Error msg -> Error msg
;;

let infer_type term env =
  let res_type = Var (new_type ()) in
  let equations = generate_equa term res_type env in
  match unify equations 100 with
  | Ok [] -> Some res_type
  | Ok equations -> List.assoc_opt res_type equations
  | Error msg -> print_endline msg; None
;;

(* let empty_env = [] *)
(**)
(* let rec apply_subst env t : ptype = *)
(*   match t with *)
(*   | Var x -> *)
(*       (match List.assoc_opt x env with *)
(*        | Some t' -> t' *)
(*        | None -> Var x) *)
(*   | Arr (t1, t2) -> Arr (apply_subst env t1, apply_subst env t2) *)
(*   | Nat -> Nat *)
(*   | Cons (t1, t2) -> Cons (apply_subst env t1, apply_subst env t2) *)
(*   | Nil -> Nil *)
(**)
(* let compose_subst s1 s2 = *)
(*   let s2' = List.map (fun (x, t) -> (x, apply_subst s1 t)) s2 in *)
(*   s2' @ s1 *)
(**)
(* let rec unify t1 t2 : env option = *)
(*   match t1, t2 with *)
(*   | Nat, Nat -> Some empty_env *)
(*   | Nil, Nil -> Some empty_env *)
(*   | Var x, t | t, Var x -> *)
(*       if occur_check x t then None *)
(*       else Some [(x, t)] *)
(*   | Arr (t1, t2), Arr (t1', t2') -> *)
(*       (match unify t1 t1' with *)
(*        | None -> None *)
(*        | Some s1 -> *)
(*            let t2s = apply_subst s1 t2 in *)
(*            let t2s' = apply_subst s1 t2' in *)
(*            match unify t2s t2s' with *)
(*            | None -> None *)
(*            | Some s2 -> Some (compose_subst s2 s1)) *)
(*   | Cons (h1, t1), Cons (h2, t2) -> *)
(*       (match unify h1 h2 with *)
(*        | None -> None *)
(*        | Some s1 -> *)
(*            let t1s = apply_subst s1 t1 in *)
(*            let t2s = apply_subst s1 t2 in *)
(*            match unify t1s t2s with *)
(*            | None -> None *)
(*            | Some s2 -> Some (compose_subst s2 s1)) *)
(*   | _ -> None *)
(**)
(* let rec generate_constraints term env : (ptype * env) option = *)
(*   match term with *)
(*   | Eval.Int _ -> Some (Nat, empty_env) *)
(*   | Eval.Var x -> *)
(*       (try Some (search_type x env, empty_env) *)
(*        with VarNotFound -> None) *)
(*   | Eval.Abs (x, body) -> *)
(*       let arg_type = Var (new_type ()) in *)
(*       let new_env = (x, arg_type) :: env in *)
(*       (match generate_constraints body new_env with *)
(*        | Some (body_type, s) -> *)
(*            Some (Arr (apply_subst s arg_type, body_type), s) *)
(*        | None -> None) *)
(*   | Eval.App (fun_term, arg_term) -> *)
(*       (match generate_constraints fun_term env with *)
(*        | Some (fun_type, s1) -> *)
(*            (match generate_constraints arg_term env with *)
(*             | Some (arg_type, s2) -> *)
(*                 let result_type = Var (new_type ()) in *)
(*                 let s1s2 = compose_subst s1 s2 in *)
(*                 let fun_type' = apply_subst s1s2 fun_type in *)
(*                 let arg_type' = apply_subst s1s2 arg_type in *)
(*                 (match unify fun_type' (Arr (arg_type', result_type)) with *)
(*                  | Some s3 -> *)
(*                      let final_subst = compose_subst s3 s1s2 in *)
(*                      Some (apply_subst final_subst result_type, final_subst) *)
(*                  | None -> None) *)
(*             | None -> None) *)
(*        | None -> None) *)
(*   | _ -> None *)
(**)
(* let infer_type term env = *)
(*   match generate_constraints term env with *)
(*   | Some (typ, _) -> Some typ *)
(*   | None -> None *)
(* ;; *)
