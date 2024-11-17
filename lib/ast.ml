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

