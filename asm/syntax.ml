type id = string

type exp =
  | Var of int
  | ImmI of int
  | ImmF of float
  | Label of id
  | Com of inst

type program = of Exp of exp

type inst =
  | Add of exp * exp
  | Addi of exp * exp
  | Addiu of 
  | Addu of 
  | Beq of 
  | Bne of 
  | J of 
  | Jal of 
  | Jr of 
  | Lbu of 
  | Lhu of 
  | Ll of 
  | Lui of 
  | Lw of 
  | Nor of 
  | Or of 
  | Ori of 
  | Slt of 
  | Slti of 
  | Sltiu of 
  | Sltu of 
  | Sll of 
  | Srl of 
  | Sb of 
  | Sc of 
  | Sh of 
  | Sw of 
  | Sub of 
  | Subu of 
  | Syscall of 
  | Bclt of 
  | Bclf of 
  | Div of 
  | Divu of 
  | Adds of 
  | Addd of 
  | Ceqs of 
  | Clts of 
  | Cles of 
  | Ceqd of 
  | Cltd of 
  | Cled of 
  | Divs of 
  | Divd of 
  | Muls of 
  | Muld of 
  | Subs of 
  | Subd of 
  | Lwcl of 
  | Ldcl of 
  | Mfhi of 
  | Mflo of 
  | Mfc0 of 
  | Mult of 
  | Mult of 
  | Sra of 
  | Swcl of 
  | Sdcl of 
  | Blt of 
  | Bgt of 
  | Ble of 
  | Bge of 
  | Li of 
  | Move of 
