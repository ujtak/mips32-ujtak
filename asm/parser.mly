%{
  open Syntax
%}

/* (* Directives *) */
%token GLOBL TEXT DATA WORD

/* (* Core instructions *) */
%token ADD ADDI ADDIU ADDU
%token BEQ BNE
%token J JAL JR
%token LBU LHU LL LUI LW
%token NOR OR ORI
%token SLT SLTI SLTIU SLTU
%token SLL SRL
%token SB SC SH SW
%token SUB SUBU
%token SYSCALL

/* (* Arithmetic core instructions *) */
%token BCLT BCLF
%token DIV DIVU
%token ADDS ADDD
%token CEQS CLTS CLES CEQD CLTD CLED
%token DIVS DIVD
%token MULS MULD
%token SUBS SUBD
%token LWCL LDCL
%token MFHI MFLO MFC0
%token MULT MULTU
%token SRA
%token SWCL SDCL

/* (* Pseudo instructions *) */
%token BLT BGT BLE BGE
%token LI
%token MOVE
