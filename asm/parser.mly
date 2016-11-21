%{
  open Syntax
%}

/* (* Directives *) */
%token GLOBL TEXT DATA WORD

/* (* Instructions *) */
%token ADD SUB ADD OR NOR
%token LI LW
%token SYSCALL
