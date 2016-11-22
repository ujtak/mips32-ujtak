{
  open Parser
  let space = [' ' '\t' '\n' '\r']
  let digit = ['0'-'9']
  let lower = ['a'-'z']
  let upper = ['A'-'Z']
  let directives = Hashtbl.create 1000
  let instructions = Hashtbl.create 1000
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add directives kwd tok)
    [
      "globl", GLOBL;
      "text", TEXT;
      "data", DATA;
      "word", WORD;
    ]
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add instructions kwd tok)
    [
      "add", ADD;
      "addi", ADDI;
      "addiu", ADDIU;
      "addu", ADDU;
      "beq", BEQ;
      "bne", BNE;
      "j", J;
      "jal", JAL;
      "JR", JR;
      "lbu", LBU;
      "lhu", LHU;
      "ll", LL;
      "lui", LUI;
      "lw", LW;
      "nor", NOR;
      "or", OR;
      "ori", ORI;
      "slt", SLT;
      "slti", SLTI;
      "sltiu", SLTIU;
      "sltu", SLTU;
      "sll", SLL;
      "srl", SRL;
      "sb", SB;
      "sc", SC;
      "sh", SH;
      "sw", SW;
      "sub", SUB;
      "subu", SUBU;
      "syscall", SYSCALL;
      "bclt", BCLT;
      "bclf", BCLF;
      "div", DIV;
      "divu", DIVU;
      "adds", ADDS;
      "addd", ADDD;
      "ceqs", CEQS;
      "clts", CLTS;
      "cles", CLES;
      "ceqd", CEQD;
      "cltd", CLTD;
      "cled", CLED;
      "divs", DIVS;
      "divd", DIVD;
      "muls", MULS;
      "muld", MULD;
      "subs", SUBS;
      "subd", SUBD;
      "lwcl", LWCL;
      "ldcl", LDCL;
      "mfhi", MFHI;
      "mflo", MFLO;
      "mfc0", MFC0;
      "mult", MULT;
      "mult", MULTU;
      "sra", SRA;
      "swcl", SWCL;
      "sdcl", SDCL;
      "blt", BLT;
      "bgt", BGT;
      "ble", BLE;
      "bge", BGE;
      "li", LI;
      "move", MOVE;
    ]
}

rule token = parse
  | space+ { token lexbuf }
  | "#*" { c
  | eof { exit 0 }
