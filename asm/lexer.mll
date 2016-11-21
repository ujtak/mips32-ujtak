{
  open Parser
  let directives = Hashtbl.create 1000
  let instructions = Hashtbl.create 1000
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add directives kwd tok)
    [
      ".globl", GLOBL;
      ".text", TEXT;
      ".data", DATA;
      ".word", WORD;
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
    ]
}

rule token = parse
