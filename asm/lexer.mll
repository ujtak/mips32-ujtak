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
      "sub", SUB;
      "li", LI;
      "lw", LW;
      "syscall", SYSCALL;
    ]
}

rule token = parse
