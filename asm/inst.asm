# foldl1 (+) [0..10]

main:
  # $t0: base pointer, $t1: 4
  lw $t0, 0($zero)
  lw $t1, 4($zero)
  lw $t7, 8($zero)
  lw $t2, 0($t0)
  add $t0, $t0, $t1
  loop:
    lw $t3, 0($t0)
    add $t2, $t2, $t3
    add $t0, $t0, $t1
    beq $t2, $t7, done
    j loop
  done:
    sw $t2, 12($zero)

