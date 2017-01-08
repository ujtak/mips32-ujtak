# Test instructions for [add, sub, and, or, lw, sw, beq]
# x1 = 25, x2 = 35
# x = (x1 - x2) && (x1 + x2)
# y = (x2 - x1) || (x2 + x1)

# Python test:
# >>> x1 = 25; x2 = 35
# >>> x = (x1 - x2) and (x1 + x2)
# >>> x
# 60
# >>> y = (x2 - x1) or (x2 + x1)
# >>> y
# 10

main:
  lw $t0, 0($zero)
  lw $t1, 0($t0)
  lw $t2, 4($t0)
  lw $t7, 8($t0)
  sub $t3, $t1, $t2
  add $t4, $t1, $t2
  and $t5, $t3, $t4
  # will skip to skip:
  beq $t5, $t7, skip
  add $t5, $t5, $t5
  add $t5, $t5, $t5

skip:
  sw $t5, 4($zero)
  sub $t3, $t2, $t1
  add $t4, $t2, $t1
  or $t5, $t3, $t4
  # won't skip to dumb:
  beq $t5, $t7, dumb
  sw $t5, 8($zero)

dumb:
  add $t5, $t5, $t5
  add $t5, $t5, $t5

