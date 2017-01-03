    lw    $t0,  0($s1)
    addi  $t1,  $zero, 0
loop:
    add   $t1,  $t1,   1
    beq   $t1,  $s2,   done
    sll   $t2,  $t2,   2
    add   $t2,  $t2,   $s1
    lw    $t3,  0($t2)
    slt   $t4,  $t0,   $t3
    beq   $t4,  $zero, loop
    addi  $t0,  $t3,   0
    j     loop
done:
