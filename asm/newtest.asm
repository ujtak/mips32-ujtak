    lw    $t0,  0($s1)
loop:
    add   $t1,  $t1,   1
    beq   $t1,  $s2,   done
    add   $t2,  $t2,   $s1
    lw    $t3,  0($t2)
    beq   $t4,  $zero, loop
    j     loop
done:
