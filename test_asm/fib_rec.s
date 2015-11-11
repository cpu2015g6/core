limm r1, 0
or r8, r1, rFF
j r2, 5
out r8, 0
limm r8, 1
add r1, r1, r8
j rFF, -5

fib:
limm r9, 2
cmp r9, r8, r9
limm r1E, ret_1
jrlt rFF, r9, r1E
limm r9, 1
stw r3, r2, rFF
add r3, r3, r9
stw r3, r8, rFF
add r3, r3, r9
sub r8, r8, r9
j r2, 65526
limm r9, 1
sub r9, r3, r9
ldw rA, r9, rFF
stw r9, r8, rFF
limm r9, 2
sub r8, rA, r9
j r2, 65519
limm r9, 1
sub r3, r3, r9
ldw rA, r3, rFF
add r8, r8, rA
sub r3, r3, r9
ldw r2, r3, rFF
jr rFF, r2, rFF

ret_1:
	limm r8, 1
	jr rFF, r2, rFF
