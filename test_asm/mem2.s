limm r1, 0
limm r2, 0
limm r3, 1
stw r1, r2, rFF
ldw r4, r1, rFF
stw r1, r3, rFF
ldw r5, r1, rFF
out r2, 0
out r3, 0
out r4, 0
out r5, 0
limm r4, 2
add r2, r2, r4
add r3, r3, r4
limm r4, 1
add r1, r1, r4
j rFF, -13
