limm r2, 0
stw r2, r2, rFF
ldw r1, r2, rFF
out r1, 0
limm r1, 1
add r2, r2, r1
j rFF, -5
