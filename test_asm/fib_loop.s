limm r0, 0
limm r1, 0
limm r2, 1
limm r3, 1

loop:
limm r10, 1
add r1, r1, r10
cmp r4, r1, r0
limm r10, end
jrgte rFF, r4, r10
add r4, r2, r3
or r2, r3, rff
or r3, r4, rff
j rff, -8

end:
out r2, 0
limm r1, 1
add r0, r0, r1
j rff, -15
