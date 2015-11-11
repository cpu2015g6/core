.PHONY: all clean
all: fib_rec.dat infinite_pohe.dat loopback.dat mem.dat fib_loop.dat fpu.dat loop.dat

%.dat: %.s
	../../simulator/asm.py $^

clean:
	rm -f *.dat *.com
