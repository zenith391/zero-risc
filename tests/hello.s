.global _start

_start:
	la x6, hello
loop:
	lbu x7, 0(x6)
	sb x7, 0x100(x0) # 0x100 is an I/O port for stdout
	addi x6, x6, 1
	bne x7, x0, loop
end:
	j end

hello:
	.asciz "Hello, World !\n"
