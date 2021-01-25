.global _start
.rodata
hello: .asciz "Hello, World !\n"

.text
_start:
	la x6, hello
loop:
	lbu x7, 0(x6)
	sb x7, 0x100(x0) # 0x100 is an I/O port for stdout
	addi x6, x6, 1 # increment address by one
	bne x7, x0, loop # if not \0, continue loop
	j _start
