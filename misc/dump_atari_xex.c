#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

unsigned expect8 (unsigned char val)
{
	int tmp = getchar();
	if (val != tmp) {
		printf("Expected %02X, read %i\n", val, tmp);
		exit(1);
	}
	// printf("got expected %02X\n", val);
	return val;
}

unsigned char get8()
{
	int tmp = getchar();
	if (tmp < 0) {
		printf("Unexpected EOF.\n");
		exit(1);
	}
	// printf("get8 %02X\n", tmp);
	return tmp;
}

unsigned short get16()
{
	unsigned short tmp = get8() | ((unsigned short)get8() << 8);
	// printf(" => 0x%04X\n", tmp);
	return tmp;

}

int main(int argc, char *argv[])
{
	unsigned short INITAD = 0;   // 0x02E2 - 0x02E3, little-endian
	unsigned short RUNAD = 0;    // 0x02E0 - 0x02E1
	if (argc != 1) {
		printf("no parameters. pipe input into stdin.\n");
		exit(0);
	}

	expect8(0xFF);
	expect8(0xFF);
	int lookahead;
	while ((lookahead = getchar()) >= 0)
	{
		int uerr = ungetc(lookahead, stdin);
		assert(uerr != EOF);

		unsigned start_addr = get16();
		if (start_addr == 0xFFFF) {
			// Atari DOS permits concatenating executable files such
			// that the 0xFFFF header may be repeated. This occurs
			// surprisingly often. The line between 'object' files and
			// 'executables' is very blurry.
			printf("Detected concatenated executables - extra FFFF header.\n");
			start_addr = get16();
		}

		unsigned end_addr = get16();
		unsigned seg_len = end_addr - start_addr + 1;

		printf("Segment %04X - %04X (%d bytes / %Xh)\n", start_addr, end_addr, seg_len, seg_len);
		if (end_addr < start_addr) {
			printf("End address less than start address?\n");
			exit(1);
		}

		for (unsigned addr = start_addr; addr <= end_addr; addr++) {
			// printf("@ %04X\n", addr);
			unsigned char x = get8();
			if (addr == 0x02E0) RUNAD = (RUNAD & 0xFF00) | x;
			if (addr == 0x02E1) RUNAD = (RUNAD & 0x00FF) | ((unsigned short)x << 8);
			if (addr == 0x02E2) INITAD = (INITAD & 0xFF00) | x;
			if (addr == 0x02E3) INITAD = (INITAD & 0x00FF) | ((unsigned short)x << 8);
		}

		if (INITAD) {
			printf("Exec init code at %04X\n", INITAD);
			INITAD = 0;
		}
	}

	if (RUNAD) {
		printf("Run address set to %04X\n", RUNAD);
	}

	return 0;
}