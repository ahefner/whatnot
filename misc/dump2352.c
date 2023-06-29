#include <stdio.h>

// Given CUE/BIN-style rip of 2352 byte sectors, extract the 2 KB
// payloads and discard headers/parity.

int main()
{
	unsigned char buf[2352];
	while (fread(buf, 2352, 1, stdin))
	{
		fwrite(buf+16, 2048, 1, stdout);
	}

	return 0;
}