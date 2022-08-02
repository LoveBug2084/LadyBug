//-----------------------------------------------------------------------------------------------------------------------------------------------------
// Lady Bug arcade style video game for the BBC Computer range based on the original arcade game by universal 1981
//-----------------------------------------------------------------------------------------------------------------------------------------------------
// Copyright (C) 2021 LoveBug https://lovebyte.eu.org
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY// without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details. https://www.gnu.org/licenses/
//-----------------------------------------------------------------------------------------------------------------------------------------------------

//---------------------------------------------------------
// filelocker by LoveBug
// locks supplied file names in the .ssd image
//---------------------------------------------------------

// required
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define false 0
#define true 1

int main(int argc, char *argv[])
{

	// print instructions
	int arguments = argc - 1;
	if(arguments < 2)
	{
		printf("----------------------------------------------------------\n");
		printf(" filelocker by LoveBug\n");
		printf("----------------------------------------------------------\n\n");
		printf("Usage: filelocker ssdFileName filename filename filename ... (as many as required)\n\n");
		exit(EXIT_FAILURE);
	}

	// get arguments
	char *ssdFileName = argv[1];

	// open ssd file in binary mode for reading and writing
	FILE *ssdInput = fopen(ssdFileName, "r+");
	if(ssdInput == NULL)
	{
		fprintf(stderr, "\n**** issue **** error opening %s\n", ssdFileName);
		exit(EXIT_FAILURE);
	}

	// create buffer for dfs file names
	unsigned int dfsBuffer[256];

	// read first 256 byte sector containing dfs file names
	for(int bytes = 0; bytes <= 255; bytes++)
	{
		unsigned int dataByte = fgetc(ssdInput);
		dfsBuffer[bytes] = dataByte;
	}

	// close the ssd file
	fclose(ssdInput);

	// process filenames
	for(int filenames = 2; filenames <= arguments; filenames++)
	{
		printf("%s ", argv[filenames]);

		int fileFound = false;

		for(int ptr = 8; ptr < 256; ptr+=8)
		{
			int fileLock = true;
			for(int chr = 0; chr <= 7; chr++)
			{
				if(argv[filenames][chr] != dfsBuffer[ptr + chr])
					fileLock = false;
			}

			// if file name was found then lock the file
			if(fileLock)
			{
				dfsBuffer[ptr + 7] |= 0x80;
				fileFound = true;
				break;
			}
		}

		if(fileFound)
			printf("locked\n");
		else
			printf("not found\n");

	}

	// open ssd file in binary mode for reading and writing
	FILE *ssdOutput = fopen(ssdFileName, "r+");
	if(ssdOutput == NULL)
	{
		fprintf(stderr, "\n**** issue **** error opening %s\n", ssdFileName);
		exit(EXIT_FAILURE);
	}

	// write the modified sector containing locked files
	for(int bytes = 0; bytes <= 255; bytes++)
	{
		fputc(dfsBuffer[bytes], ssdOutput);
	}

	// close the ssd file
	fclose(ssdOutput);

}
