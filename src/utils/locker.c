//---------------------------------------------------------
// locker by LoveBug
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
		printf(" locker by LoveBug\n");
		printf("----------------------------------------------------------\n\n");
		printf("Usage: locker ssdFileName filename filename filename ... (as many as required)\n\n");
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
			{
			// beeb file not found so exit
			printf("not found\n");
			exit(EXIT_FAILURE);
			}
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
