//---------------------------------------------------------
// font by lovebug
// converts a font in raw format to format used by ladybug
//---------------------------------------------------------

// required
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//--------------
// main program
//--------------

int main(int argc, char *argv[])
{

	// print instructions
	int arguments = argc;
	if(arguments < 3)
	{
		printf("----------------------------------------------------------\n");
		printf(" font by LoveBug\n");
		printf("----------------------------------------------------------\n\n");
		printf("Usage: font rawInputName binOutputName\n\n");
		exit(EXIT_FAILURE);
	}

	// filenames
	char *rawInputName, *fontOutputName;


	// check for duplicate file names
	if(strcmp(argv[1], argv[2]) == 0)
	{
		fprintf(stderr, "\n**** issue **** duplicate file names\n");
		exit(EXIT_FAILURE);
	}

	// get filenames
	rawInputName = argv[1];
	fontOutputName = argv[2];

	// open rawInputName in binary mode
	FILE *rawInputFile = fopen(rawInputName, "rb");
	if(rawInputFile == NULL)
	{
		fprintf(stderr, "\n**** issue **** error opening %s\n", rawInputName);
		exit(EXIT_FAILURE);
	}

	// get file size
	fseek(rawInputFile, 0L, SEEK_END);
	int rawInputSize = ftell(rawInputFile);
	rewind(rawInputFile);

	// create buffer for raw font data
	unsigned int rawBuffer[rawInputSize];

	// read raw font data into buffer
	for(int i = 0; i < rawInputSize; i++)
	{
		unsigned int dataByte = fgetc(rawInputFile);
		rawBuffer[i] = dataByte;
	}

	// close rawInput file
	fclose(rawInputFile);

	// get number of characters in raw font
	int rawCharacters = rawInputSize / 36;

	// display info
	printf("%d images %s -> ", rawCharacters, rawInputName);

	// open fontOutputName for writing
	FILE *fontOutputFile = fopen(fontOutputName, "wb");
	if(fontOutputFile == NULL)
	{
		fprintf(stderr, "\n**** issue **** error creating %s\n", fontOutputName);
		exit(EXIT_FAILURE);
	}

	// process characters
	for(int character = 0; character < rawCharacters; character++)
	{

		// start with a blank byte and bit 7
		unsigned int dataByte = 0;
		unsigned int bit = 128;

		// 3 pairs of pixels
		for(int x = 0; x <= 2; x++)
		{

		// calculate horizontal offset in raw data
		int horizontal = (character * 6) + (x * 2);

			// 6 lines of chr pixels
			for(int y = 0; y <= 5; y++)
			{

			// calculate vertical offset in raw data
			int vertical = (rawInputSize / 6) * y;

				// check first pixel in pair and set bit for output
				if(rawBuffer[vertical + horizontal] != 0)
					dataByte |= bit;
				bit /= 2;

				// check second pixel in pair and set bit bit for output
				if(rawBuffer[vertical + horizontal + 1] != 0)
					dataByte |= bit;
				bit /= 2;

				// if all 8 bits done then output byte to font file and setup byte and bit for next one
				if(bit == 0)
				{
					fputc(dataByte, fontOutputFile);
					dataByte = 0;
					bit = 128;
				}

			}

		}

		// after chr is done check if last byte is incomplete and output it (6 x 6 pixel chr = 36 bits or 4.5 bytes so this sends the last byte making 5 bytes total)
		if(bit != 128)
			fputc(dataByte, fontOutputFile);

	}

	// close fontOutput
	fclose(fontOutputFile);

	// were done
	printf("%s\n", fontOutputName);

	// exit
	exit(EXIT_SUCCESS);
}
