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
// pixels by lovebug
// converts images in raw format to ladybug format
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
	if(arguments < 5)
	{
		printf("----------------------------------------------------------\n");
		printf(" pixels by LoveBug\n");
		printf("----------------------------------------------------------\n\n");
		printf("Usage: pixels rawInputName imageWidth imageHeight binOutputName\n\n");
		exit(EXIT_FAILURE);
	}

	// check for duplicate file names
	if(strcmp(argv[1], argv[4]) == 0)
	{
		fprintf(stderr, "\n**** issue **** duplicate file names\n");
		exit(EXIT_FAILURE);
	}

	// get arguments
	char *rawInputName	= argv[1];
	long imageWidth			= strtol(argv[2], NULL, 10);
	long imageHeight		= strtol(argv[3], NULL, 10);
	char *binOutputName	= argv[4];

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

	// create buffer for raw tile data
	unsigned int rawBuffer[rawInputSize];

	// read raw tile data into buffer
	for(int i = 0; i < rawInputSize; i++)
	{
		unsigned int dataByte = fgetc(rawInputFile);
		rawBuffer[i] = dataByte;
	}

	// close rawInput file
	fclose(rawInputFile);

	// get number of images in raw data
	int rawImages = rawInputSize / (imageWidth * imageHeight);

	// display info
	printf("%d images %s -> ", rawImages, rawInputName);

	// open binOutputName for writing
	FILE *binOutputFile = fopen(binOutputName, "wb");
	if(binOutputFile == NULL)
	{
		fprintf(stderr, "\n**** issue **** error creating %s\n", binOutputName);
		exit(EXIT_FAILURE);
	}

	// process images
	for(int images = 0; images < rawImages; images++)
	{

		// get pairs of pixels
		for(int x = 0; x < imageWidth / 2; x++)
		{

		// calculate horizontal offset in raw data
		int horizontal = (images * imageWidth) + (x * 2);

			// get lines of pixels
			for(int y = 0; y < imageHeight; y++)
			{

				// calculate vertical offset in raw data
				int vertical = (rawInputSize / imageHeight) * y;

				unsigned int dataByte = 0;

				unsigned int pixel;

				pixel = rawBuffer[vertical + horizontal];
				if((pixel & 0x01) != 0)
					dataByte |= 0x02;
				if((pixel & 0x02) != 0)
					dataByte |= 0x08;
				if((pixel & 0x04) != 0)
					dataByte |= 0x20;
				if((pixel & 0x08) != 0)
					dataByte |= 0x80;

				pixel = rawBuffer[vertical + horizontal + 1];
				if((pixel & 0x01) != 0)
					dataByte |= 0x01;
				if((pixel & 0x02) != 0)
					dataByte |= 0x04;
				if((pixel & 0x04) != 0)
					dataByte |= 0x10;
				if((pixel & 0x08) != 0)
					dataByte |= 0x40;

				fputc(dataByte, binOutputFile);

			}

		}

	}

	// close binOutput
	fclose(binOutputFile);

	// were done
	printf("%s\n", binOutputName);

	// exit
	exit(EXIT_SUCCESS);
}
