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
// filelocker by lovebug
// locks files in the .ssd image
//---------------------------------------------------------

// required
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{

	// print instructions
	int arguments = argc;
	if(arguments < 2)
	{
		printf("----------------------------------------------------------\n");
		printf(" filelocker by LoveBug\n");
		printf("----------------------------------------------------------\n\n");
		printf("Usage: filelocker ssdFileName\n\n");
		exit(EXIT_FAILURE);
	}

	// get arguments
	char *ssdFileName = argv[1];

	// open ssdFileName in binary mode for reading and writing
	FILE *ssdFileHandle = fopen(ssdFileName, "r+");
	if(ssdFileHandle == NULL)
	{
		fprintf(stderr, "\n**** issue **** error opening %s\n", ssdFileName);
		exit(EXIT_FAILURE);
	}

	// close ssdFileHandle
	fclose(ssdFileHandle);

}
