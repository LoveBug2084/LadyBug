//---------------------------------------------------------
// pixels by lovebug
// converts images in raw format to ladybug format
//---------------------------------------------------------

// required
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

// palette lookup table: palette[index] = {R, G, B}
unsigned char palette[16][3] = {
	// Standard 8 colors (0-7)
	{0,     0,     0},     // 0: black
	{255,   0,     0},     // 1: red
	{0,   255,     0},     // 2: green
	{255, 255,     0},     // 3: yellow
	{0,     0,   255},     // 4: blue
	{255,   0,   255},     // 5: magenta
	{0,   255,   255},     // 6: cyan
	{255, 255,   255},     // 7: white

	// Extra colors (8-15)
	{128, 255, 255},       // 8: multiplier0
	{128, 128, 255},       // 9: multiplier1
	{255, 128, 128},       // 10: special0
	{255, 128, 255},       // 11: special1
	{255, 255, 128},       // 12: extra0
	{128, 255, 128},       // 13: extra1
	{128, 128, 128},       // 14: skull
	{ 96,  96,  96},       // 15: objects
};

// convert RGB to palette index (0-15), exit on unknown color
int rgb_to_palette(unsigned char r, unsigned char g, unsigned char b)
{
	for (int i = 0; i < 16; i++) {
		if (r == palette[i][0] && g == palette[i][1] && b == palette[i][2]) {
			return i;
		}
	}
	fprintf(stderr, "\n**** issue **** unknown RGB color: %d %d %d\n", r, g, b);
	exit(EXIT_FAILURE);
}

//--------------
// main program
//--------------

int main(int argc, char *argv[])
{

	// print instructions
	int arguments = argc;
	if (arguments < 3)
	{
		printf("----------------------------------------------------------\n");
		printf(" pixels by LoveBug\n");
		printf("----------------------------------------------------------\n\n");
		printf("Usage: pixels pngInputName binOutputName\n\n");
		exit(EXIT_FAILURE);
	}

	// check for duplicate file names
	if (strcmp(argv[1], argv[2]) == 0)
	{
		fprintf(stderr, "\n**** issue **** duplicate file names\n");
		exit(EXIT_FAILURE);
	}

	// get filenames
	char *pngInputName = argv[1];
	char *binOutputName = argv[2];

	// load PNG (force 3 channels = RGB)
	int width, height, channels;
	unsigned char *pngData = stbi_load(pngInputName, &width, &height, &channels, 3);
	if (!pngData) {
		fprintf(stderr, "\n**** issue **** PNG load failed: %s\n", stbi_failure_reason());
		exit(EXIT_FAILURE);
	}
	if (channels != 3) {
		fprintf(stderr, "\n**** issue **** expected RGB, got %d channels\n", channels);
		stbi_image_free(pngData);
		exit(EXIT_FAILURE);
	}

	// populate rawBuffer with palette indices
	int rawInputSize = width * height;
	unsigned int rawBuffer[rawInputSize];
	for (int i = 0; i < rawInputSize; i++) {
		unsigned char r = pngData[i * 3 + 0];
		unsigned char g = pngData[i * 3 + 1];
		unsigned char b = pngData[i * 3 + 2];
		rawBuffer[i] = rgb_to_palette(r, g, b);
	}
	stbi_image_free(pngData);

	// For compatibility, assume single image (rawImages = 1)
	// The original format had multiple images stacked vertically in strips
	// With PNG, each file is one image
	int rawImages = 1;
	int imageWidth = width;
	int imageHeight = height;

	// display info
	printf("%d images %s -> ", rawImages, pngInputName);

	// open binOutputName for writing
	FILE *binOutputFile = fopen(binOutputName, "wb");
	if (binOutputFile == NULL)
	{
		fprintf(stderr, "\n**** issue **** error creating %s\n", binOutputName);
		exit(EXIT_FAILURE);
	}

	// process images
	for (int images = 0; images < rawImages; images++)
	{

		// get pairs of pixels
		for (int x = 0; x < imageWidth / 2; x++)
		{

		// calculate horizontal offset in raw data
		int horizontal = (images * imageWidth) + (x * 2);

			// get lines of pixels
			for (int y = 0; y < imageHeight; y++)
			{

				// calculate vertical offset in raw data
				int vertical = (rawInputSize / imageHeight) * y;

				unsigned int dataByte = 0;

				unsigned int pixel;

				pixel = rawBuffer[vertical + horizontal];
				if ((pixel & 0x01) != 0)
					dataByte |= 0x02;
				if ((pixel & 0x02) != 0)
					dataByte |= 0x08;
				if ((pixel & 0x04) != 0)
					dataByte |= 0x20;
				if ((pixel & 0x08) != 0)
					dataByte |= 0x80;

				pixel = rawBuffer[vertical + horizontal + 1];
				if ((pixel & 0x01) != 0)
					dataByte |= 0x01;
				if ((pixel & 0x02) != 0)
					dataByte |= 0x04;
				if ((pixel & 0x04) != 0)
					dataByte |= 0x10;
				if ((pixel & 0x08) != 0)
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
