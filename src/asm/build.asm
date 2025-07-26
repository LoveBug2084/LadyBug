;-----------------------------------------------------------------------------------------------------------------------------------------------------
; build everything
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	include "asm/memoryUsage.asm"		; show memory usage details

	include "asm/constants.asm"		; game constants

	include "asm/variables.asm"		; zero page variables, counters, flags etc

	include "asm/!boot.asm"			; custom !Boot file

	include "asm/config.asm"		; default game configs and high score table

	include "asm/loader.asm"		; graphics, utils, sideways ram and maingame loader

	include "asm/mapNames.asm"		; 3 default maze map names

	include "asm/ladybug.asm"		; main game
	
	include "asm/relocate.asm"		; initialization and game relocation

	include "asm/editor.asm"		; tile drawing functions for editor.bas

	include "asm/cls.asm"			; mode 1 clear screen function for editor.bas

	include "asm/bonus.asm"			; game bonus settings for game instructions in menu.bas


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; create disk files, keep them in loading order as far as possible to reduce floppy seek time
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	save		"!Boot", bootasmStart, bootasmEnd, &ffffff, 0

	putbasic	"bas/menu.bas", "Menu"

	save		"_Bonus", bonusBin, bonusBinEnd, &ffffff, 0

	save		"_Config", config, configEnd, &ffffff, 0
	save		"_Hsc", config, configEnd, &ffffff, 0

	save		"_Maps", mapFilenames, mapFilenamesEnd, &ffffff, 0

	putfile		"inc/map1.bin", "_Map1", 0, &ffffff
	putfile		"inc/map2.bin", "_Map2", 0, &ffffff
	putfile		"inc/map3.bin", "_Map3", 0, &ffffff

	save		"Loader", swramStart, loaderEnd, &ff0000 + loaderStartReloc, &ff0000 + loaderPage

	save		"LadyBug", progReloc, bootstrapEnd, &ff0000 + bootstrap + progOffset, &ff0000 + progLoad

	putbasic	"bas/reset.bas", "Reset"

	save		"_Reset", config, configEnd, &ffffff, 0

	putbasic	"bas/editor.bas", "Editor"

	save		"Cls", cls, clsEnd, &ff0000 + cls - canvasCls + pageCls, &ff0000 + cls - canvasCls + pageCls

	save		"EditorM", editorStart, editorEnd, &ffffff, 0

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; load the bbc editor tiles contained in the 20K mode 1 image saved by image2bbc.exe and save just the 19 tiles
	;---------------------------------------------------------------------------------------------------------------------------------------------

	org 0					; clear 20K of space for the bbc mode 1 image file
	clear 0, (20 * 1024) - 1

	incbin		"inc/editorTiles.bin"	; load the image file

						; save the 19 tiles of 12x8 pixels @ 4 pixels per byte (mode 1)
	save		"EditorT", 0, 19 * 12 * 8 / 4, &ffffff, 0

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; copy the readme.txt
	;---------------------------------------------------------------------------------------------------------------------------------------------

	puttext		"txt/readMe", "ReadMe", 0, &ffffff

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; save temporary speed test
	;---------------------------------------------------------------------------------------------------------------------------------------------

	putbasic	"bas/speed.bas", "STest"




;-----------------------------------------------------------------------------------------------------------------------------------------------------
; check for memory overrun
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	assert programEnd <= screenAddr		; main ram limit exceeded, check listing.txt
	assert swramLastAddr <= swramEnd	; high ram limit exceeded, check listing.txt



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; done
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print


