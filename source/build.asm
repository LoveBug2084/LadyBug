;-----------------------------------------------------------------------------------------------------------------------------------------------------
; Lady Bug arcade style video game for the BBC Computer range based on the original 1981 arcade game by Universal
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; Copyright (C) 2021 LoveBug https://github.com/LoveBug2084/LadyBug
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details. https://www.gnu.org/licenses/
;-----------------------------------------------------------------------------------------------------------------------------------------------------

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; build everything
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	include "memoryusage.asm"		; show memory usage details

	include "constants.asm"			; global constants

	include "variables.asm"			; zero page variables, counters, flags etc

	include "!boot.asm"			; generate the !Boot file

	include "config.asm"			; default game configs and high score table

	include "loader.asm"			; sideways ram and game loader

	include "map.asm"			; 3 default maze maps

	include "ladybug.asm"			; main game
	
	include "relocator.asm"			; initialization and game relocation

	include "editor.asm"			; tile drawing functions for the basic map editor



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; create disk files, keep them in loading order as far as possible to reduce floppy seek time
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	save		"!Boot", bootasmStart, bootasmEnd, &ffffff, 0

	putbasic	"boot.bas", "Boot"

	save		"_Conf", config, configEnd, &ffffff, 0

	save		"_Maps", mapFilenames, mapFilenamesEnd, &ffffff, 0

	putfile		"default-map1.bin", "_Map1", 0, &ffffff
	putfile		"default-map2.bin", "_Map2", 0, &ffffff
	putfile		"default-map3.bin", "_Map3", 0, &ffffff

	save		"Loader", swramStart, loaderEnd, &ff0000 + loaderStartReloc, &ff0000 + loaderPage

	save		"LadyBug", progReloc, bootstrapEnd, &ff0000 + bootstrap + progOffset, &ff0000 + progLoad

	putbasic	"reset.bas", "Reset"

	save		"_ConfR", config, configEnd, &ffffff, 0

	putbasic	"editor.bas", "Editor"

	save		"EditorM", editorStart, editorEnd, &ffffff, 0

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; load the bbc editor tiles contained in the 20K mode 1 image saved by image2bbc.exe and save just the 19 tiles
	;---------------------------------------------------------------------------------------------------------------------------------------------

	org 0					; clear 20K of space for the bbc mode 1 image file
	clear 0, (20 * 1024) - 1

	incbin		"img-editor-tiles.bin"	; load the image file

						; save the 19 tiles of 12x8 pixels @ 4 pixels per byte (mode 1)

	save		"EditorT", 0, 19 * 12 * 8 / 4, &ffffff, 0



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; check for memory overrun
;-----------------------------------------------------------------------------------------------------------------------------------------------------


	assert programEnd <= screenAddr		; main ram limit exceeded, check listing.txt

	assert loaderEnd <= swramEnd		; high ram limit exceeded, check listing.txt



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; done
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print


