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

	include "!boot.asm"			; generate the !Boot file

	include "config.asm"			; default game configs and high score table

	include "loader.asm"			; sideways ram and game loader

	include "maze.asm"			; 3 default maze maps

	include "ladybug.asm"			; main game
	
	include "relocator.asm"			; initialization and game relocation

	include "editor.asm"			; tile drawing functions for the basic map editor


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; create disk files, keep them in loading order as far as possible to reduce floppy seek time
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	save "$.!Boot", bootasmStart, bootasmEnd, &ffffff, 0

	putbasic "boot.bas", "$.Boot"

	save "$.Config", config, configEnd, &ffffff, &ff0000 + config

	save "E.Maps", mazeFilenames, mazeFilenamesEnd, &ffffff, 0
	putfile "default-maze1.bin", "D.Maze1", 0, &ffffff
	putfile "default-maze2.bin", "D.Maze2", 0, &ffffff
	putfile "default-maze3.bin", "D.Maze3", 0, &ffffff

	save "$.Loader", swramStart, loaderEnd, &ff0000 + loaderStartReloc, &ff0000 + loaderPage
	save "$.LadyBug", progReloc, bootstrapEnd, &ff0000 + bootstrap + progOffset, &ff0000 + progLoad

	putbasic "reset.bas", "$.Reset"
	save "D.Config", config, configEnd, &ffffff, &ff0000 + config

	putbasic "editor.bas", "$.Editor"
	save "E.Code", editorStart, editorEnd, &ffffff, &ff0000 + editorStart
	putfile "img-editor.bin", "E.Tiles", 0, &ffffff

	assert programEnd <= screenAddr		; main ram limit exceeded, check ladybug.lst
	assert loaderEnd <= swramEnd		; high ram limit exceeded, check ladybug.lst



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; done
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print


