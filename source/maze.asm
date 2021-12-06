;-----------------------------------------------------------------------------------------------------------------------------------------------------
; Lady Bug arcade style video game for the BBC Computer range based on the original arcade game by universal 1981
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; Copyright (C) 2021 LoveBug https://lovebug.ml
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
; Maze						default maze data
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " maze.asm"
	print "----------------------------------------------------"
	print

;-----------------------------------------------------------------------------------------------------------------------------------------------------

maze1Load = &7800				; maze addresses used by boot.bas to load the 3 maps before running lady bug
maze2Load = &7900
maze3Load = &7a00

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org &d000				; temporary space for creating maze files (saved by ladybug.asm)

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mazeFilenames

	equb &00,&07,"1ezaM.D"			; the 3 default mazes to load D.Maze1 D.Maze2 D.Maze3
	equb &00,&07,"2ezaM.D"			; acorn backwards style strings for use with OPENIN#
	equb &00,&07,"3ezaM.D"

.mazeFilenamesEnd

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mazeDefault1

	incbin "default-maze1.bin"

.mazeDefault1end

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mazeDefault2

	incbin "default-maze2.bin"

.mazeDefault2end

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mazeDefault3

	incbin "default-maze3.bin"

.mazeDefault3end

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print
