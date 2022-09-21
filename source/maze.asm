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
; Maze						default maze names
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " maze.asm"
	print "----------------------------------------------------"
	print



;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org mazeNames				; temporary space for creating Maps file

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mazeFilenames

	equb &00,&06,"1ezaM_"			; the 3 default mazes to load _Maze1 _Maze2 _Maze3
	equb &00,&06,"2ezaM_"			; acorns backward style strings (used by the basic programs)
	equb &00,&06,"3ezaM_"

.mazeFilenamesEnd

	print
	print
	print
