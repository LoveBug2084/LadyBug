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
; Map						default map names
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " map.asm"
	print "----------------------------------------------------"
	print



;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org mapNames				; temporary space for creating Maps file

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mapFilenames

	equb &00,&05,"1paM_"			; the 3 default maps to load _Map1 _Map2 _Map3
	equb &00,&05,"2paM_"			; acorns backward style strings for openin/openout
	equb &00,&05,"3paM_"

.mapFilenamesEnd

	print
	print
	print
