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
; Config
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " config.asm"
	print "----------------------------------------------------"
	print

;-----------------------------------------------------------------------------------------------------------------------------------------------------

config = &7b80					; default highscore table and settings

	org config

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; high score table
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	equb &00,&10,&00
	equs "LOVEBUG.ML",&FF

	equb &50,&09,&00
	equs "UNIVERSAL ",&FF

	equb &00,&09,&00
	equs "LADYBUG   ",&FF

	equb &50,&08,&00
	equs "LOVEBUG.ML",&FF

	equb &00,&08,&00
	equs "UNIVERSAL ",&FF

	equb &50,&07,&00
	equs "LADYBUG   ",&FF

	equb &00,&07,&00
	equs "LOVEBUG.ML",&FF

	equb &50,&06,&00
	equs "UNIVERSAL ",&FF




;-----------------------------------------------------------------------------------------------------------------------------------------------------
; settings					default settings
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	equb 1					; enemy speed 0-3 (0=slower, 1=normal, 2=faster, 3=fastest)
	equb 4					; enemy attack 0-9 (0=more random, 4=normal, 9=more attack)
	equb 3					; lives 1-9
	equb 1					; sound enable 0-1 (0=off, 1=on)
	equb 1					; timer volume 0-3 (0=off, 1=low, 2=medium, 3=high)

	equb keyX				; right 'X'
	equb keyZ				; left 'Z'
	equb keySlash				; down '/'
	equb keyColon				; up ':'

	equs "XZ/:"				; ascii text for keys

	equb &d3				; validation code



;-----------------------------------------------------------------------------------------------------------------------------------------------------
.configEnd	
	skip 0
;-----------------------------------------------------------------------------------------------------------------------------------------------------
	
	print
	print
	print
