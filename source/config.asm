;-----------------------------------------------------------------------------------------------------------------------------------------------------
; Lady Bug arcade style video game for the BBC Computer range based on the original 1981 arcade game by Universal
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

	org pageConfig				; default highscore table and settings
.config
	skip 0



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; high score table
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	equb &00,&10,&00
	equs "UNIVERSAL",chrCopyright,&FF

	equb &50,&09,&00
	equs "ASTEROIDS ",&FF

	equb &00,&09,&00
	equs "CENTIPEDE ",&FF

	equb &50,&08,&00
	equs "GALAXIAN  ",&FF

	equb &00,&08,&00
	equs "ROBOTRON  ",&FF

	equb &50,&07,&00
	equs "DEFENDER  ",&FF

	equb &00,&07,&00
	equs "SCRAMBLE  ",&FF

	equb &50,&06,&00
	equs "GAUNTLET  ",&FF




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

	equb &00				; validation code, no need to calculate it here
						; it is calculated by boot.bas and reset.bas



;-----------------------------------------------------------------------------------------------------------------------------------------------------
.configEnd	
	skip 0
;-----------------------------------------------------------------------------------------------------------------------------------------------------
	
	print
	print
	print
