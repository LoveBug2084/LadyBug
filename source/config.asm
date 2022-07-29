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
; Config
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " config.asm"
	print "----------------------------------------------------"
	print



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; default highscore table and settings
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org pageConfig				

.config
	skip 0					; show this address in listing



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; high score table
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	equb &00,&45,&00
	equs "UNIVERSAL",chrHeart,&FF

	equb &00,&40,&00
	equs "ASTEROIDS ",&FF

	equb &00,&35,&00
	equs "CENTIPEDE ",&FF

	equb &00,&30,&00
	equs "GALAXIAN  ",&FF

	equb &00,&25,&00
	equs "DEFENDER  ",&FF

	equb &00,&20,&00
	equs "ROBOTRON  ",&FF

	equb &00,&15,&00
	equs "SCRAMBLE  ",&FF

	equb &00,&10,&00
	equs "GAUNTLET  ",&FF




;-----------------------------------------------------------------------------------------------------------------------------------------------------
; settings					default settings
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	equb 3					; lady bugs (lives) 1-9
	equb 1					; enemy speed 0-5 (0=slower, 1=normal, 2=fast, 3=faster, 4=even faster, 5=crazy fast)
	equb 4					; enemy attack 0-9 (0=more random, 4=normal, 9=more aim)
	equb 1					; timer volume 0-3 (0=off, 1=low, 2=medium, 3=high)
	equb 1					; sound enable 0-1 (0=off, 1=on)

	equb keyX				; right 'X'
	equb keyZ				; left 'Z'
	equb keySlash				; down '/'
	equb keyColon				; up ':'

	equs "XZ/:"				; ascii text for keys

	equb &00				; validation code, no need to calculate it here
						; it is calculated in game and by boot.bas, reset.bas



;-----------------------------------------------------------------------------------------------------------------------------------------------------
.configEnd	
	skip 0					; show this address in listing
;-----------------------------------------------------------------------------------------------------------------------------------------------------
	
	print
	print
	print
