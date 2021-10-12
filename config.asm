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



	org diskConfig

	equb defaultEnemySpeed			; default enemy speed
	equb defaultEnemyAttack			; default enemy attack
	equb defaultLadybugLives		; default lives
	equb defaultSound			; default sound enabled
	equb defaultTimerVolume			; default timer volume
	equb keyX				; default key for right
	equb keyZ				; default key for left
	equb keySlash				; default key for down
	equb keyColon				; default key for up
	equs "XZ/:"				; default ascii text for keys
	equb &00,&10,&00			; default high score
	equs "LOVEBUG.ML", &ff			; default high score name

	print

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; save Config
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	save "Config", diskConfig, *
	clear diskConfig, *

	print
	print
	print
