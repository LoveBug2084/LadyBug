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
; !Boot
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " !boot.asm"
	print "----------------------------------------------------"
	print

	org pageBoot				; temporary canvas address for !Boot data

.bootasmStart

	equb "*BASIC",13			; select basic language

	equb 22,7				; mode 7
	equb 23,1,0,0,0,0,0,0,0,0		; cursor off
	equb 23,0,6,0,0,0,0,0,0,0		; disable display

	equs "*SHADOW 1", 13			; disable shadow ram (generates an error on model B but screen is disabled so it does'nt show)
	equs "*FX 200 1",13			; disable ESC
	equs "*FX 4 1",13			; disable cursor editing

	equb 12					; erase text from screen

	equb 22,7				; select non-shadow mode 7
	equb 23,1,0,0,0,0,0,0,0,0		; cursor off
	equb 23,0,6,0,0,0,0,0,0,0		; disable display

	equs "CHAIN",34,"Boot",34,13		; CHAIN"Boot"



;-----------------------------------------------------------------------------------------------------------------------------------------------------
.bootasmEnd
	skip 0
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print
