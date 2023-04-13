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

	org canvasBoot				; temporary canvas address for !Boot data

.bootasmStart

	equb "*BASIC",13			; select basic language

	equb 6					; enable vdu

	equb 22,7				; select mode 7
	equb 23,1,0,0,0,0,0,0,0,0		; cursor off
	equb 23,0,1,0,0,0,0,0,0,0		; disable display

	equs "*FX 200 1",13			; disable ESC
	equs "*FX 4 1",13			; disable cursor editing

	equs "*FX 114 1", 13			; disable shadow ram (generates an error on model B but screen is disabled so it does'nt show)
	equs "HIMEM=&7C00", 13			; set himem to non-shadow mode 7 address

	equb 12					; clear screen
	equb 22,7				; select non-shadow mode 7
	equb 23,1,0,0,0,0,0,0,0,0		; cursor off
	equb 23,0,1,0,0,0,0,0,0,0		; disable display

	equs "CHAIN",quote,"Boot",quote,13	; CHAIN"Boot"



;-----------------------------------------------------------------------------------------------------------------------------------------------------
.bootasmEnd
	skip 0					; show this address in listing
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print
