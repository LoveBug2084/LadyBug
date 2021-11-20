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
; rom
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " rom.asm"
	print "----------------------------------------------------"
	print

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; dummy sideways rom to test sideways ram check in loader.asm
	; load dummy into sideways ram for loader to skip that bank
	; fill all ram banks for loader to report that there is no sideways ram available
	;---------------------------------------------------------------------------------------------------------------------------------------------

	org &E000				; temporary address for fakerom data

.romStart

	equb 0,0,0				; no language entry
	equb 0,0,0				; no service entry
	equb 0					; no rom type entry
	equb lo(romCopyright)			; offset to copyright message
	equb 0					; no binary version number
	equb 0					; no title string
	equb 0					; no version string
	
.romCopyright

	equs 0,"(C) LoveBug 2021",13,0		; copyright string

.romEnd
	skip 0

	print
	print
	print
