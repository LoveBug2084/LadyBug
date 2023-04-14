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
; thanks to everyone @ stardot forums for their kind words and support
;-----------------------------------------------------------------------------------------------------------------------------------------------------



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; clear screen function for editor.bas
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " cls.asm"
	print "----------------------------------------------------"
	print



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; assemble at termporary canvas address
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org canvasCls


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; clear mode 1 memory area &3000-&7fff then change to graphics mode
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.cls

	ldy #0
	tya
	
.clsLoop					; fill all screen pages with 0

	for s, screenAddrMode1, screenAddrMode1 + screenSizeMode1 - 1, &100
	sta s, y
	next

	iny
	beq clsExit
	jmp clsLoop - canvasCls + pageCls

.clsExit

	lda #22					; change to mode 1
	jsr oswrch
	lda #1
	jsr oswrch
	
	lda #23					; turn off cursor and return
	jsr oswrch
	lda #1
	jsr oswrch
	lda #0
	jsr oswrch
	jsr oswrch
	jsr oswrch
	jsr oswrch
	jsr oswrch
	jsr oswrch
	jsr oswrch
	jmp oswrch



;-----------------------------------------------------------------------------------------------------------------------------------------------------

.clsEnd
	skip 0					; show this address in listing

;-----------------------------------------------------------------------------------------------------------------------------------------------------



;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print
