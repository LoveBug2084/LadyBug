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
; editor - map editor functions called from editor.bas
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " editor.asm"
	print "----------------------------------------------------"
	print

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; main editor functions
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org &2b00

.editorStart
	skip 0

.editorTileMirror

	equb &00,&01,&02,&03,&04,&05,&07,&06,&09,&08,&0B,&0A,&0C,&0D,&0E,&0F,&11,&10

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.editorMain

	lda &70					; get tile address
	sta editorRead + 1
	lda &71
	sta editorRead + 2

	lda &74					; get screen address
	sta editorWrite + 1
	lda &75
	sta editorWrite + 2
	
	ldy #23					; transfer 24 bytes

.editorRead

	lda addr16, y
	
.editorWrite

	sta addr16, y
	
	dey
	bpl editorRead
	
	rts					; return

;-----------------------------------------------------------------------------------------------------------------------------------------------------
.editorEnd
	skip 0
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print

	