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



	print "----------------------------------------------------"
	print " editor.asm"
	print "----------------------------------------------------"
	print



;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org pageEditorCanvas

.editorStart
	skip 0					; show this address in listing

	;---------------------------------------------------------------------------------------------------------------------------------------------

.editorMapDirty					; dirty flag for maps (data changed)

	equb 0					; map 1
	equb 0					; map 2
	equb 0					; map 3
	equb 0					; not used but here so that editor.bas can !&2b00 to clear or test all maps at same time

.editorMapDirtyOld				; old copy so that editor.bas can display on change

	equb 0
	equb 0
	equb 0
	equb 0

	;---------------------------------------------------------------------------------------------------------------------------------------------

.editorTileMirror				; mirror tile id's for right hand side of maze

	equb &00,&01,&02,&03,&04,&05,&07,&06,&09,&08,&0B,&0A,&0C,&0D,&0E,&0F,&11,&10

	;---------------------------------------------------------------------------------------------------------------------------------------------

						; start address of editor mode 1 tile set
basTileSet	= pageEditor + &83

basTile		= &70				; calculated address of tile
basScreen	= &74				; screen address to write tile
basMaze		= &78				; maze data address

	;---------------------------------------------------------------------------------------------------------------------------------------------

.editorDrawTile

	asl a					; multiply tile id by 8
	asl a
	asl a
	
	sta basTile + 2				; save it

	clc					; add to tile set address 3 times (3*8=24 bytes per tile)
	adc #lo(basTileSet)
	sta basTile + 0
	lda #0
	adc #hi(basTileSet)
	sta basTile + 1

	lda basTile + 2
	clc
	adc basTile + 0
	sta basTile + 0
	bcc P%+4
	inc basTile + 1
	
	lda basTile + 2
	clc
	adc basTile + 0
	sta basTile + 0
	bcc P%+4
	inc basTile + 1
	
	ldy #23					; transfer 24 bytes

.editorDrawTileLoop

	lda (basTile), y
	sta (basScreen), y
	dey
	bpl editorDrawTileLoop
	
	clc					; add 24 to screen address
	lda #24
	adc basScreen + 0
	sta basScreen + 0
	lda #0
	adc basScreen + 1
	sta basScreen + 1

	rts					; return

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; draw a line of maze tiles
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.editorDrawLine

	lda basMaze + 0				; store maze data address into loop
	sta editorDrawLineLeft + 1 - pageEditorOffset
	sta editorDrawLineRight + 1 - pageEditorOffset
	lda basMaze + 1
	sta editorDrawLineLeft + 2 - pageEditorOffset
	sta editorDrawLineRight + 2 - pageEditorOffset

	ldx #0					; draw 11 bytes from maze (left half)

.editorDrawLineLeft

	lda dummy16, x				; get byte from maze and draw tile on screen (address previously setup)
	jsr editorDrawTile - pageEditorOffset
	
	inx
	cpx #11
	bne editorDrawLineLeft
	
	ldx #9					; draw 10 bytes from maze mirrored (right half)

.editorDrawLineRight

	ldy dummy16, x				; get byte from maze (address previously setup)

						; get mirrored tile id and draw it
	lda editorTileMirror - pageEditorOffset, y
	jsr editorDrawTile - pageEditorOffset
	
	dex
	bpl editorDrawLineRight

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
.editorEnd
	skip 0					; show this address in listing
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print

