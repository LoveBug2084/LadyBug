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
; relocator - initialise hardware and relocate main game to runtime address
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " relocator.asm"
	print "----------------------------------------------------"
	print

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	align &100

.bootstrap

	lda #19					; wait vsync
	jsr osByte

	lda #6					; disable screen
	sta crtcAddr
	lda #0
	sta crtcData

	lda #19					; wait 4 x vsync
	jsr osByte
	jsr osByte
	jsr osByte
	jsr osByte

	sei					; disable cpu irq interrupts

	ldx #&ff				; initialise stack
	txs

	lda swrBank				; select sideways ram bank
	sta bankSelect

	lda #&02				; initialize random seed
	sta randomSeed + progOffset
	sta randomSeed + 1 + progOffset

	lda #0					; timer 1 timed interupt mode
	sta viaAcr

	lda #&7f				; disable all interrupts
	sta viaIer
	sta via2Ier

	sta viaIfr				; clear all interrupt flags
	sta via2Ifr
	
	lda #&c2				; enable vsync and timer 1 interrupts
	sta viaIer

	lda #&0b				; disable keyboard output that drives bit 7 of via port A
	sta viaPortB

	lda #&ff				; via port A bits 0 - 7 output
	sta viaPortDdrA

	ldx #15					; setup the 16 palette colors

.bootstrapPalette

	lda bootstrapPalData + progOffset, x	; copy colors to ula
	sta ulaPalette

	dex					; until done
	bpl bootstrapPalette

	ldx #13					; 14 crtc registers for custom video mode

.bootstrapCrtc

	stx crtcAddr				; copy data to crtc
	lda bootstrapCrtcData + progOffset, x
	sta crtcData

	dex					; until done
	bpl bootstrapCrtc

	lda #&f4				; put ula into mode 2
	sta ulaMode



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; relocate program to runtime address
;-----------------------------------------------------------------------------------------------------------------------------------------------------
	
	ldx #hi(bootstrap - progReloc)		; program page length
	ldy #0

.relocateProgram

	lda progLoad, y				; copy from load page to relocation page
	sta progReloc, y

	iny
	bne relocateProgram

	inc relocateProgram + progOffset + 2	; do next page
	inc relocateProgram + progOffset + 5

	dex					; until all pages copied
	bne relocateProgram
	
	jmp main				; run the main game



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; crtc register data for custom screen mode
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.bootstrapCrtcData
	
	equb 127				; r0  horizontal total
	equb 69					; r1  horizontal displayed
	equb 92					; r2  horizontal position
	equb 40					; r3  sync width
	equb 38					; r4  vertical total
	equb 0					; r5  vertical total adjust
	equb 0					; r6  vertical displayed (set to 0 here to blank the display, set later in the game code)
	equb 32					; r7  vertical position
	equb 0					; r8  non-interlaced and no delays
	equb 7					; r9  scanlines per row
	equb 32					; r10 cursor start
	equb 8					; r11 cursor end
	equb hi(screenAddr / 8)			; r12 screen start address, high
	equb lo(screenAddr / 8)			; r13 screen start address, low



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; palette data
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.bootstrapPalData

	equb &00 + palBlack
	equb &10 + palRed
	equb &20 + palGreen
	equb &30 + palYellow
	equb &40 + palBlue
	equb &50 + palMagenta
	equb &60 + palCyan
	equb &70 + palWhite
	equb &80 + palBlack			; black for printing blank chrTiles/mapTiles and in score digits
	equb &90 + palBlack			; unused
	equb &a0 + palRed			; flashing red/magenta letters
	equb &b0 + palMagenta			; flashing red/magenta letters alternte phase
	equb &c0 + palYellow			; flashing yellow/green letters
	equb &d0 + palGreen			; flashing yellow/greem letters alternate phase
	equb &e0 + palWhite			; skull color changed by game for normal/invulnerable mode
	equb &f0 + palCyan			; bonus pickups color change cyan, red, yellow for hearts and letters

	print

;-----------------------------------------------------------------------------------------------------------------------------------------------------
.bootstrapEnd					; end of bootstrap program
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print
