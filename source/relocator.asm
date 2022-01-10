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
; relocator - initialise hardware and relocate main game to runtime address
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " relocator.asm"
	print "----------------------------------------------------"
	print

;-----------------------------------------------------------------------------------------------------------------------------------------------------
	align &100
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; disable display
	;---------------------------------------------------------------------------------------------------------------------------------------------

.bootstrap

	lda #19					; wait vsync
	jsr osbyte

	lda #6					; disable screen
	sta crtcAddr
	lda #0
	sta crtcData

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; set all palette colors to black
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #palBlack				; set all 16 palette colors to black

.bootstrapBlackPalette

	sta ulaPalette
	clc
	adc #&10
	bcc bootstrapBlackPalette
	
	lda #19					; wait 4 x vsync
	jsr osbyte
	jsr osbyte
	jsr osbyte
	jsr osbyte

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup stack, interrupts, via etc
	;---------------------------------------------------------------------------------------------------------------------------------------------

	sei					; disable cpu irq interrupts

	ldx #&ff				; initialise stack
	txs

	lda #magicNumber			; initialize random seed
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

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup custom graphics mode
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #13					; 14 crtc registers for custom video mode

.bootstrapCrtc

	stx crtcAddr				; copy data to crtc
	lda bootstrapCrtcData + progOffset, x
	sta crtcData

	dex					; until done
	bpl bootstrapCrtc

	lda #&f4				; put ula into mode 2
	sta ulaMode

	lda acccon				; disable shadow ram
	and #%00111000
	sta acccon

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup reset stuff
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda swrBank				; select ram bank
	sta bankSelect
	sta cleanResetBank + progOffset		; save ram bank for clean reset code

	eor #magicNumber			; calculate validation
	sta cleanResetValidation + progOffset
	
	lda machineType				; save machine type index for clean reset code
	sta cleanResetMachine + progOffset

	eor #magicNumber			; calculate validation
	clc
	adc cleanResetValidation + progOffset
	sta cleanResetValidation + progOffset

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; relocate program to runtime address
	;---------------------------------------------------------------------------------------------------------------------------------------------
	
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
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup config
	;---------------------------------------------------------------------------------------------------------------------------------------------

.relocateProgramConfig

	lda config, x				; copy config into the game config data
	sta configData, x
	inx
	cpx #configDataEnd - configData
	bne relocateProgramConfig

	lda optionLives				; copy lives
	sta lives

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; copy the 3 mazes into the game
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #0					; copy maze data into ram
	
.relocateProgramMazeData

	lda maze1Load, x
	sta maze1, x

	lda maze2Load, x
	sta maze2, x

	lda maze3Load, x
	sta maze3, x

	inx
	cpx #21*11
	bne relocateProgramMazeData

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; run lady bug
	;---------------------------------------------------------------------------------------------------------------------------------------------

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
	equb 0					; r8  non-interlaced, no skew
	equb 7					; r9  scanlines per row
	equb 32					; r10 cursor start
	equb 8					; r11 cursor end
	equb hi(screenAddr / 8)			; r12 screen start address, high
	equb lo(screenAddr / 8)			; r13 screen start address, low

;-----------------------------------------------------------------------------------------------------------------------------------------------------
.bootstrapEnd					; end of bootstrap program
	skip 0
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print
