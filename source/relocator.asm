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

	ldx #25					; set display rows from 25 to 1

.bootstrapDisableDisplay

	txa					; save x
	pha

	lda #19					; wait vsync
	jsr osbyte

.bootstrapDisableDisplayStore

	lda #6					; get saved x and set number of rows
	sta crtcAddr
	pla
	sta crtcData

	tax					; reduce row count by two
	dex
	dex
	bpl bootstrapDisableDisplay		; repeat until all rows done

	lda #1					; set columns to 1 (blank display)
	sta crtcAddr
	lda #0
	sta crtcData

	jsr bootstrapVsync + progOffset		; wait 8 vsyncs

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup custom graphics mode
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #13					; 14 crtc registers for custom video mode

.bootstrapCrtc

	stx crtcAddr				; copy register values to crtc
	lda bootstrapCrtcData + progOffset, x
	sta crtcData

	dex					; until done
	bpl bootstrapCrtc

	jsr bootstrapVsync + progOffset		; wait 8 vsyncs


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
	sta via1Acr

	lda #%01111111				; disable all interrupts
	sta via1Ier
	sta via2Ier

	sta via1Ifr				; clear all interrupt flags
	sta via2Ifr
	
	lda #%11000010				; enable vsync and timer 1 interrupts
	sta via1Ier

	lda #sbKeyboard + sbHigh		; disable keyboard output that drives bit 7 of via port A
	sta via1PortB

	lda #%11111111				; via port A bits 0 - 7 output
	sta via1PortDdrA

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup reset stuff
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda swrBank				; get ram bank number

	sta cleanResetBank + progOffset		; save ram bank for clean reset code

	sta bankSelect				; select ram bank

	bmi bootstrapResetValidation		; if its not B+ (bank 128) then

	sta bankSelectSolidisk			; select solidisk write bank

	tax					; select watford electronics write bank
	sta bankSelectWatford, x

.bootstrapResetValidation

	eor #magicNumber			; calculate validation
	sta cleanResetValidation + progOffset
	
	lda machineType				; save machine type index for clean reset code
	sta cleanResetMachine + progOffset

	eor #magicNumber			; calculate validation
	clc
	adc cleanResetValidation + progOffset
	sta cleanResetValidation + progOffset

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup joystick stuff
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda joystickEnable			; save joystick enable flag for relocation
	sta joystickEnable + progOffset

	beq bootstrapRelocate			; if its keyboard only then skip joystick setup

	lda joystickEnable			; if analogue joystick was selected then set that up
	cmp #1
	beq bootstrapJoystickAnalogue

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup digital joystick
	;---------------------------------------------------------------------------------------------------------------------------------------------

.bootstrapJoystickDigital

	lda via2PortDdrB			; set digital joystick inputs
	and #&ff eor (joystickFire + joystickLeft + joystickDown + joystickUp + joystickRight)
	sta via2PortDdrB
	jmp bootstrapRelocate + progOffset

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup analogue joystick addresses for b/b+ or master
	;---------------------------------------------------------------------------------------------------------------------------------------------

.bootstrapJoystickAnalogue

	lda machineType				; choose master or b/b+ adc addresses
	cmp #6
	beq bootstrapJoystickAnalogueMaster
	
	lda #lo(adcHighB)			; setup b/b+ adc addresses
	sta joystickAnalogueChannelRead + 1 + progOffset
	lda #hi(adcHighB)
	sta joystickAnalogueChannelRead + 2 + progOffset
	
	lda #lo(adcControlB)
	sta joystickAnalogueControlRead + 1 + progOffset
	sta joystickAnalogueControlWrite + 1 + progOffset
	lda #hi(adcControlB)
	sta joystickAnalogueControlRead + 2 + progOffset
	sta joystickAnalogueControlWrite + 2 + progOffset

	bne bootstrapRelocate			; done, do relocation (bne used as branch always)

.bootstrapJoystickAnalogueMaster

	lda #lo(adcHighM)			; setup master adc addresses
	sta joystickAnalogueChannelRead + 1 + progOffset
	lda #hi(adcHighM)
	sta joystickAnalogueChannelRead + 2 + progOffset
	
	lda #lo(adcControlM)
	sta joystickAnalogueControlRead + 1 + progOffset
	sta joystickAnalogueControlWrite + 1 + progOffset
	lda #hi(adcControlM)
	sta joystickAnalogueControlRead + 2 + progOffset
	sta joystickAnalogueControlWrite + 2 + progOffset

						; done, do relocation

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; relocate program to runtime address
	;---------------------------------------------------------------------------------------------------------------------------------------------

.bootstrapRelocate
	
	ldx #hi(bootstrap - progReloc)		; number of 256 byte pages to relocate
	ldy #0

.relocateProgram

	lda progLoad, y				; copy from load page to relocation page
	sta progReloc, y

	iny
	bne relocateProgram

	inc relocateProgram + progOffset + 2	; calculate next page address
	inc relocateProgram + progOffset + 5

	dex					; and repeat relocate until all pages copied
	bne relocateProgram
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; copy config
	;---------------------------------------------------------------------------------------------------------------------------------------------

.relocateProgramConfig

	lda config, x				; copy config into the game config data
	sta configData, x
	inx
	cpx #configDataEnd - configData
	bne relocateProgramConfig

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; copy lives
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda optionLives				; copy lives
	sta lives

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; copy the 3 maze maps into the game
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #0					; copy maze data into ram
	
.relocateProgramMapData

	lda map1Load, x
	sta map1, x

	lda map2Load, x
	sta map2, x

	lda map3Load, x
	sta map3, x

	inx
	cpx #21*11
	bne relocateProgramMapData

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; run lady bug
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jmp main				; run the main game



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; wait 8 vsyncs
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.bootstrapVsync

	lda #19
	jsr osbyte
	jsr osbyte
	jsr osbyte
	jsr osbyte
	jsr osbyte
	jsr osbyte
	jsr osbyte
	jmp osbyte



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; crtc register data for custom screen mode
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.bootstrapCrtcData
	
	equb 127				; r0  horizontal total
	equb 0					; r1  horizontal displayed (set to 0 here to blank the display, set later in the game code)
	equb 92					; r2  horizontal position
	equb 40					; r3  sync width
	equb 38					; r4  vertical total
	equb 0					; r5  vertical total adjust
	equb screenHeight			; r6  vertical displayed
	equb 32					; r7  vertical position
	equb 0					; r8  non-interlaced, no skew
	equb 7					; r9  scanlines per row
	equb 32					; r10 cursor start
	equb 8					; r11 cursor end
	equb hi(screenAddr / 8)			; r12 screen start address, high
	equb lo(screenAddr / 8)			; r13 screen start address, low



;-----------------------------------------------------------------------------------------------------------------------------------------------------
.bootstrapEnd					; end of bootstrap program
	skip 0					; show this address in listing
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print
