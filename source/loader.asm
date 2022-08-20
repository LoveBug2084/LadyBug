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
; loader					find sideways ram, copy data and then run ladybug
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " loader.asm"
	print "----------------------------------------------------"
	print

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; swram data goes here
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org swramStart

.highRamStart

	skip 16					; filler to prevent any possibility of the beeb detecting this as a valid sideways rom after reset



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; game high score table and config
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.configData
	skip 0					; show this address in listing



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; high score table
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.highScoreTable
	skip 0					; show this address in listing
	
	for i,1,8				; 8 entrys in table 1st to 8th
	
	skip 3					; reserve space for score
	skip 11					; reserve space for name

	next

.highScoreTableEnd
	skip 0					; show this address in listing



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; game settings
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.gameSettings					; start of game settings
	skip 0					; show this address in listing

.optionLives

	skip 1					; reserve space for ladybug lives
	
.optionEnemySpeed

	skip 1					; reserve space for enemy speed
	
.optionEnemyAttack

	skip 1					; reserve space for enemy attack
	
.optionTimerVolume

	skip 1					; reserve space for timer volume
	
.optionSound

	skip 1					; reserve space for sound off/on
	
.optionKeys

	skip 4					; reserve space for control keys scan code
	
.optionKeysAscii

	skip 4					; reserve space for control keys ascii code

.validationCode

	skip 1					; reserve space for validation code for high score table and settings
	
.configDataEnd
	skip 0					; show this address in listing



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; vegetables, points and sprites used by drawSprite
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.spriteBin
	skip 0					; show this address in listing

.sprite10x10Bin
	skip 0					; show this address in listing

.vegetablesBin
	skip 0					; show this address in listing

	incbin "img-vegetables.bin"		; load the 10x10 pixel vegetable sprites into memory

.vegetablesBinEnd
	skip 0					; show this address in listing

.pointsBin
	skip 0					; show this address in listing

	incbin "img-points.bin"			; load the 10x10 pixel points sprites into memory

.pointsBinEnd
	skip 0					; show this address in listing

.sprite10x10BinEnd
	skip 0					; show this address in listing

	sprite10x10 = (sprite10x10BinEnd - sprite10x10Bin) / sprite10x10Bytes
	pointsBaseImg = (pointsBin - spriteBin) / sprite10x10Bytes

.ladybugBin
	skip 0					; show this address in listing

	incbin "img-ladybug.bin"		; load ladybug sprite set into memory

.enemy1Bin
	skip 0					; show this address in listing

	incbin "img-enemy1.bin"			; load 1st enemy sprite set into memory

.enemy2Bin
	skip 0					; show this address in listing

	incbin "img-enemy2.bin"			; load 2nd enemy sprite set into memory

.enemy3Bin
	skip 0					; show this address in listing

	incbin "img-enemy3.bin"			; load 3rd enemy sprite set into memory

.enemy4Bin
	skip 0					; show this address in listing

	incbin "img-enemy4.bin"			; load 4th enemy sprite set into memory

.enemy5Bin
	skip 0					; show this address in listing

	incbin "img-enemy5.bin"			; load 5th enemy sprite set into memory

.enemy6Bin
	skip 0					; show this address in listing

	incbin "img-enemy6.bin"			; load 6th enemy sprite set into memory

.enemy7Bin
	skip 0					; show this address in listing

	incbin "img-enemy7.bin"			; load 7th enemy sprite set into memory

.enemy8Bin
	skip 0					; show this address in listing

	incbin "img-enemy8.bin"			; load 8th enemy sprite set into memory

.angelBin
	skip 0					; show this address in listing
	
	incbin "img-angel.bin"			; load ladybug angel sprite set into memory

.spriteBinEnd
	skip 0					; show this address in listing



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; clean reset code
;-----------------------------------------------------------------------------------------------------------------------------------------------------

zpAddr 			= 0

	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrCleanResetJmp

	equw swrCleanResetB			; Electron	No plans for this
	equw swrCleanResetB			; B		Save feature working
	equw swrCleanResetBplus			; B+		Save feature working
	equw swrCleanResetMaster		; Master	Save feature working
	equw swrCleanResetB			; Master ET	No plans for this
	equw swrCleanResetCompact		; Compact	Save feature working
	equw swrCleanResetB			; Unknown
	equw swrCleanResetB			; Unknown
	equw swrCleanResetB			; Unknown
	equw swrCleanResetB			; Unknown



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; choose correct machine function
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrCleanReset

	lda #opcodeRTI				; disable nmi
	sta pageNmi 

	lda swrCleanResetJmp, x			; get function address for machine type
	sta zpAddr
	lda swrCleanResetJmp + 1, x
	sta zpAddr + 1

	ldx #&ff				; initialize stack
	txs

	inx					; x = 0

	jmp (zpAddr)				; run the function



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; B reset use the regular simulated power on reset
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrCleanResetB

	lda #&7f				; disable all via interrupts (simulate power on state)
	sta via1Ier
	
	jmp (resetVector)			; run the regular reset code



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; B+ reset clear zp, jump into os to clear 0200-7fff
;-----------------------------------------------------------------------------------------------------------------------------------------------------

continueBplus		= &d973			; os rom reset code

	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrCleanResetBplus

	txa					; push 0 on stack (fake via interrupt enable flags)
	pha

.swrCleanResetBplusLoop
	sta pageZero, x				; fill 0000-00ff with 0
	inx
	bne swrCleanResetBplusLoop

	lda #&02				; fill 0200-7fff with 0 using the os rom reset code
	sta zpAddr + 1
	stx zpAddr
	txa
	jmp continueBplus			; run B+ reset code



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; Master reset clear 0000-00ff, 0200-7fff, c000-dfff (twice, once with shadow ram selected and then with main ram selected)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

masterMos320 = &e364				; reset vector @ fffc (used to check mos version number)
masterMos350 = &e374

	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrCleanResetMaster

	lda resetVector				; check for mos version
	
	cmp #lo(masterMos320)			; mos 3.20 supported
	beq swrCleanResetMaster320
	
	cmp #lo(masterMos350)			; mos 3.50 supported
	beq swrCleanResetMaster350

	bne swrCleanResetB			; other mos not supported, use the bbc model b function to wipe memory

	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrCleanResetMaster320

	txa					; push 0 on stack (fake via interrupt enable flags)
	pha

	jsr swrCleanResetMC			; clear shadow and main ram
	
	jmp cleanResetMaster320			; jump to code in stack to continue with mos setup

	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrCleanResetMaster350

	txa					; push 0 on stack (fake via interrupt enable flags)
	pha

	jsr swrCleanResetMC			; clear shadow and main ram
	
	jmp cleanResetMaster350			; jump to code in stack to continue with mos setup



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; Compact
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrCleanResetCompact

	txa					; push 0 on stack (fake via interrupt enable flags)
	pha

	jsr swrCleanResetMC			; clear shadow and main ram
	
	jmp cleanResetCompact			; jump to code in stack to continue with os setup
	


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; clear shadow and main ram (used for Master 128 and Master Compact)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrCleanResetMC

	tay					; zero index

	lda acccon				; clear bits 7-4, 1-0. set bits 3-2 of accon (select shadow ram at 3000-7fff and private ram at c000-dfff)
	and #%11110011
	ora #%00001100
	sta acccon

.swrCleanResetInit

	tya

	sta zpAddr + 1				; start at pageZero
	sta zpAddr

.swrCleanResetWipe

	sta (zpAddr), y				; wipe a page of memory
	iny
	bne swrCleanResetWipe

	ldx #opcodeRTI				; disable nmi
	stx pageNmi

.swrCleanResetNext

	inc zpAddr + 1				; next page

	ldx zpAddr + 1				; if page = &01 (stack) then skip to the next page
	cpx #&01
	beq swrCleanResetNext

	cpx #&80				; if page = &80 (swr) then skip to page &c0
	bne swrCleanResetEnd
	
	ldx #&c0
	stx zpAddr + 1

.swrCleanResetEnd

	cpx #&e0				; repeat until we get to &e000
	bne swrCleanResetWipe

	lda acccon				; test bit 2 of acccon and save test result (shadow ram select)
	and #%00000100
	php

	lda acccon				; clear bit 2 of acccon (select main ram into 3000-7fff)
	and #%11111011
	sta acccon

	plp					; if shadow ram was wipe then wipe main memory
	bne swrCleanResetInit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; swrInitScreen					fill screen ram with zero, setup palette
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrInitScreen

	ldx #lo(screenAddr)			; start x at low 8 bits of screen start address

	lda #pixels0				; fill screen with black

.swrInitScreenClear

	sta hi(screenAddr) * 256, x		; fill page with data
	inx
	bne swrInitScreenClear
	
	inc swrInitScreenClear + 2		; next page
	bpl swrInitScreenClear			; repeat until end of screen ram

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup default palette colors
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #15					; setup the 16 palette colors

.swrInitScreenPalette

	lda swrPaletteData, x			; copy colors to ula
	sta ulaPalette

	dex					; until done
	bpl swrInitScreenPalette
	
	rts					; return

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; initial palette colors
	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrPaletteData

	equb &00 + palBlack			; colors 0-7 are regular beeb colors
	equb &10 + palRed
	equb &20 + palGreen
	equb &30 + palYellow
	equb &40 + palBlue
	equb &50 + palMagenta
	equb &60 + palCyan
	equb &70 + palWhite

	equb &80 + palBlack			; color 8 unused
	equb &90 + palBlack			; color 9 unused

	equb palSpecial0 + palRed		; color 10 special (flashing red/magenta)
	equb palSpecial1 + palMagenta		; color 11 special (flashing magenta/red)
	equb palExtra0 + palYellow		; color 12 extra (flashing yellow/green)
	equb palExtra1 + palGreen		; color 13 extra (flashing green/yellow)
	equb palSkull + palWhite		; color 14 skull (fade effect or red when shield is active)
	equb palObject + palCyan		; color 15 object (red,yellow,cyan)



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; swrJoystickControl				combine playerInput with joystickInput
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrJoystickControl

	lda joystickEnable			; if userport joystick enabled
	cmp #2
	bne swrJoystickControlAnalogueFire
	
	lda via2PortB				; read digital joystick port, invert bits and store into joystickInput
	and #joystickFire + joystickLeft + joystickDown + joystickUp + joystickRight
	eor #joystickFire + joystickLeft + joystickDown + joystickUp + joystickRight
	sta joystickInput

.swrJoystickControlAnalogueFire

	lda joystickEnable			; if analogue joystick enabled
	cmp #1
	bne swrJoystickControlCombine

	lda joystickInput			; clear joystickInput fire button bit
	and #&ff eor keyBitStart
	sta joystickInput

	lda via1PortB				; if analogue joystick fire button pressed
	and #joystickFireAnalogue
	bne swrJoystickControlCombine

	lda #keyBitStart			; then set joystickInput fire button bit
	ora joystickInput
	sta joystickInput

.swrJoystickControlCombine

	lda playerInput				; combine playerInput (keyboard) with joystickInput
	ora joystickInput
	sta playerInput

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
	align &100
.swramLastAddr
	skip 0					; show this address in listing
;-----------------------------------------------------------------------------------------------------------------------------------------------------



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; loaderMessages				; various messages for ram test etc
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.loaderMessages
	skip 0					; show this address in listing

.loaderBuild

	equb 31,9,11				; position cursor
	equs 129,"Lady Bug ",132,"Build",135
	incbin "build.bin"
	equb &ff				; end

.loaderUsingBank

	equb 31,7,13				; position cursor
	equs 132,"Using sideways ram bank",135,"0"
	equb &ff				; end

.loaderUsingWorkspace

	equb 31,9,13				; position cursor
	equs 132, "Using B+ workspace ram"
	equb &ff				; end

.loaderBank

	equs "0123456789ABCDEF"			; hexadecimal bank number

.loaderRamFailed

	equb 31,7,13				; position cursor
	equs 136,132,"Sideways ram unavailable"
	equb &ff				; end



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; oscli command to run the main Lady Bug code
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.runLadybug

	equs "/LadyBug", &0d			; oscli run ladybug



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; screen center for erase
;-----------------------------------------------------------------------------------------------------------------------------------------------------

screenCenter	= &7d90				; start address of center block to erase for messages
screenCenterY	= &7e71				; address of tail of the 'y' below the center block that also needs to be erased



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; loader main entry point
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.loaderStart

	lda #19					; wait for vsync
	jsr osbyte

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; clear the center of the screen for message display
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #0					; clear center section of display for messages
	lda #' '

.loaderClearCenter

	sta screenCenter, x
	inx
	cpx #200
	bne loaderClearCenter

	sta screenCenterY			; clear the tail of the 'y'
	sta screenCenterY + 1
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; display the current build of Lady Bug
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldy #lo(loaderBuild - loaderReloc)	; display build number
	jsr loaderPrint - loaderReloc

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check machine type
	;---------------------------------------------------------------------------------------------------------------------------------------------

	sei					; disable interrupts

	lda bankSelectCopy			; save original sideways bank
	sta swrBankOriginal

	lda #0					; get bbc model
	ldx #1
	jsr osbyte

	stx machineType				; save machine type

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; if the machine type is b+ then check its not b with integra
	;---------------------------------------------------------------------------------------------------------------------------------------------

	cpx #2					; if it reports back as a b+ model
	bne loaderNotBplus

	jsr swrTestBplus - loaderReloc		; then test that there really is ram at bank 128 (real b+)
	beq loaderBplus				; if ram found in bank 128 then its a real B+
	
	dec machineType				; else its a model b with integra
	bne loaderNotBplus

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; real b+ was detected so display message and do the relocation
	;---------------------------------------------------------------------------------------------------------------------------------------------

.loaderBplus
						; we got this far so its a real b+, display b+ workspace message
	ldy #lo(loaderUsingWorkspace - loaderReloc)
	jsr loaderPrint - loaderReloc

	jmp loaderCopyData - loaderReloc	; copy data and run ladybug

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; not a b+ so check banks for sideways ram
	;---------------------------------------------------------------------------------------------------------------------------------------------

.loaderNotBplus

	jsr swrTest - loaderReloc		; test for sideways ram banks
	bne loaderFailed			; if none found then jump to failed message

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; sideways ram found so display message and bank number
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldy #lo(loaderUsingBank - loaderReloc)	; else display sideways bank message
	jsr loaderPrint - loaderReloc

	ldy swrBank				; display bank number
	lda loaderBank - loaderReloc, y
	jsr oswrch

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; relocate data to sideways ram
	;---------------------------------------------------------------------------------------------------------------------------------------------

.loaderCopyData

	jsr swramCopy - loaderReloc		; copy data to swram

	asl machineType				; convert machine type into index

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; run the Lady Bug main game code
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #lo(runLadybug - loaderReloc)	; run the main ladybug
	ldy #hi(runLadybug - loaderReloc)
	jmp oscli

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; sideways ram not found so display the error message
	;---------------------------------------------------------------------------------------------------------------------------------------------

.loaderFailed

	ldy #lo(loaderRamFailed - loaderReloc)	; display failure message
	jsr loaderPrint - loaderReloc
	
.loaderFailedLoop

	jmp loaderFailedLoop - loaderReloc	; infinite loop



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; loaderPrint					print from loaderMessages,y to oswrch
;						quit at terminator byte &ff (not printed)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.loaderPrint	

	lda loaderMessages - loaderReloc, y	; get byte from messages, y
	cmp #&ff				; if its the end marker (&ff) then exit
	beq loaderPrintExit

	jsr oswrch				; else print it
	
	iny					; and get another
	bne loaderPrint

.loaderPrintExit	

	rts					; done.



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; check system for sideways ram
;-----------------------------------------------------------------------------------------------------------------------------------------------------

swrTestCopyright	= pageHigh + 7		; memory location for copyright offset
swrTestLocation		= pageHigh + 8		; memory location for write test

swrBankOriginal		= pageHigh - 1		; storage for original system bank number
swrBank			= pageHigh - 2		; storage for bank number of swram
machineType		= pageHigh - 3		; storage for machine type

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; test sideways ram bank starting at 0
	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrTest

	ldy #0					; start with bank 0
	
.swrTestLoop

	jsr swrTestByte - loaderReloc		; if ram found then exit
	beq swrTestExit

	iny					; else try next bank

	cpy #16					; until all banks tested
	bne swrTestLoop

	lda #&ff				; ram test failed

.swrTestExit

	rts					; done.

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; test for b+ workspace ram
	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrTestBplus

	ldy #0					; save bank 0 byte
	sty bankSelect
	lda swrTestLocation
	pha
	
	ldy #128				; test B+ workspace ram
	jsr swrTestByte - loaderReloc
	bne swrTestBplusFailed
	
	ldy #0					; ram was found at bank 128 but it could be bank 0 on model B with integra
	sty bankSelect				; so select bank 0

	cmp swrTestLocation			; and check if byte is different
	beq swrTestBplusFailed
	
	pla					; byte was different to bank 0 so test passed, remove saved byte from stack

	ldy #128				; select bank 128
	sty bankSelect
	
	lda #0					; exit with passed status
	rts
	
.swrTestBplusFailed

	pla					; restore bank 0 byte
	sta swrTestLocation
	
	ldy #128				; select bank 128
	sty bankSelect

	lda #&ff				; exit with failed status
	rts


	;---------------------------------------------------------------------------------------------------------------------------------------------
	; select ram bank and test for ram
	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrTestByte

	sty bankSelectCopy			; save bank number
	sty swrBank

	tya					; select solidisk write bank
	and #&0f
	sta bankSelectSolidisk

	tax					; select watford electronics write bank
	sta bankSelectWatford, x

	sty bankSelect				; select bank

	ldx swrTestCopyright			; skip bank if in use ( contains &00,"(C)" at copyright offset )

	lda pageHigh + 0, x
	bne swrTestByteWrite
	
	lda pageHigh + 1, x
	cmp #'('
	bne swrTestByteWrite
	
	lda pageHigh + 2, x
	cmp #'C'
	bne swrTestByteWrite
	
	lda pageHigh + 3, x
	cmp #')'
	beq swrTestFailed

.swrTestByteWrite

	tya					; flip bits at test location
	eor #magicNumber
	eor swrTestLocation
	sta swrTestLocation
	
	ldx #0					; delay loop

.swrTestByteDelay

	dex
	bne swrTestByteDelay
	
	cmp swrTestLocation			; exit with compare result
	rts

.swrTestFailed

	lda #&ff				; exit with not equal (failed)
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; swramCopy					copy data from main ram to sideways ram
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swramCopy

	ldx #hi(swramLastAddr - swramStart)	; number of pages to copy

	ldy #0

.swramCopyLoop

	lda loaderPage, y			; read byte from loader data
	sta swramStart, y			; write byte to sideways ram

	iny					; repeat until page transferred
	bne swramCopyLoop
	
	inc swramCopyLoop + 2 - loaderReloc	; move to next page
	inc swramCopyLoop + 5 - loaderReloc

	dex					; repeat until all pages transferred
	bne swramCopyLoop

	rts					; done.



;-----------------------------------------------------------------------------------------------------------------------------------------------------
.loaderEnd
	skip 0					; show this address in listing
;-----------------------------------------------------------------------------------------------------------------------------------------------------

loaderPage = progLoad				; load address

loaderReloc = swramStart - loaderPage		; relocation
loaderStartReloc = loaderStart - loaderReloc

	print
	print
	print
