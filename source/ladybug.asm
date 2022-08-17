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
; lady bug main program
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " ladybug.asm"
	print "----------------------------------------------------"
	print



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; page0100 functions
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org page0100



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; random					generate an 8 bit random number
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			8 bit random number from address randomSeed + 1
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		randomSeed		2 bytes used to calculate next random number
;						total length = 65535 random bytes before pattern repeats
;						add more seed bytes if longer run length is required
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.random

	lda randomSeed + 1			; get hi 8 bits of seed
	lsr a					; shift it right to put bit 0 into carry
	lda randomSeed				; get lo 8 bits of seed
	ror a					; rotate it right putting carry into bit 7 and bit 0 into carry
	eor randomSeed + 1			; eor with hi 8 bits
	sta randomSeed + 1			; store in high 8 bits
	ror a					; rotate it right putting carry into bit 7
	eor randomSeed				; eor with lo 8 bits
	sta randomSeed				; store in lo 8 bits
	eor randomSeed + 1			; eor with hi 8 bits
	sta randomSeed + 1			; store in hi 8 bits

	rts					; return with random number in A



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawVegetableScore				draws vegetable score in the center box if enabled
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawVegetableScore

	lda vegetableScoreActive		; if vegetableScoreActive != 0 (active)
	beq drawVegetableScoreExit

						; set draw position to the center box

	lda #lo(screenAddr + vegetableScoreX * chrColumn + vegetableScoreY * chrRow)
	sta drawChrMiniAddr
	lda #hi(screenAddr + vegetableScoreX * chrColumn + vegetableScoreY * chrRow)
	sta drawChrMiniAddr + 1

	

	lda vegetableScore			; draw the top 2 digits of vegetable bonus
	jsr drawBcdMini

	lda #&00				; draw "00"
	jsr drawBcdMini

.drawVegetableScoreExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; stack variables				storage for variables preserved after clean reset
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org page0130

	;---------------------------------------------------------------------------------------------------------------------------------------------

.cleanResetBank

	skip 1					; swr bank number for program and data
	
.cleanResetMachine

	skip 1					; machine type index

.cleanResetValidation

	skip 1					; validation of cleanResetBank and cleanResetMachine (checksum)

.joystickEnable

	skip 1					; control input mode, 0 = no joystick, 1 = analogue joystick, 2 = userport joystick
						; note: keyboard is always active even when joystick is enabled


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; cleanReset (break key)			simulated power on reset while preserving the stack and high ram
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			jumps into high ram to select one of the following clean reset functions for each machine
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.cleanReset

	sei					; disable irq interrupts
	
	lda cleanResetBank			; page in high ram bank
	sta bankSelect
	
	ldx cleanResetMachine			; get machine index

	jmp swrCleanReset			; continue with reset code in high ram to clear memory (in loader.asm) which then jumps back to
						; continue with original os reset setup code



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; cleanResetMaster320				page in bank 15, jump into mos to continue
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			jumps into os rom to continue with mchine setup
;-----------------------------------------------------------------------------------------------------------------------------------------------------

continueMaster320	= &8073			; master 3.20 entry point

	;---------------------------------------------------------------------------------------------------------------------------------------------

.cleanResetMaster320

	lda #&0f				; page in extra os code at 8000
	sta bankSelectCopy
	sta bankSelect
	
	jmp continueMaster320			; continue with master os reset code



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; cleanResetMaster350				page in extra mos code at fc00 and bank 15, jump into mos to continue
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			jumps into os rom to continue with mchine setup
;-----------------------------------------------------------------------------------------------------------------------------------------------------

continueMaster350	= &fc76			; master 3.50 entry point

	;---------------------------------------------------------------------------------------------------------------------------------------------

.cleanResetMaster350

	lda #&49				; page in extra mos code at fc00
	sta acccon

	lda #&0f				; page in extra mos code at 8000
	sta bankSelectCopy
	sta bankSelect
	
	jmp continueMaster350			; continue with master os reset code



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; cleanResetCompact				page in bank 15, jump into mos to continue
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			jumps into os rom to continue with mchine setup
;-----------------------------------------------------------------------------------------------------------------------------------------------------

continueCompact		= &8068			; master compact entry point

	;---------------------------------------------------------------------------------------------------------------------------------------------

.cleanResetCompact

	lda #&0f				; page in extra os code at 8000
	sta bankSelectCopy
	sta bankSelect
	
	jmp continueCompact			; continue with compact os reset code



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; irq interrupt					handle vsync and timer1 interrupts, setting screenHalf upper/lower flag and bump counters
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			screenHalf		set to 00 for upper half or ff for lower half
;			vsyncCounter		incremented every vsync (50Hz)
;			pauseCount		decremented every 2 * vsync (25Hz)
;			P			preserved
;			A			preserved
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

rasterTimer		= (312 / 2) * 64	; timer1 interupt raster line 156 ( (312 / 2) * 64uS )

	;---------------------------------------------------------------------------------------------------------------------------------------------

.irqInterrupt

	lda via1Ifr				; if interrupt flag = vsync
	and #2
	bne irqVsync				; then go do the upper vsync interrupt
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; lower interrupt (timer 1)
	;---------------------------------------------------------------------------------------------------------------------------------------------

.irqTimer					; else its a timer interrupt so do the lower interrupt

	lda #&40				; clear timer interrupt flag
	sta via1Ifr

	lda #&ff				; screenHalf = lower
	sta screenHalf

	lda irqAcc				; restore A

	rti					; return to main program

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; upper interrupt (vsync)
	;---------------------------------------------------------------------------------------------------------------------------------------------

.irqVsync

	sta via1Ifr				; clear vsync interrupt flag

	lda #lo(rasterTimer)			; set timer 1 for lower interrupt
	sta via1T1CounterLo
	lda #hi(rasterTimer)
	sta via1T1CounterHi

	lda #&00				; screenHalf = upper
	sta screenHalf

	inc vsyncCounter			; bump vsync counter

	lda vsyncCounter			; if vsyncCounter & 1 == 0
	and #1
	bne irqVsyncExit
	
	dec pauseCounter			; then bump the 25Hz pause counter

	;---------------------------------------------------------------------------------------------------------------------------------------------

 .irqVsyncExit

	lda irqAcc				; restore A
	
	rti					; return to main program



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; psgWrite 					write to sound chip
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			data to be written to 76489 psg
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.psgWrite

	sta via1PortA				; place psg data on port a slow bus
	
	lda #sbPsg + sbLow			; slow bus psg -we low
	sta via1PortB
	
	pha					; 5uS delay
	pla
	nop
	nop
	
	lda #sbPsg + sbHigh			; slow bus psg -we high
	sta via1PortB

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; waitVsyncUpper				wait for next vsync to upper area and read analogue joystick (if enabled)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed (by .joytickAnalogue)
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.waitVsyncUpper

	bit screenHalf				; wait until upper area
	bpl waitVsyncUpper
	
	jmp joystickAnalogue			; read analogue joystick (if enabled) and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; waitVsyncLower				wait for next vsync to lower area and read analogue joystick (if enabled)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed (by .joystickAnalog)
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.waitVsyncLower

	bit screenHalf				; wait until lower area
	bmi waitVsyncLower
	
	jmp joystickAnalogue			; read analogue joystick (if enabled) and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; updateObjectTimer				update object timer (25Hz), change object mode and palette color
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateObjectTimerPalette			; palette colors for letters and hearts

	equb palObject + palCyan
	equb palObject + palRed
	equb palObject + palYellow

	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateObjectTimerFrames			; duration of the colors letters and hearts

	equb objectModeCyanTime
	equb objectModeRedTime
	equb objectModeYellowTime

	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateObjectTimer

	lda vsyncCounter			; if vsync counter and 1 = 0 (25Hz)
	and #1
	bne updateObjectTimerExit
	
	dec objectModeTimer			; then bump object timer
	bne updateObjectTimerExit		; if objectModeTimer = 0

	stx updateObjectTimerSaveX		; save register
	
	ldx objectMode				; get current objectMode

	inx					; bump it
	cpx #3					; if its >= 3 then set it back to 0
	bne updateObjectTimerColor
	ldx #0
	
.updateObjectTimerColor

	stx objectMode				; update objectMode
	
	lda updateObjectTimerPalette, x		; update color palette
	sta ulaPalette
	
	lda updateObjectTimerFrames, x		; update timer
	sta objectModeTimer

	ldx updateObjectTimerSaveX		; restore register
	
.updateObjectTimerExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; end of page0100 functions
;-----------------------------------------------------------------------------------------------------------------------------------------------------
.page0100End
	skip 0					; show this address in listing
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.stack						; stack area from here to &01ff
	skip 0					; show this address in listing



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; page0200 irq vector, functions, break key vector
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org page0200

.keyOrder					; squeeze this in the previously unused 4 bytes here

	equb 1, 2, 3, 0				; change key order from the config's right, left, down, up
						; to the digital joystick order of left, down, up, right

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; set irq interrupt vector
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org irqVector

.irqVectorAddress

	equw irqInterrupt			; set bbc os irq1v interrupt vector to our irqInterrupt function



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; inputScan					check up down left right start and esc keys
;						combine with joystickInput so either keyboard or joystick can control the game
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			playerInput		bit 0=start 1=left 2=down 3=up 4=right 5=esc
;			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.inputScan

	lda #&7f				; set port A bit 7 as input ( from keyboard output )
	sta via1PortDdrA
	
	lda #sbKeyboard + sbLow			; keyboard -enable low
	sta via1PortB
	
	lda #0					; clear player input flags
	sta playerInput

	lda #keyEsc				; check esc key
	jsr readKey

	ldx #3					; check user defined movement keys
	
.inputScanLoop

	ldy keyOrder, x				; change order of config keys to match digital joystick layout
	lda optionKeys, y
	jsr readKey
	
	dex
	bpl inputScanLoop
	
	lda #keyReturn				; check start key
	jsr readKey

	lda #sbKeyboard + sbHigh		; slow bus keyboard -enable high
	sta via1PortB
	
	lda #&ff				; set port A all bits output
	sta via1PortDdrA

	jmp swrJoystickControl			; combine keyboard input with joystick input (if enabled) (in loader.asm)



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; readKey					place key onto slow bus port a and read key status into player input bits
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			key scan code
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			playerInput		shifted left and key status placed into bit 0
;			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------


.readKey

	sta via1PortA				; select key

	lda via1PortA				; read key status

	asl a					; shift key status into player input bits
	rol playerInput
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawObjectScore				draws object score img at xy if enabled
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawObjectScore

	lda objectScoreX			; set position of object score
	sta drawSpriteX
	lda objectScoreY
	sta drawSpriteY

	lda objectScoreImg			; if objectScoreImg != 0 (active)
	beq drawObjectScoreExit

	jmp drawSprite10x10			; then draw it
	
.drawObjectScoreExit

	rts					; else return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; end of page0200 functions
;-----------------------------------------------------------------------------------------------------------------------------------------------------
.page0200End
	skip 0					; show this address in listing
;-----------------------------------------------------------------------------------------------------------------------------------------------------



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; pagefx200					os location for *fx 200 value, set to 0 to make sure the ram is not erased by the os on break key
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org pagefx200

.osFx200
	
	equb 0



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; chooseLetters					choose 3 random letters make sure there are no duplicates
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			levelLetters		random letter tile id
;			levelLetters + 1	random letter tile id
;			levelLetters + 2	random letter tile id
;			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.chooseLetters

	jsr chooseLetterRandom			; pick 1st random letter
	sta levelLetters			; and store it in 1st
	
.chooseLetters2nd

	jsr chooseLetterRandom			; pick 2nd random letter

	cmp levelLetters			; if its the same as 1st then try again
	beq chooseLetters2nd
		
	sta levelLetters + 1			; else store it in 2nd
	
.chooseLetters3rd

	jsr chooseLetterRandom			; pick 3rd random letter
	
	cmp levelLetters			; if its the same as 1st then try again
	beq chooseLetters3rd
		
	cmp levelLetters + 1			; if its the same as 2nd then try again
	beq chooseLetters3rd

	sta levelLetters + 2			; else store it in 3rd

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; chooseLetterRandom				pick a random number (0-9) and return with one of the 10 letter tile id's in A
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			random letter tile id
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.chooseLetterRandom

	jsr random				; pick a random number 0-15
	and #15
	cmp #10					; if its >= 10 then pick another
	bcs chooseLetterRandom
	
	adc #objectTileIndex			; add object tile index for letters (carry is clear here so no need for clc)

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; end of pagefx200
;-----------------------------------------------------------------------------------------------------------------------------------------------------
.pagefx200End
	skip 0					; show this address in listing
;-----------------------------------------------------------------------------------------------------------------------------------------------------



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; set the break key jump vector
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org pageBreak				; set the os break jump vector to the clean reset function

.osBreakVector

	jmp cleanReset



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; updateEnemyTimer				update the enemy timer, draw timer tile when needed
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed (call to changeTimerTile)
;			Y			destroyed (call to changeTimerTile)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateEnemyTimer

	lda pauseEnemy				; if enemy movement is paused then exit
	bne updateEnemyTimerExit

	dec enemyTimerSpeedCounter		; bump enemy timer speed counter
	bne updateEnemyTimerExit		; exit if not zero

	lda enemyTimerSpeed			; reset the enemy timer speed counter
	sta enemyTimerSpeedCounter

	jsr changeTimerTile			; change enemy timer tile color

	inc enemyTimer				; bump enemy timer

	lda enemyTimer				; if enemyTimer = 88
	cmp #88
	bcc updateEnemyTimerSound

	lda #0					; then reset enemy timer
	sta enemyTimer

.updateEnemyTimerSound

	jsr playSoundTimer			; play timer sound at selected volume

	lda enemyTimer				; if enemyTimer is top left
	cmp #78
	bne updateEnemyTimerExit
	
	lda enemiesActive			; if theres an enemy waiting in the center
	cmp #spritesTotal - 1
	beq updateEnemyTimerExit

	lda #sfxEnemyWarning			; then play enemy release warning and enable enemy release
	jsr playSound
	lda #&ff
	sta enemyReleaseEnable

.updateEnemyTimerExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; initPlayfieldMiddle				copy maze tiles to tileMap, count the number of dots and store in levelEdibles
;						place hearts in the map replacing dots (hearts are edible so no change to levelEdibles)
;						place letters in the map replacing dots (letters are edible so no change to levelEdibles)
;						place skulls in map replacing dots ..
;						(skulls are not edible so the number of skulls is subtracted from levelEdibles by placeTileMapSkulls)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.initPlayfieldMiddleMazeTable

	equw maze1, maze2, maze3		; list of available maze maps

	;---------------------------------------------------------------------------------------------------------------------------------------------

.initPlayfieldMiddleTilesLeft

	equb &00,&01,&dc,&dd,&9e,&9f,&a0,&a1,&d2,&d3,&d4,&d5,&d6,&d7,&d8,&d9,&da,&db

.initPlayfieldMiddleTilesRight

	equb &00,&01,&dc,&dd,&9e,&9f,&a1,&a0,&d3,&d2,&d5,&d4,&d6,&d7,&d8,&d9,&db,&da


	;---------------------------------------------------------------------------------------------------------------------------------------------

.initPlayfieldMiddle

	lda #0
	sta levelEdibles			; initialize edibles

	lda mazeMap				; get maze map number
	and #%0110				; remove bit 0 so that 0,1,2,3,4,5 becomes 0,0,2,2,4,4

	tay					; set start address of maze data using map number as index
	lda initPlayfieldMiddleMazeTable, y
	sta initPlayfieldMiddleRead + 1
	lda initPlayfieldMiddleMazeTable + 1, y
	sta initPlayfieldMiddleRead + 2
	
	lda #lo(tileMap + 24)			; set start address of tile map to row 1 column 1 (inside the timer tiles)
	sta initPlayfieldMiddleWriteLeft + 1
	lda #hi(tileMap + 24)
	sta initPlayfieldMiddleWriteLeft + 2

	lda #21					; 21 rows to copy
	sta initPlayfieldMiddleRows

	;---------------------------------------------------------------------------------------------------------------------------------------------

.initPlayfieldMiddleLoop

	ldy #0

	lda initPlayfieldMiddleWriteLeft + 1	; calculate end of row address
	clc
	adc #20
	sta initPlayfieldMiddleWriteRight + 1
	lda initPlayfieldMiddleWriteLeft + 2
	adc #0
	sta initPlayfieldMiddleWriteRight + 2

	;---------------------------------------------------------------------------------------------------------------------------------------------

.initPlayfieldMiddleRead

	ldx dummy16, y				; get byte from maze (address previously setup)

	lda initPlayfieldMiddleTilesLeft, x	; convert to tile for left side

	;---------------------------------------------------------------------------------------------------------------------------------------------

.initPlayfieldMiddleWriteLeft

	sta dummy16, y				; store tile in map (address previously setup)
	
	cmp #mapTileDot				; if its a dot
	bne initPlayfieldMiddleRight

	inc levelEdibles			; then increment levelEdibles (count the dots)

	cpy #10					; if not middle column (we dont want to count the middle column dot twice)
	beq initPlayfieldMiddleRight
	
	inc levelEdibles			; then increment levelEdibles

	;---------------------------------------------------------------------------------------------------------------------------------------------

.initPlayfieldMiddleRight

	lda initPlayfieldMiddleTilesRight, x	; convert to tile for right side

	;---------------------------------------------------------------------------------------------------------------------------------------------

.initPlayfieldMiddleWriteRight

	sta dummy16				; store tile in map (address previously setup)

	sec					; decrement right side address as we are filling from right most column to center
	lda initPlayfieldMiddleWriteRight + 1
	sbc #1
	sta initPlayfieldMiddleWriteRight + 1
	lda initPlayfieldMiddleWriteRight + 2
	sbc #0
	sta initPlayfieldMiddleWriteRight + 2

	iny					; repeat until row transferred
	cpy #11
	bne initPlayfieldMiddleRead
	
	tya					; move maze address forward 11 bytes to next row
	clc
	adc initPlayfieldMiddleRead + 1
	sta initPlayfieldMiddleRead + 1
	bcc initPlayfieldMiddleWriteNextRow
	inc initPlayfieldMiddleRead + 2
	
.initPlayfieldMiddleWriteNextRow

	clc					; move map address forward 23 bytes to next row
	lda initPlayfieldMiddleWriteLeft + 1
	adc #23
	sta initPlayfieldMiddleWriteLeft + 1
	bcc initPlayfieldMiddleRightNext
	inc initPlayfieldMiddleWriteLeft + 2
	
.initPlayfieldMiddleRightNext

	dec initPlayfieldMiddleRows		; repeat until all rows done
	bne initPlayfieldMiddleLoop

	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr placeTileMapHearts			; place 3 hearts at random positions in the tileMap (replacing dots)

	jsr placeTileMapLetters			; place 3 random letters at random positions in the tileMap (replacing dots)

	jmp placeTileMapSkulls			; place the correct number of skulls at random positions in the tileMap (replacing dots) and exit



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; initTimerTiles				fill the outer edges of tileMap with timer tiles
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.initTimerTiles

	lda #mapTileTimerTopLeft + wallSolid	; store top left timer tile in tileMap
	sta tileMap

	lda #mapTileTimerTopRight + wallSolid	; store top right timer tile in tileMap
	sta tileMap + 22
	
	lda #mapTileTimerBottomLeft + wallSolid	; bottom left timer tile in tileMap
	sta tileMap + 22 * 23
	
	lda #mapTileTimerBottomRight + wallSolid; bottom right timer tile in tilemap
	sta tileMap + 22 * 23 + 22
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #21					; do 21 copies of
	
.initTimerTilesHorizontal

	lda #mapTileTimerTop + wallSolid	; store top timer tile in top row of tileMap
	sta tileMap, x
	
	lda #mapTileTimerBottom + wallSolid	; store bottom timer tile in bottom row of timeMap
	sta tileMap + 22 * 23, x
	
	dex					; next column
	bne initTimerTilesHorizontal		; until all columns 21-1 are done

	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #lo(tileMap + 1 * 23)		; start at column 1 row 1 in tileMap
	sta tileMapAddr
	lda #hi(tileMap + 1 * 23)
	sta tileMapAddr + 1

	ldx #21					; do 21 copies of

.initTimerTilesVertical

	lda #mapTileTimerLeft + wallSolid	; store left timer tile in column 1 of tileMap
	ldy #0
	sta (tileMapAddr), y

	lda #mapTileTimerRight + wallSolid	; store right timer tile in column 22 of tileMap
	ldy #22
	sta (tileMapAddr), y

	clc					; move to next row
	lda #23
	adc tileMapAddr
	sta tileMapAddr
	bcc initTimerTilesNextRow
	inc tileMapAddr + 1

.initTimerTilesNextRow

	dex					; until rows 21-1 done
	bne initTimerTilesVertical
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #0					; zero enemy timer position
	sta enemyTimer

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; tileMapfindDot				find a random tileMap location that contains a dot and isnt near a turnstile
;						if location hasnt been found within 0.08 seconds then timeout
;						timeout happens because of a bad maze design with not enough spaces for objects
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			tileMapAddr		contains the address of the dot in the tileMap
;			carry			set if location found, clear if not found (timed out)
;			A			destroyed
;			X			preserved
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.tileMapFindDot

	lda #pause * 0.08			; set timeout for 0.08 seconds
	sta pauseCounter

	;---------------------------------------------------------------------------------------------------------------------------------------------

.tileMapFindDotY

	lda pauseCounter			; if timed out then exit with failed status
	beq tileMapFindDotFailed

	jsr random				; get random value 0-255 and mask to become 0-28 in steps of 4
	and #%00011100

	cmp #21					; if its higher than 20 then try again
	bcs tileMapFindDotY
	
	tay					; convert to tileMap row address
	lda tileMapRowsLo, y
	sta tileMapAddr
	lda tileMapRowsHi, y
	sta tileMapAddr + 1
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.tileMapFindDotX

	jsr random				; get random value 0-255 and mask it to become 0-28 in steps of 4
	and #%00011100

	cmp #21					; if its higher than 20 then try again
	bcs tileMapFindDotX
	
	adc tileMapAddr				; add to tileMapAddr so that it points to the top left of the 3x3 tile cube to investigate
	sta tileMapAddr				; (carry is clear so no need to use clc before adc)
	bcc tileMapFindDotCheck
	inc tileMapAddr + 1

	;---------------------------------------------------------------------------------------------------------------------------------------------

.tileMapFindDotCheck

	ldy #24					; if center tile does not contain a dot then pick another location
	lda (tileMapAddr), y
	cmp #mapTileDot
	bne tileMapFindDotY

	ldy #1					; if above center tile contains turnstile then pick another location
	lda (tileMapAddr), y
	and #wallSolid
	eor #wallTurnstile
	beq tileMapFindDotY
	
	ldy #47					; if below center tile contains turnstile then pick another location
	lda (tileMapAddr), y
	and #wallSolid
	eor #wallTurnstile
	beq tileMapFindDotY
	
	ldy #23					; if left of center tile contains turnstile then pick another location
	lda (tileMapAddr), y
	and #wallSolid
	eor #wallTurnstile
	beq tileMapFindDotY

	ldy #25					; if right of center tile contains turnstile then pick another location
	lda (tileMapAddr), y
	and #wallSolid
	eor #wallTurnstile
	beq tileMapFindDotY
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

	clc					; location found so adjust address to center tile that contained the dot
	lda #24
	adc tileMapAddr
	sta tileMapAddr
	bcc tileMapFindDotExit
	inc tileMapAddr + 1

	;---------------------------------------------------------------------------------------------------------------------------------------------

.tileMapFindDotExit

	sec					; return with location found status and tile address in tileMapAddr
	rts

	;---------------------------------------------------------------------------------------------------------------------------------------------

.tileMapFindDotFailed

	clc					; return with location failed status (timed out)
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; placeTileMapHearts				place 3 hearts at random locations in the map
;						exit if location not found within 0.08 seconds
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.placeTileMapHearts

	ldx #3					; 3 hearts

.placeTileMapHeartsLoop

	jsr tileMapFindDot			; pick a random tileMap location containing a dot that isnt near a turnstile

	bcc placeTileMapHeartsExit		; if location not found (bad maze design) then exit

	lda #mapTileHeart			; replace it with a heart
	ldy #0
	sta (tileMapAddr), y
	
	dex					; repeat until all hearts placed
	bne placeTileMapHeartsLoop
	
.placeTileMapHeartsExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; placeTileMapLetters				place 3 letters at random locations in the map
;						exit if location not found within 0.08 seconds
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.placeTileMapLetters

	ldx #3					; 3 letters

.placeTileMapLettersLoop

	jsr tileMapFindDot			; pick a random tileMap location containing a dot that isnt near a turnstile

	bcc placeTileMapLettersExit		; if location not found (bad maze design) then exit

	lda levelLetters - 1, x			; replace dot with a letter from the levelLetters table
	ldy #0					; use levelLetters - 1 address because x index is 3,2,1 not 2,1,0
	sta (tileMapAddr), y
	
	dex					; repeat until all letters placed
	bne placeTileMapLettersLoop

.placeTileMapLettersExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; placeTileMapSkulls				place skulls at random locations in the map, decrement edibles for each skull placed
;						exit if location not found within 0.08 seconds
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.placeTileMapSkulls

	ldx levelSkulls				; get number of skulls for level

.placeTileMapSkullsLoop

	jsr tileMapFindDot			; pick a random tileMap location containing a dot that isnt near a turnstile

	bcc placeTileMapSkullsExit		; if location not found (bad maze design) then exit

	lda #mapTileSkull			; replace it with a skull
	ldy #0
	sta (tileMapAddr), y
	
	dec levelEdibles			; and decrement number of edible objects

.placeTileMapSkullsNext

	dex					; repeat until all skulls placed
	bne placeTileMapSkullsLoop
	
.placeTileMapSkullsExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; initSprites					set all sprites as blanked and not moving
;						mark all sprites as erased
;						disable ladybug movement animation
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------


.initSprites

	ldx #spritesTotal - 1			; total number of sprites to initialise

.initSpritesLoop

	lda #spriteBlanking + moveStop		; sprite disabled (blanked and not moving)
	sta spritesDir, x

	lda #&ff				; sprite erase buffer disabled
	sta spritesErased, x

	dex
	bpl initSpritesLoop			; repeat until all sprites initialised

	lda #0					; no active enemies yet
	sta enemiesActive
	
	sta animateLadybugActive		; disable ladybug movement animation

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawScore					draw a single score digit and allow for leading zero blanking
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			drawScoreAddr		points to next tile position on screen
;			drawScoreIndex		points to the next digit index
;			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		drawDigitsEnable	flag for leading zero blanking
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawScore

	lda #score				; set address for score display
	sta drawScoreDigitRead + 1

	dec drawScoreIndex			; move to next digit
	bpl drawScoreDigit			; if index < 0 (all digits drawn)
	
	lda #5					; then index = 5 (first digit again)
	sta drawScoreIndex
	lda #0					; and also enable leading zero blanking
	sta drawScoreBlanking

						; set screen address to lower panel for score

	lda #lo(screenAddr + 16 * chrColumn + 24 * chrRow)
	sta drawScoreAddr
	lda #hi(screenAddr + 16 * chrColumn + 24 * chrRow)
	sta drawScoreAddr + 1

.drawScoreDigit

	lda drawScoreAddr			; copy digit address to tile draw address
	sta drawMapTileAddr
	lda drawScoreAddr + 1
	sta drawMapTileAddr + 1

	lda drawScoreIndex			; get digit index and divide by two to create byte index in x (2 digits per byte)
	lsr a					; carry now = odd (1) or even (0) digit
	tax
	
.drawScoreDigitRead

	lda dummy8, x				; get byte from score  (address previously setup)
	
	bcc drawScoreDigitPrint			; if carry is set (odd digit) then shift bits to get the upper bcd digit
	
	lsr a
	lsr a
	lsr a
	lsr a

.drawScoreDigitPrint

	and #%00001111				; just use lowest 4 bits

	beq drawScoreDigitCheckBlanking		; if digit != 0 then disable leading zero blanking
	dec drawScoreBlanking

.drawScoreDigitCheckBlanking

	bit drawScoreBlanking			; if leading zero blanking enabled (drawScoreBlanking < 0)
	bmi drawScoreNext

	lda #extraTileBlank			; then print a blank tile instead of digit 0

.drawScoreNext

	jsr drawExtraTile			; print digit tile
	
	lda drawMapTileAddr			; save screen address for next digit
	sta drawScoreAddr
	lda drawMapTileAddr + 1
	sta drawScoreAddr + 1
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawHighScore					draw highScore using 6 calls to drawscore
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawHighScore

	lda #highScore				; set address for highScore display
	sta drawScoreDigitRead + 1

	lda #5					; index = 5 (first digit of high score)
	sta drawScoreIndex
	lda #0					; enable leading zero blanking
	sta drawScoreBlanking

						; set screen address to lower panel for high score

	lda #lo(screenAddr + 16 * chrColumn + 25 * chrRow)
	sta drawScoreAddr
	lda #hi(screenAddr + 16 * chrColumn + 25 * chrRow)
	sta drawScoreAddr + 1

.drawHighScoreLoop

	jsr drawScoreDigit			; print a digit of high score
	
	dec drawScoreIndex			; move to next digit
	bpl drawHighScoreLoop			; repeat until all 6 digits done
	
	lda #0					; draw final 0 digit and return
	jmp drawExtraTile



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; eraseSprite					erase the sprite block of 10x14 pixels on screen
;						redraw the tile at the tail end of the sprite
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			X			sprite number (index into spritesErase table containing x, y, dir information)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.eraseSpriteTileX

	equb 7, 7, 14, 0			; x offset for tile behind sprite
	
.eraseSpriteTileY

	equb 14, 0, 7, 7			; y offset for tile behind sprite

	;---------------------------------------------------------------------------------------------------------------------------------------------

.eraseSprite

	stx eraseSpriteSaveX			; preserve registers
	sty eraseSpriteSaveY
	
	lda spritesEraseX, x			; erase 10x14 pixel area at sprite coordinates
	sta drawSpriteX
	lda spritesEraseY, x
	sta drawSpriteY
	jsr eraseBlock

	ldx eraseSpriteSaveX			; get saved sprite index

	lda spritesEraseDir, x			; if sprite was moving
	and #moveStop
	bne eraseSpriteExit

	lda spritesEraseDir, x			; get direction it was moving
	and #3
	tay
	
	lda spritesEraseX, x			; calculate the x offset of the tile behind the sprite
	clc
	adc eraseSpriteTileX, y
	sta spriteToAddrX
	
	lda spritesEraseY, x			; calculate the y offset of the tile behind the sprite
	clc
	adc eraseSpriteTileY, y
	sta spriteToAddrY
	jsr spriteToAddr			; get screenAddr and tileMapAddr for tile behind the sprite

	ldy #0					; get tile from map and if its not a blank tile
	lda (tileMapAddr), y
	cmp #mapTileBlank
	beq eraseSpriteAdjustAddress

	jsr drawMapTile				; then redraw it on screen
	jmp eraseSpriteCheckLeft
	
.eraseSpriteAdjustAddress

	lda #chrColumn				; else just advance the screen address without drawing the tile
	jsr drawMapTileAddrAdvance
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.eraseSpriteCheckLeft
	
	lda spritesEraseDir,x			; if sprite was moving left
	and #3
	cmp #moveLeft
	bne eraseSpriteCheckUp

	ldy #1					; then check the next tile to the right
	lda (tileMapAddr), y
	bmi eraseSpriteExit			; if its a maze tile then exit

	cmp #objectTileIndex			; if its not an object then then exit
	bcc eraseSpriteExit

	jsr drawMapTile				; else draw the object tile to prevent cropping a column on objects cause by the previous tile draw

.eraseSpriteExit

	ldy eraseSpriteSaveY			; restore registers
	ldx eraseSpriteSaveX

	rts					; return

	;---------------------------------------------------------------------------------------------------------------------------------------------

.eraseSpriteCheckUp
	
	lda spritesEraseDir, x			; if sprite was moving up
	and #3
	cmp #moveUp
	bne eraseSpriteExit

	lda spritesEraseY, x			; and if ladybug isnt on the bottom row
	cmp #20 * 8
	bcs eraseSpriteExit

	ldy #23					; then check tile below
	lda (tileMapAddr), y
	bmi eraseSpriteExit			; if its a maze tile then exit

	cmp #objectTileIndex			; if its an object tile
	bcc eraseSpriteExit

	clc					; adjust screen address to 1 tile below
	lda drawMapTileAddr
	adc #lo(chrRow - chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrRow - chrColumn)
	sta drawMapTileAddr + 1

	lda (tileMapAddr), y			; draw tile to prevent cropping the top of the object tile
	jsr drawMapTile

	jmp eraseSpriteExit			; restore registers and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; eraseBlock					erase a 10x14 block of pixels
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			spriteX			sprite coordinates for erasure
;			spriteY
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.eraseBlock

	jsr spriteToScreen			; convert sprite xy to screen address

	lda drawSpriteY				; setup screen address
	and #7
	ora drawSpriteScreenAddr
	sta drawSpriteScreenAddr
	sta eraseBlockWrite + 1
	lda drawSpriteScreenAddr + 1
	sta eraseBlockWrite + 2

	lda #spriteTileBytes			; store total number of bytes in sprite
	sta eraseBlockBytes	

.eraseBlockColumn

	ldy #spriteTileHeight			; y = sprite height for this pixel column

	lda drawSpriteScreenAddr		; x = 7 - row adjust counter
	and #7
	eor #7
	tax

	lda #pixels0				; fill byte = black

.eraseBlockWrite

	sta dummy16				; write to screen (address previously setup)

	dec eraseBlockBytes			; if all bytes written then exit
	beq eraseBlockExit

	inc eraseBlockWrite + 1			; inc screen write address
	bne eraseBlockNext
	inc eraseBlockWrite + 2
	
.eraseBlockNext

	dex					; check if we need to adjust for next row
	bpl eraseBlockContinue

	clc					; adjust write address to next row
	lda #lo(chrRow - 8)
	adc eraseBlockWrite + 1
	sta eraseBlockWrite + 1
	lda #hi(chrRow - 8)
	adc eraseBlockWrite + 2
	sta eraseBlockWrite + 2

	ldx #7					; reset row adjust counter

	lda #pixels0				; reload A with black

.eraseBlockContinue

	dey					; repeat until column done
	bne eraseBlockWrite

	clc					; adjust write address to next column
	lda drawSpriteScreenAddr
	adc #8
	sta drawSpriteScreenAddr
	sta eraseBlockWrite + 1
	lda drawSpriteScreenAddr + 1
	adc #0
	sta drawSpriteScreenAddr + 1
	sta eraseBlockWrite + 2

	bne eraseBlockColumn			; and do it again (bne used as branch always, high byte of addr is never 0)

.eraseBlockExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawExtraTile					draw tile to screen, move to next tile position
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			tile img to be drawn
;			drawMapTileAddr		current screen location for tile
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			drawMapTileAddr		points to next tile position on screen
;			A			preserved
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawExtraTile

	sta drawMapTileSaveA			; preserve registers
	sty drawMapTileSaveY

	tay					; convert tile img number to extra tile address
	lda extraTileAddrLo, y
	sta drawMapTileRead + 1
	lda extraTileAddrHi, y
	sta drawMapTileRead + 2

	lda #mapTileBytes - 1			; regular 6 pixel map tile
	sta drawMapTileTransfer + 1

	bne drawMapTileTransfer			; draw the extra tile



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawMapTile4Pixel				draw only 4 pixels wide tile skipping the last 2 pixel columns to screen, move to next tile position
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			tile img to be drawn (bits 7,6 not used)
;			drawMapTileAddr		current screen location for tile
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			drawMapTileAddr		points to next tile position on screen
;			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawMapTile4Pixel

	sta drawMapTileSaveA			; preserve registers
	sty drawMapTileSaveY

	and #%00111111				; remove bits 7,6

.drawMapTileCalcAddr4pixel

	tay					; convert to mapTile index to address
	lda mapTileAddrLo, y
	sta drawMapTileRead + 1
	lda mapTileAddrHi, y
	sta drawMapTileRead + 2

	lda #(mapTileBytes - 1) - 8		; 4 pixel wide map tile (regular tile size - 8 bytes)
	sta drawMapTileTransfer + 1

	jmp drawMapTileTransfer



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawMapTile					draw map tile to screen, move to next tile position
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			tile img to be drawn (bits 7,6 not used)
;			drawMapTileAddr		current screen location for tile
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			drawMapTileAddr		points to next tile position on screen
;			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawMapTile

	sta drawMapTileSaveA			; preserve registers
	sty drawMapTileSaveY

	and #%00111111				; remove bits 7,6

.drawMapTileCalcAddr

	cmp #objectTileIndex			; if its an object tile then use drawObjectTile instead
	bcs drawObjectTile
	
	tay					; convert to mapTile index to address
	lda mapTileAddrLo, y
	sta drawMapTileRead + 1
	lda mapTileAddrHi, y
	sta drawMapTileRead + 2

	lda #mapTileBytes - 1			; regular 6 pixel map tile
	sta drawMapTileTransfer + 1

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; transfer map tile data to screen
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawMapTileTransfer

	ldy #dummy8				; bytes to transfer (previously setup)
	
.drawMapTileRead

	lda dummy16, y				; read byte from tile (address previously setup)
	
.drawMapTileWrite

	sta dummy16, y				; write byte to screen (address previously setup)
	
.drawMapTileContinue

	dey					; repeat until 24 bytes copied
	bpl drawMapTileRead
	
.drawMapTileNext

	clc					; add tile bytes to screen address for next tile
	lda drawMapTileTransfer + 1
	adc #1
	adc drawMapTileWrite + 1
	sta drawMapTileWrite + 1
	bcc drawMapTileExit
	inc drawMapTileWrite + 2
	
.drawMapTileExit

	ldy drawMapTileSaveY			; restore registers
	lda drawMapTileSaveA

	rts					; return

	drawMapTileAddr = drawMapTileWrite + 1



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawObjectTile				draw 8 pixel wide object tile to screen, move to next tile position
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			tile img to be drawn
;			drawObjectTileAddr	current screen location for tile
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			drawObjectTileAddr	points to next tile position on screen
;			A			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawObjectTile

	sec					; subtract starting index so objectTile is indexed from 0
	sbc #objectTileIndex

	tay					; convert to objectTile index to address
	lda objectTileAddrLo, y
	sta drawMapTileRead + 1
	lda objectTileAddrHi, y
	sta drawMapTileRead + 2

	sec					; offset drawMapTileAddress -1 column because object tiles are 8 pixels wide
	lda drawMapTileAddr
	sbc #8
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	sbc #0
	sta drawMapTileAddr + 1

	lda #objectTileBytes - 1		; number of bytes to transfer from tile to screen
	sta drawMapTileTransfer + 1

	bne drawMapTileTransfer			; go draw the tile (bne used as branch always)



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawBcd					draw A as 2 digits of BCD characters
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			value to display as hex
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			drawChrAddr		points to next chr position on screen
;			A			destroyed
;			X			preserved
;			y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		stack			1 byte stack space
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawBcd

	pha					; save value
	
	lsr a					; draw high nybble bcd digit
	lsr a
	lsr a
	lsr a
	ora #'0'
	jsr drawChr
	
	pla					; get value
	
	and #%00001111				; draw low nybble bcd digit
	ora #'0'

	; continue into drawChr wth low nybble



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawChr					draw chr to screen, move to next chr position ready for next
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			chr to be drawn
;			drawChrAddr		current screen location for chr
;			drawChrColor		bit mask for pixel colors
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			drawChrAddr		points to next chr position on screen
;			A			destroyed
;			X			preserved
;			y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		drawChrFontData		data read from font to be converted to pixels
;-----------------------------------------------------------------------------------------------------------------------------------------------------

; chrs are 6 x 6 pixels stored in pairs from top to bottom, left to right order using 5 bytes per character, the last 4 bits are unused
;
; [byte,bit]
;
; [0,7][0,6] [1,3][1,2] [3,7][3,6]
; [0,5][0,4] [1,1][1,0] [3,5][3,4]
; [0,3][0,2] [2,7][2,6] [3,3][3,2]
; [0,1][0,0] [2,5][2,4] [3,1][3,0]
; [1,7][1,6] [2,3][2,2] [4,7][4,6]
; [1,5][1,4] [2,1][1,0] [4,5][4,4]

	;---------------------------------------------------------------------------------------------------------------------------------------------

pixelLeft		= &aa			; bit mask for left pixel
pixelRight		= &55			; bit mask for right pixel

	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawChr

	stx drawChrSaveX			; save registers
	sty drawChrSaveY

	ldy drawChrColor			; store chr color
	sty drawChrWriteColor + 1

	ldx #0					; zero bit counter
	
	stx drawChrFontRead + 2			; zero high byte of chr value

	sec					; set character code origin to 0 (ascii 32-95 becomes 0-63 00-3f)
	sbc #' '

	sta drawChrFontRead + 1			; multiply A by 5 (5 bytes per character)
	asl a
	asl a
	adc drawChrFontRead + 1			; no need for clc as carry is already clear
	sta drawChrFontRead + 1
	bcc drawChrFont
	inc drawChrFontRead + 2
	
.drawChrFont

	clc					; add font address to chr index
	lda drawChrFontRead + 1
	adc #lo(fontBin)
	sta drawChrFontRead + 1
	lda drawChrFontRead + 2
	adc #hi(fontBin)
	sta drawChrFontRead + 2

	ldy #6					; 6 lines per column
	
.drawChrLoop

	txa					; every 4 pairs get a byte from chr table
	and #3
	bne drawChrPixelLeft
	
.drawChrFontRead

	lda dummy16				; read 4 pixel pairs from font and save it (address previously setup)
	sta drawChrFontData

	inc drawChrFontRead + 1			; bump font read address
	bne drawChrPixelLeft
	inc drawChrFontRead + 2

.drawChrPixelLeft

	lda #&00				; get 1 bit from chr data and convert to left pixel
	asl drawChrFontData
	bcc drawChrPixelRight
	ora #pixelLeft

.drawChrPixelRight

	asl drawChrFontData			; get 1 bit from chr data and convert to right pixel and combine with left pixel
	bcc drawChrWriteColor
	ora #pixelRight

.drawChrWriteColor	

	and #dummy8				; mask pixels with color value previously stored here

.drawChrWriteScreen

	sta dummy16				; write to screen (address previously setup)

.drawChrNextLine

	inc drawChrWriteScreen + 1		; next line down
	bne drawChrWriteTest
	inc drawChrWriteScreen + 2

.drawChrWriteTest

	inx					; bump pixel pair counter

	dey					; all 6 lines done yet ?
	bne drawChrLoop
	
	ldy #6					; reset line counter

	inc drawChrWriteScreen + 1		; move 2 lines down from top of this new column
	inc drawChrWriteScreen + 1
	
.drawChrNext

	cpx #(6 * 6) / 2			; are all pixels done yet ? (6 * 6 pixels / 2 (left, right))
	bcc drawChrLoop				; if not then go back and process more
	
	ldy drawChrSaveY			; restore registers
	ldx drawChrSaveX

	rts					; return

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; alias to self modifying code address
;-----------------------------------------------------------------------------------------------------------------------------------------------------

drawChrAddr		= drawChrWriteScreen + 1; screen address to write chr








;*****************************************************************************************************************************************************
;
; main						entry point to program
;
;*****************************************************************************************************************************************************

.main

	cli					; enable interrupts

	jsr swrInitScreen			; full screen erase, setup palette colors (in loader.asm)
	
	lda #pause * 0.5			; wait 0.5 seconds
	sta pauseCounter
	
.mainPauseLoop

	lda pauseCounter			; wait until pause time has expired
	bne mainPauseLoop

	lda #6					; enable display
	sta crtcAddr
	lda #screenHeight
	sta crtcData

	jsr drawPlayfieldUpper			; display the upper playfield

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; display the main screen with player options, wait for start to be pressed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.gameIntroScreen

	jsr updateHighScoreFirstPlace		; copy first place high score from table and display it

	jsr mainMenu				; display the main menu screen



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; setup a new game for level 1
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.gameStartNew

	lda #&01				; start game on level 1
	sta level

	lda #centerCucumber			; start with a cucumber at 1000 points
	sta vegetableImg
	lda #&10
	sta vegetableScore

	lda #0					; no shield at start, ladybug is vulnerable to skulls
	sta shield

	sta score				; zero the player score
	sta score + 1
	sta score + 2

	sta mazeMap				; start with mazeMap 0

	lda optionLives				; initialize player lives
	sta lives

	lda #&ff				; clear the special and extra bonus flag bits
	sta bonusBits
	sta bonusBits + 1

	sta ladybugEntryEnable			; enable ladybug entry movement animation

	sta bonusDiamondEnable			; enable the possibility of getting a diamond bonus



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup level here too so that instructions page shows correct settings
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr initLevelSettings			; setup skulls, letters, enemy settings etc for current level

	jsr drawPlayfieldUpperBonus		; update the upper bonus top panel

	jsr drawPlayfieldLower			; draw playfield lower section (info panel)

	jsr instructions			; display the game instructions
	bcc gameIntroScreen			; return to intro screen if esc was pressed

	lda #sfxTwinkle				; play twinkle sound effect for first level
	jsr playSound



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; setup current level ready for play
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.gameLevelStart

	jsr initLevelSettings			; setup skulls, letters, enemy settings etc for current level

	jsr drawPlayfieldUpperBonus		; update the upper bonus top panel (multipliers are cleared for new round)

	jsr drawPlayfieldLower			; draw playfield lower section (info panel)

	jsr drawLevelIntro			; draw the level intro screen

	jsr initPlayfieldMiddle			; initialize playfield middle section tileMap and count number of edible items (dots, hearts, letters)

	lda levelEdibles			; if there are no edible objects (bad maze design)
	bne gameLevelRestart
	jmp gameOver				; then end the game

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; restart current level
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.gameLevelRestart

	jsr drawPlayfieldLowerLives		; update the lives value on screen

	jsr initSprites				; disable all sprites and mark all erased

	lda lives				; if lives = 0 then its game over
	bne gameLevelRestartDrawMiddle
	jmp gameOver

.gameLevelRestartDrawMiddle

	jsr initTimerTiles			; fill tileMap edges with timer tiles
	
	jsr drawPlayfieldMiddle			; draw playfield middle section from tileMap

	jsr enemySpawn				; spawn 1st enemy in center box ready to be active on release timer

	jsr ladybugSpawn			; spawn ladybug

	lda #0					; unpause ladybug
	sta pauseLadybug
	
	sta bonusItemActive			; deactivate center bonus item

	sta vegetableScoreActive		; deactivate center vegetable score display

	sta objectScoreImg			; deactivate object score display

	sta enemyReleaseEnable			; disable enemy release

	lda #escTime				; reset esc key timer
	sta escCounter



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; game loop					wait interrupts, draw graphics, update sprites and game data. everything !
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; wait for timer middle interrupt then process stuff for upper half of screen
	;---------------------------------------------------------------------------------------------------------------------------------------------

.gameLoopUpper

	jsr waitVsyncUpper			; wait for vsync interrupt for upper area (6845 vsync interrupt), read analogue joystick (if enabled)

	jsr ladybugEntryAnimation		; draw ladybug entry movement animation if enabled

	jsr checkBonus				; check if special, extra or diamond bonus screens are required
	bcs gameLevelStart			; if bonus was awarded then start a new level

	jsr checkLevelEnd			; check if current level has ended
	bcs gameLevelStart			; if level has ended then start new level

	jsr drawBonusItemCenter			; draw the vegetable/diamond in the center bug box (if active)

	jsr redrawSprites			; erase and redraw sprites in upper area

	jsr ladybugDeathAnimation		; draw ladybug death movement animation (if enabled)
	bcs gameLevelRestart			; restart level if ladybug death movement animation has just completed

	jsr drawVegetableScore			; draw the vegetable bonus score in center (if active)

	jsr drawObjectScore			; draw object score (if active)

	jsr drawTurnstile			; draw new turnstile position (if active)

	jsr updateSkullColor			; update the skull palette color

	jsr updateBonusColor			; update the bonus letters palette colors

	jsr checkPauseGame			; if game is paused then skip sprite movement and timer stuff
	bcs gameLoopLower

	jsr updateLadybug			; update ladybug direction, handle turnstile and object detection

	jsr moveSprites				; move all sprites, handle enemy collision with skulls and ladybug collision with enemys

	jsr updateObjectTimer			; update object timer, object mode and palette

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; wait for vsync top interrupt and process stuff for lower half of screen
	;---------------------------------------------------------------------------------------------------------------------------------------------

.gameLoopLower

	jsr waitVsyncLower			; wait for vsync interrupt for lower area (6522 timer1 interrupt), read analogue joystick (if enabled)
	
	jsr processSound			; process sound effects and music

	jsr redrawSprites			; erase and redraw sprites in lower area
	
	jsr ladybugDeathAnimation		; draw ladybug death movement animation (if enabled)
	bcs gameLevelRestart			; restart level if ladbug death animation has just completed

	jsr drawVegetableScore			; draw the vegetable bonus score in center (if active)

	jsr drawObjectScore			; draw object score (if active)

	jsr updateAnimationFrame		; update the sprite animation frame number

	jsr drawScore				; draw a single score digit, move to next score digit ready for next time around

	jsr inputScan				; read keyboard input and joystick (if enabled)

	jsr checkEsc				; if we need to quit the game (esc held)
	bcc gameLoopLowerCheckPause
	jmp gameIntroScreen			; then jump back to the game intro screen

	;---------------------------------------------------------------------------------------------------------------------------------------------

.gameLoopLowerCheckPause

	jsr checkPauseGame			; if game is paused then skip enemy timer, enemy release, enemy/ladybug pause timers
	bcs gameLoopUpper

	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr updateEnemyTimer			; update the enemy timer and draw the new timer tile when needed

	jsr enemyRelease			; release an enemy (if enabled)

	jsr updatePauseTimers			; update ladybug and enemy pause timers, also handles erasure of object score and bonus item score

	jmp gameLoopUpper			; loop back to game main loop



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; checkEsc					check esc key
;						if not pressed then reset esc counter
;						if pressed then return true if counter = 0
;						else decrement counter and return false
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.checkEsc

	lda playerInput				; if esc key not pressed
	cmp #keyBitEsc
	beq checkEscPressed
	
	lda #escTime				; then reset esc counter
	sta escCounter
	
.checkEscReturnFalse

	clc					; return false
	rts
	
.checkEscPressed

	dec escCounter				; else decrement escCounter
	bne checkEscReturnFalse			; if escCounter != 0 then return false
	
.checkEscReturnTrue

	jsr playSoundSilence			; esc time has passed so terminate any current sound effects or music

	lda #sfxSkull				; play skull sound
	jsr playSound

	sec					; return true
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; gameOver					clear the screen and display game over message
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.gameOver

	lda #gameOverTime			; set display time
	sta pauseCounter

	jsr playfieldMiddleWithTimer		; initialize and draw empty playfield with timer, initialize all sprites as blanked and erased

	jsr drawString				; draw game over message
	equb pixels1
	equw screenAddr + 2 + 7 * chrColumn + 12 * chrRow
	equs "GAME OVER", &ff
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; process some color stuff and wait for the pauseCounter to expire
	;---------------------------------------------------------------------------------------------------------------------------------------------

.gameOverLoop

	jsr waitVsyncUpper			; wait upper half, read analogue joystick (if enabled)
	jsr waitVsyncLower			; wait lower half, read analogue joystick (if enabled)

	jsr updateBonusColor			; update the bonus letters palette colors

	lda pauseCounter			; repeat until time expires
	bne gameOverLoop

	jsr checkHighScore			; check if highScore was beaten (handles score position and the high score entry)

	jmp gameIntroScreen			; return to game intro screen



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; checkHighScore				check high score table and if score can be entered then call the name registration function
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.checkHighScore

	lda #lo(highScoreTable)			; start with 1st place
	sta highScorePtr
	lda #hi(highScoreTable)
	sta highScorePtr + 1
	
	ldx #7					; 8 entrys to check (7 to 0)

.checkHighScoreLoop

	ldy #0

	sec					; subtract score from highScore in table
	lda (highScorePtr), y
	sbc score + 0
	iny
	lda (highScorePtr), y
	sbc score + 1
	iny
	lda (highScorePtr), y
	sbc score + 2
	bcc checkHighScoreEntry			; if there was a borrow then score > current score we are checking so do the name registration and return
	
	lda #14					; else move to the next score in the table
	clc					; cannot cross page boundry so no need to adjust high byte
	adc highScorePtr
	sta highScorePtr
	
	dex					; and repeat until end of table
	bpl checkHighScoreLoop

	rts					; score didnt qualify for the high score table so just return
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; shift high score table down if needed to make room for entry
	;---------------------------------------------------------------------------------------------------------------------------------------------

.checkHighScoreEntry

	cpx #0					; if the entry is last in the table then no need to shift scores, go directly to registration and exit
	beq checkHighScoreRegister

	ldy #lo(highScoreTableEnd - 15)

.checkHighScoreShift				; else shift high score entrys down to make room for high score entry

	lda hi(highScoreTable) * 256, y		; shift memory
	sta hi(highScoreTable) * 256 + 14, y
	
	dey
	cpy highScorePtr
	bne checkHighScoreShift

	lda hi(highScoreTable) * 256, y		; shift last byte
	sta hi(highScoreTable) * 256 + 14, y
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; enter name into table
	;---------------------------------------------------------------------------------------------------------------------------------------------

.checkHighScoreRegister

	jsr nameReg				; get name registration from player

	jsr generateValidation			; update the validation code

	lda #sfxMusicLetters			; play high score music
	jsr playSound
	
	jsr updateHighScoreFirstPlace		; update lower playfield with first place high score

	jmp drawScoreTable			; draw the high score page and return
	


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; playfieldMiddleWithTimer			initialize all sprites as blanked and erased
;						clear tilemap
;						fill tilemap with timer
;						draw tilemap
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.playfieldMiddleWithTimer

	jsr initSprites				; initialize all sprites and blanked and erased

	jsr clearTileMap			; fill tilemap with blank tile
	
	jsr initTimerTiles			; fill edges with timer tiles
	
	jmp drawPlayfieldMiddle			; draw the middle playfield and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; ladybugEntryAnimation
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.ladybugEntryAnimation

	lda ladybugEntryEnable			; if ladybug entry movement animation enabled
	beq ladybugEntryAnimationExit

	lda spritesX + 0			; if ladybug at entry position
	cmp #ladybugEntryX
	bne ladybugEntryAnimationNext

	lda spritesY + 0
	cmp #ladybugEntryY
	bne ladybugEntryAnimationNext
	
	lda #sfxMusicEntry			; play the entry music
	jsr playSound

	lda #ladybugEntryTime			; pause enemy timer during ladybug entry movement animation
	sta pauseEnemy

.ladybugEntryAnimationNext

	jsr animateLadybug			; do the movement animation
	
	lda spritesX + 0			; if ladybug at start position
	cmp #ladybugStartX
	bne ladybugEntryAnimationExit
	
	lda spritesY + 0
	cmp #ladybugStartY
	bne ladybugEntryAnimationExit
	
	lda #0					; then disable entry movement animation
	sta ladybugEntryEnable

.ladybugEntryAnimationExit

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; checkLevelEnd					check levelEnd flag and levelEdibles, trigger an end if needed (set the carry flag)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.checkLevelEnd

	lda levelEnd				; if level has ended then advance level and exit with true status
	bne checkLevelEndTrue

	lda levelEdibles			; if theres still edible objects then exit with false status
	bne checkLevelEndFalse

	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #endLevelTime			; pause ladybug and enemies
	sta pauseLadybug
	sta pauseEnemy

	lda soundTimers + 0			; if sound effect not playing on channel 0 (music)
	bne checkLevelEndFalse

	lda soundTimers + 3			; and if sound effects not playing on channel 3 (object)
	bne checkLevelEndFalse

	lda #&ff				; flag level as ended
	sta levelEnd

	lda #sfxEndLevel			; play end of level sound
	jsr playSound

	;---------------------------------------------------------------------------------------------------------------------------------------------

.checkLevelEndFalse

	clc					; flag end level as false and return
	rts

	;---------------------------------------------------------------------------------------------------------------------------------------------

.checkLevelEndTrue

	lda pauseLadybug			; if pause is over (ladybug and enemy unpaused)
	ora pauseEnemy
	bne checkLevelEndFalse

	jsr levelAdvance			; advance game to next level

	sec					; flag end level as true and return
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; moveSprites					update coordinates of all sprites
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		moveSpritesIndex	index to current sprite
;			moveSpritesPathCounter	count the number of paths from sprite location to check for valid junction
;
;			moveSpritesSaveDirection
;						temporary storage for sprite direction
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; enemy control table
	;---------------------------------------------------------------------------------------------------------------------------------------------

moveSpritesJunctionPaths = 3			; must be at least this number of paths at a grid location to be valid junction
						; at a valid junction an enemy will either turn to ladybugs direction (attack)
						; or will choose a random available direction
						; if the attack direction is not available then a random available direction is chosen

	;---------------------------------------------------------------------------------------------------------------------------------------------

.enemyRandomChance

	equb (99.96 * 256) / 100		; percentage chance of enemy turning randomly instead of towards ladybug
	equb (91.63 * 256) / 100		; enemy attack value on game menu (0-9) is added to the enemy number (0-3)
	equb (83.30 * 256) / 100		; and used as an index into this table to give the enemy its attack strength
	equb (74.97 * 256) / 100
	equb (66.64 * 256) / 100
	equb (58.31 * 256) / 100
	equb (49.98 * 256) / 100
	equb (41.65 * 256) / 100
	equb (33.32 * 256) / 100
	equb (20.00 * 256) / 100
	equb (15.00 * 256) / 100
	equb (10.00 * 256) / 100
	equb (05.00 * 256) / 100

	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesDirX

	equb 0, 0, -1 , 1			; X up down left right

.moveSpritesDirY

	equb -1, 1, 0, 0			; Y up down left right
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; move all sprites 1 pixel
	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSprites

	ldx #0					; start at sprite index 0 (ladybug)
	jsr moveSpritesPixel			; move ladybug and enemies (if they're enabled to move)
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; move enemy sprites an extra pixel if required (enemy speed)
	;---------------------------------------------------------------------------------------------------------------------------------------------

	clc					; enemy speed fraction counter see if we need to move the enemies another pixel
	lda enemySpeedCounter
	adc enemySpeed
	sta enemySpeedCounter
	bcs moveSpritesEnemy			; if carry generated (extra move required for enemies)
	jmp moveSpritesExit
	
.moveSpritesEnemy

	ldx #1					; then start at sprite index 1 (enemies) and move them again

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; move sprites
	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesPixel	

	stx moveSpritesIndex			; set the starting sprite index

	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesLoop

	ldx moveSpritesIndex			; get current sprite index

	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesIsLadybugEnabled

	cpx #0					; if index = 0 (ladybug) and ladybug is paused then skip it
	bne moveSpritesIsEnemyEnabled
	lda pauseLadybug
	beq moveSpritesGetDirection
	jmp moveSpritesNext
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesIsEnemyEnabled

	lda pauseEnemy				; else if enemies are paused then skip them all
	beq moveSpritesGetDirection
	jmp moveSpritesExit

	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesGetDirection

	lda spritesDir, x			; if sprite is blanked or not moving then skip it and go on to next sprite
	and #spriteBlanking + moveStop
	beq moveSpritesDirection
	
	jmp moveSpritesNext

	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesDirection

	ldy spritesDir, x			; get sprite direction

	clc					; update sprite x from direction table
	lda spritesX, x
	adc moveSpritesDirX, y
	sta spritesX, x
	sta spriteToAddrX			; save sprite x for tileMapAddr conversion

	clc					; update sprite y from direction table
	lda spritesY, x
	adc moveSpritesDirY, y
	sta spritesY, x
	sta spriteToAddrY			; save sprite y for tileMapAddr conversion

	cpx #0					; if this is sprite 0 (ladybug) then skip checks and move onto next sprite
	bne moveSpritesCollision
	jmp moveSpritesNext

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; enemy collision check with ladybug
	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesCollision

	lda spritesDir + 0			; if ladybug is active (not blanked) then
	and #spriteBlanking
	bne moveSpritesCheckAlignmentX

	lda spritesX, x				; if ladybugX is close enough to enemyX .. abs(enemyX - ladybugX) < ladybugEnemyRange
	sec
	sbc spritesX + 0
	bcs moveSpritesCollisionX
	eor #&ff
	adc #1					; carry is clear, clc not needed

.moveSpritesCollisionX

	cmp #ladybugEnemyRange
	bcs moveSpritesCheckAlignmentX

	lda spritesY, x				; and if ladybugY is close enough to enemyY .. abs(enemyY - ladybugY) < ladyBugEnemyRange
	sec
	sbc spritesY + 0
	bcs moveSpritesCollisionY
	eor #&ff
	adc #1					; carry is clear, clc not needed

.moveSpritesCollisionY

	cmp #ladybugEnemyRange
	bcs moveSpritesCheckAlignmentX

	jsr ladybugKill				; then ladybug and enemy have collided so kill ladybug

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; enemy collision check with skull
	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesCheckAlignmentX

	lda spritesX, x				; if enemy spriteX not on grid then skip to next sprite .. spriteX & 15 != 8
	and #15
	cmp #8
	beq moveSpritesCheckAlignmentY
	jmp moveSpritesNext

.moveSpritesCheckAlignmentY

	lda spritesY, x				; if enemy spriteY not on grid then skip to next sprite .. spriteY & 15 != 8)
	and #15
	cmp #8
	beq moveSpritesGetMapAddr
	jmp moveSpritesNext

.moveSpritesGetMapAddr

	jsr spriteToAddr			; sprite is grid aligned so convert sprite XY to tileMapAddr

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check if enemy hit a skull
	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesCheckSkull

	ldy #24					; if tile under enemy = skull
	lda (tileMapAddr), y
	cmp #mapTileSkull
	bne moveSpritesCheckValidJunction

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; enemy hit a skull so remove skull from map, remove skull from screen, kill enemy and spawn new enemy in center box
	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesKillEnemy

	lda #mapTileBlank			; erase skull from map
	sta (tileMapAddr), y

	jsr offsetDrawMapTileAddr		; adjust mapTileAddress to be location underneath sprite center

	lda #mapTileBlankObj			; erase skull from screen
	jsr drawMapTile

	lda #sfxSkull				; play skull sound
	jsr playSound

	dec enemiesActive			; reduce number of active enemies
	
	lda #spriteBlanking			; deactivate the current enemy
	ora spritesDir, x
	sta spritesDir, x

	jsr enemySpawn				; spawn enemy in center box

	jmp moveSpritesNext			; skip to next sprite

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; enemy ai, check for valid number of paths
	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesCheckValidJunction

	lda #moveSpritesJunctionPaths - 1	; set the number of paths for a valid junction
	sta moveSpritesPathCounter
	
	ldx #3					; check in 4 directions, count how many paths
	
.moveSpritesCountPathsLoop

	ldy moveDirMap, x			; read the tileMap
	lda (tileMapAddr), y
	bmi moveSpritesCountPathsNext		; if its a path then decrement the path counter
	dec moveSpritesPathCounter

.moveSpritesCountPathsNext
	
	dex					; repeat until all 4 directions checked
	bpl moveSpritesCountPathsLoop

	ldx moveSpritesIndex			; get current sprite index
	ldy spritesDir, x			; get current direction

	bit moveSpritesPathCounter		; if not a valid junction (not enough paths)
	bpl moveSpritesFindPath			; then go check current direction is a path

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; enemy ai, choose attack direction or random direction
	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesRandomOrAttack

	stx moveSpritesSaveX			; preserve enemy index

	txa					; enemyAttack tableIndex = optionEnemyAttack + (spriteNumber and 3) 
	and #3
	clc
	adc optionEnemyAttack
	tax

	jsr random				; compare random number with enemyRandomChance table
	cmp enemyRandomChance, x

	ldx moveSpritesSaveX			; restore enemy index

	bcs moveSpritesAttack			; if random < random chance table

.moveSpritesRandom

	jsr random				; then pick a random direction
	and #&03
	tay

	bpl moveSpritesFindPath			; go check if this direction is a path (bpl use as branch always)
	
.moveSpritesAttack

	jsr random				; else choose random attack check order, either vertical/horizontal or horizontal/vertical
	bmi moveSpritesAttackReversedOrder

	jsr moveSpritesCheckHorizontal		; check horizontal
	jsr moveSpritesCheckVertical		; check vertical
	jmp moveSpritesFindPath
	
.moveSpritesAttackReversedOrder

	jsr moveSpritesCheckVertical		; check vertical
	jsr moveSpritesCheckHorizontal		; check horizontal

.moveSpritesFindPath

	sty moveSpritesSaveDirection		; save the chosen direction
	
	lda moveDirMap, y			; get direction offset for tile map

	tay					; if theres a wall at the chosen direction then choose a random direction instead
	lda (tileMapAddr), y
	bmi moveSpritesRandom

	lda moveSpritesSaveDirection		; get the saved chosen direction
	sta spritesDir, x			; set enemy sprite direction

.moveSpritesNext

	inc moveSpritesIndex			; repeat until all sprites have been processed
	lda moveSpritesIndex
	cmp #spritesTotal
	beq moveSpritesExit
	jmp moveSpritesLoop

.moveSpritesExit

	rts					; return



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; if enemy is below ladybug then return with up direction, if above then return down direction else no change in direction
	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesCheckVertical

	lda spritesY, x				; if enemyY = ladybugY then return with current direction unchanged
	cmp spritesY + 0
	beq moveSpritesCheckVerticalExit

	ldy #moveDown				; else choose down or up
	bcc moveSpritesCheckVerticalExit
	ldy #moveUp

.moveSpritesCheckVerticalExit

	rts



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; if enemy is left of ladybug then return right direction, if right of ladybug then return left direction else no change in direction
	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesCheckHorizontal

	lda spritesX, x				; if enemyX = ladybugX then return with current direction
	cmp spritesX + 0
	beq moveSpritesCheckHorizontalExit

	ldy #moveRight				; else choose right or left
	bcc moveSpritesCheckHorizontalExit
	ldy #moveLeft

.moveSpritesCheckHorizontalExit

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawPlayfieldUpper				draws the top red, yellow and cyan bars with text "special" "extra" "x2 x3 x5"
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawPlayfieldUpperBoxData

	equw screenAddr + 0			; screen position
	equb 1					; tile count
	equb extraTileUpper + 0			; red left
	
	equw screenAddr + 1 * chrColumn
	equb 7
	equb extraTileUpper + 1			; red center
	
	equw screenAddr + 16 + 7 * chrColumn
	equb 1
	equb extraTileUpper + 2			; red right
	
	equw screenAddr + 16 + 8 * chrColumn
	equb 1
	equb extraTileUpper + 3			; yellow left

	equw screenAddr + 16 + 9 * chrColumn
	equb 5
	equb extraTileUpper + 4			; yellow center
	
	equw screenAddr + 8 + 14 * chrColumn
	equb 1
	equb extraTileUpper + 5			; yellow right

	equw screenAddr + 8 + 15 * chrColumn
	equb 1
	equb extraTileUpper + 6			; cyan left
	
	equw screenAddr + 8 + 16 * chrColumn
	equb 6
	equb extraTileUpper + 7			; cyan center

	equw screenAddr + 22 * chrColumn
	equb 1
	equb extraTileUpper + 8			; cyan right

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw the red, yellow and cyan boxes plus the 3 'x' multiplier characters
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawPlayfieldUpper

	ldy #0					; draw the red, yellow, cyan boxes
	
.drawPlayfieldUpperGetData

	lda drawPlayfieldUpperBoxData + 0, y	; get screen address
	sta drawMapTileAddr + 0
	lda drawPlayfieldUpperBoxData + 1, y
	sta drawMapTileAddr + 1

	ldx drawPlayfieldUpperBoxData + 2, y	; get tile count
	
	lda drawPlayfieldUpperBoxData + 3, y	; get tile

.drawPlayfieldUpperLoop

	jsr drawExtraTile			; draw the tile
	
	dex					; repeat until all done
	bne drawPlayfieldUpperLoop
	
	iny					; move to next entry in list
	iny
	iny
	iny

	cpy #9 * 4				; repeat until all done
	bne drawPlayfieldUpperGetData

	jsr drawString				; draw the 3 'x' multipliers
	equb pixels6
	equw screenAddr + 2 + 16 * chrColumn
	equs chrMultiplierX, ' ', chrMultiplierX, ' ', chrMultiplierX, &ff

	; contine to drawPlayfieldUpperBonus
	


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawPlayfieldUpperBonus			draws the bonus letters and multipliers in the correct colors from bonusBits flags
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		bonusBitsCopy		copy of bonus bits while processing letter colors
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawPlayfieldUpperBonus

	stx drawPlayfieldUpperBonusSaveX	; save x

	jsr drawString				; set address
	equb pixels7
	equw screenAddr + 2 + 16
	equb &ff
	
	lda bonusBits				; copy bonus bits (shifting everything left one bit to drop the unused bit 15)
	asl a
	sta bonusBitsCopy
	lda bonusBits + 1
	rol a
	sta bonusBitsCopy + 1

	ldx #0					; index for bonus tables

.drawPlayfieldUpperText

	lda #pixels7				; if bit = 1 use white
	bit bonusBitsCopy + 1
	bmi drawPlayfieldUpperTextChr
	lda upperBonusColor, x			; else bit = 0 so use color from table

.drawPlayfieldUpperTextChr

	sta drawChrColor			; store color

	lda upperBonusText, x			; get chr from table and print it
	jsr drawChr
	
	lda upperBonusOffset, x			; add chr offset from table
	clc
	adc drawChrAddr
	sta drawChrAddr
	bcc drawPlayfieldUpperTextShiftBits
	inc drawChrAddr + 1
	
.drawPlayfieldUpperTextShiftBits

	asl bonusBitsCopy			; shift to next bonus bit
	rol bonusBitsCopy + 1

	inx					; repeat until all chrs done
	cpx #15
	bne drawPlayfieldUpperText

	ldx drawPlayfieldUpperBonusSaveX	; restore x

	rts					; return

	;---------------------------------------------------------------------------------------------------------------------------------------------

.upperBonusText

	equs "SPECIAL"
	equs "EXTRA"
	equs chrMultiplier2, chrMultiplier3, chrMultiplier5
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.upperBonusOffset

	equb 0,0,0,0,0,0,40			; special offsets
	equb 0,0,0,0,64				; extra offsets
	equb 24,24,0				; *2 *3 *5 offsets (last byte is used but has no effect as no more chrs printed so could be removed)

	;---------------------------------------------------------------------------------------------------------------------------------------------

.upperBonusColor				; colors for special, extra, 235

	equb pixelsSpecial0, pixelsSpecial1, pixelsSpecial0, pixelsSpecial1, pixelsSpecial0, pixelsSpecial1, pixelsSpecial0
	equb pixelsExtra1, pixelsExtra0, pixelsExtra1, pixelsExtra0, pixelsExtra1
	equb pixels6, pixels6, pixels6

	;---------------------------------------------------------------------------------------------------------------------------------------------

bonusBitsSpecial	= &7f			; bit mask for special bits on bonusBits + 1
bonusBitsExtra		= &f8			; bit mask for extra bits on bonusBits + 0
bonusBitsMultiplier	= &07			; bit mask for x2x3x5 multiplier bits on bonusBits + 0



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawPlayfieldLower				draws the bottom info panel showing
;						lady bug, lives, vegetable, vegetable score, level, score, highScore, highScore name
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawPlayfieldLower

	lda spriteBaseImg			; ladybug sprite image
	clc
	adc #10
	sta drawSpriteImg

	lda #3					; at lower left corner
	sta drawSpriteX
	lda #(screenHeight - 3) * 8 + spriteToAddrOffset
	sta drawSpriteY

	jsr drawSprite

	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawPlayfieldLowerLives		; draw 2 digit lives value

	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #34					; draw vegetable image to the right of ladybug
	sta drawSpriteX
	lda #23 * 8 + 6
	sta drawSpriteY

	jsr drawBonusItemVegetable

	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawString				; set screen address for vegetable score and color to green
	equb pixels2
	equw screenAddr + 2 + 16 + 5 * chrColumn + 24 * chrRow
	equb &ff

	lda vegetableScore			; draw 2 digit vegetable score in green
	jsr drawBcd

	lda #&00				; draw 2 digit 00 in green
	jsr drawBcd

	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawString				; draw "L" in blue
	equb pixels4
	equw screenAddr + 2 + 8 + 10 * chrColumn + 24 * chrRow
	equs "L", &ff

	lda #pixels5				; set color to magenta
	sta drawChrColor

	lda level				; draw 2 digits level number
	jsr drawBcd

	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawString				; set screen position, set color to yellow, draw "1P"
	equb pixels3
	equw screenAddr + 2 + 16 + 13 * chrColumn + 24 * chrRow
	equs "1P", &ff
	
						; set screen position to last digit of score and draw a 0 tile
	lda #lo(screenAddr + 22 * chrColumn + 24 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 22 * chrColumn + 24 * chrRow)
	sta drawMapTileAddr + 1

	lda #0
	jsr drawExtraTile

	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawHighScoreName			; draw the high score name in red

	jmp drawHighScore			; draw high score points and exit



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawHighScoreName				draw the high score name in red on the lower playfield panel
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawHighScoreName

	jsr drawString				; set screen position and color to red
	equb pixels1
	equw screenAddr + 2 + 16 + 5 * chrColumn + 25 * chrRow
	equb &ff
	
	lda #lo(highScoreTable + 3)		; draw the high score name text
	sta drawTextAddr
	lda #Hi(highScoreTable + 3)
	sta drawTextAddr + 1
	
	jmp drawText



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawPlayfieldLowerLives			; draw lives value
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawPlayfieldLowerLives

	jsr drawString				; set screen position to right of ladybug, set color to yellow
	equb pixels3
	equw screenAddr + 2 + 16 + 1 * chrColumn + 24 * chrRow
	equb &ff

	lda lives				; draw 2 digit lives value and return
	jmp drawBcd



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawPlayfieldMiddle				draw middle playfield tiles
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

; tileMap		= 23 * 23 tiles = &211 bytes so draw 2 pages + &11 bytes

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawPlayfieldMiddle

	lda #lo(screenAddr + 1 * chrRow)	; start at screen column 0 row 1
	sta drawMapTileAddr
	lda #hi(screenAddr + 1 * chrRow)
	sta drawMapTileAddr + 1
	
	ldx #0					; initialize index
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawPlayfieldMiddleLoop0

	lda tileMap, x				; draw 1st &100 of &211 map tiles
	jsr drawMapTile
	
	inx
	bne drawPlayfieldMiddleLoop0
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawPlayfieldMiddleLoop1			; draw 2nd &100 of &211 map tiles

	lda tileMap + &100, x
	jsr drawMapTile
	
	inx
	bne drawPlayfieldMiddleLoop1
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawPlayfieldMiddleLoop2

	lda tileMap + &200, x			; draw last &11 of &211 map tiles
	jsr drawMapTile
	
	inx
	cpx #&11
	bne drawPlayfieldMiddleLoop2
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawBonusItemCenter				draw vegetable sprite in center bug box (if active) or draw diamond sprite (if active and correct level)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusItemCenter

	lda bonusItemActive			; if bonus item not active then return
	bne drawBonusItemCenterActive

	rts

	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusItemCenterActive

	lda #centerBoxX				; else set screen position to center box
	sta drawSpriteX
	lda #centerBoxY + 2
	sta drawSpriteY

	lda bonusDiamondEnable			; if bonusDiamondEnable == true
	beq drawBonusItemVegetable

	lda level				; if level >= bonusDiamondLevel
	cmp #bonusDiamondLevel
	bcc drawBonusItemVegetable
	
	lda #centerDiamond			; then draw a diamond
	bne drawSprite10x10

	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusItemVegetable

	lda vegetableImg			; else get the vegetable img

	; continue down to drawSprite10x10 to draw the vegetable



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; draw a 10x10 image at supplied coordinates
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawSprite10x10

	sta drawSpriteImg			; store image number for sprite to be drawn

	stx drawSpriteSaveX			; save registers
	sty drawSpriteSaveY

	lda #0					; draw column from 0
	sta drawSpriteColumnVinit + 1

	lda #sprite10x10Height			; set hight for the 10x10 pixel sprite
	sta drawSpriteColumnVtest + 1
	sta drawSpriteColumnTileHeight + 1
	
	lda #opcodeINX				; drawing normal so use INX instruction
	sta drawSpriteNextLineInstruction

	lda #sprite10x10Bytes			; store number of bytes for sprite in counter
	sta drawByteCount

	bpl drawSpriteGetX			; draw the vegetable sprite



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawSprite					draw sprite on screen
; drawSpriteFlipped				draw sprite on screen vertically flipped
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			drawSpriteImg		sprite to be drawn
;			drawSpriteX		x position of sprite
;			drawSpriteY		y position of sprite
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		drawSpriteScreenAddr	calculated screen position
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; additional information: 3 will be automatically added to spriteImg when the x position requires a pixel shifted image
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawSprite

	stx drawSpriteSaveX			; save registers
	sty drawSpriteSaveY

	lda #0					; draw column from 0 to spriteTileHeight - 1
	sta drawSpriteColumnVinit + 1

	lda #spriteTileHeight			; done when = spriteTileHeight
	sta drawSpriteColumnVtest + 1

	sta drawSpriteColumnTileHeight + 1	; set tile height to spriteTileHeight

	lda #opcodeINX				; drawing normal so use INX instruction
	sta drawSpriteNextLineInstruction

	lda #spriteTileBytes			; store number of bytes in sprite in counter
	sta drawByteCount

	bne drawSpriteGetX			; contine to drawSprite code

	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawSpriteFlipped

	stx drawSpriteSaveX			; save registers
	sty drawSpriteSaveY

	lda #spriteTileHeight - 1		; draw column from spriteTileHeight - 1 to 0
	sta drawSpriteColumnVinit + 1

	lda #&ff				; done when < 0
	sta drawSpriteColumnVtest + 1

	lda #spriteTileHeight			; set tile height to spriteTileHeight
	sta drawSpriteColumnTileHeight + 1

	lda #opcodeDEX				; drawing mirrored so use DEX instruction
	sta drawSpriteNextLineInstruction

	lda #spriteTileBytes			; store number of bytes in sprite in counter
	sta drawByteCount

	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawSpriteGetX

	jsr spriteToScreen			; convert sprite XY to screen address

	lda drawSpriteImg			; get sprite data address from spriteImg address table

	tay
	lda spriteImgAddrLo, y
	sta drawSpriteRead + 1
	lda spriteImgAddrHi, y
	sta drawSpriteRead + 2

.drawSpriteColumn

	lda drawSpriteY				; set y index to lowest 3 bits of y position
	and #7
	tay
	
	lda drawSpriteScreenAddr		; setup screen address for writing
	sta drawSpriteWrite + 1
	lda drawSpriteScreenAddr + 1
	sta drawSpriteWrite + 2

.drawSpriteColumnVinit

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; data value of following instruction modified by drawSprite/drawSpriteVflip/drawBonusItemCenter
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #dummy8

.drawSpriteRead

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; sprite source address written into the following instruction
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda dummy16, x				; read byte from sprite (address previously setup)

.drawSpriteWrite	

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; screen destination address written into the following instruction
	;---------------------------------------------------------------------------------------------------------------------------------------------

	sta dummy16, y				; write byte to screen

.drawSpriteNextLine

	iny					; move to next line down on screen
	
	tya
	and #7					; check if row adjustment needed
	bne drawSpriteCount
	
	clc					; adjust screen address for next row
	lda drawSpriteWrite + 1
	adc #lo(chrRow)
	sta drawSpriteWrite + 1
	lda drawSpriteWrite + 2
	adc #hi(chrRow)
	sta drawSpriteWrite + 2
	
	ldy #0					; zero y index for start of next row

.drawSpriteCount

	dec drawByteCount			; bump sprite byte counter
	beq drawSpriteDrawn			; exit if all bytes drawn

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; the following instruction is replaced with inx/dex by drawSprite/drawSpriteVflip/drawBonusItemCenter
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawSpriteNextLineInstruction

	nop

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; the value in the following instruction is replaced by drawSprite/drawSpriteVflip/drawBonusItemCenter
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawSpriteColumnVtest

	cpx #dummy8
	bne drawSpriteRead			; if still processing column then continue reading and writing sprite data
	
	clc					; adjust sprite address for next column
	lda drawSpriteRead + 1

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; the value in the following instruction modified by drawSprite/drawSpriteVflip/drawBonusItemCenter
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawSpriteColumnTileHeight

	adc #dummy8
	sta drawSpriteRead + 1
	bcc drawSpriteColumnTileHeightNext
	inc drawSpriteRead + 2

.drawSpriteColumnTileHeightNext

	clc					; adjust screen address for next column
	lda drawSpriteScreenAddr
	adc #8
	sta drawSpriteScreenAddr
	bcc drawSpriteColumn
	inc drawSpriteScreenAddr + 1
	bpl drawSpriteColumn

.drawSpriteDrawn

	sec					; exit with drawn status

.drawSpriteExit

	ldx drawSpriteSaveX			; restore registers
	ldy drawSpriteSaveY

	rts					; return


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; addScoreMultiply				add A to score in bcd mode with multiplier
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			score value
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.addScoreMultiplyTable

	equb 1, 2, 3, 5				; x1 x2 x3 x5 multiplier

	;---------------------------------------------------------------------------------------------------------------------------------------------

.addScoreMultiply

	sta addScoreMultiplySaveA		; preserve score
	stx addScoreMultiplySaveX		; preserve X register
	
	ldx scoreMultiplier			; get scoreMultiplier index

	lda addScoreMultiplyTable, x		; get multiplier loop count from table
	tax
	
.addScoreMultiplyLoop

	lda addScoreMultiplySaveA		; add score to player score
	jsr addScore
	
	dex					; repeat as required
	bne addScoreMultiplyLoop
	
	ldx addScoreMultiplySaveX		; restore register

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; addScore					add A to score in bcd mode
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			LSB
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.addScore

.addScoreBottom

	sed					; bcd mode

	clc					; add A to score LSB
	adc score
	sta score

	lda #0					; add carry if any

.addScoreMiddle

	adc score + 1
	sta score + 1

	lda #0					; add carry if any

.addScoreTop

	adc score + 2
	sta score + 2

	bcc addScoreExit			; if no score overflow ( > 999999 ) then exit
	
	lda #&99				; else set score to 999999
	sta score
	sta score + 1
	sta score + 2

.addScoreExit

	cld					; back to binary mode

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; addScoreVegetable				shift vegetable score one digit and add to score
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry						none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.addScoreVegetable

	lda vegetableScore			; get vegetable score and shift units digit to tens and add to score
	asl a
	asl a
	asl a
	asl a
	jsr addScore
	
	lda vegetableScore			; get vegetable score and shift units digit to hundreds
	lsr a
	lsr a
	lsr a
	lsr a
	
	sed					; select bcd mode as we are jumping into the middle of addScore to do the hundreds
	clc					; no carry
	bcc addScoreMiddle			; add hundreds



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; addScoreDiamond				add diamond bonus to score
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.addScoreDiamond

	lda #diamondBonusScore			; add the diamond bonus score to the top 2 digits of score (bcd)
	sed
	clc
	bcc addScoreTop



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; addScoreSpecial				add special bonus to score
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.addScoreSpecial

	lda #specialBonusScore			; add the special bonus score to the top 2 digits of score (bcd)
	sed
	clc
	bcc addScoreTop



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; generateValidation				generate a validation code for the high score table and game settings
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.generateValidation

	ldx #0					; zero the validation code
	stx validationCode
	
.generateValidationLoop

	lda configData, x			; validationCode += configData[x] eor #magicNumber
	eor #magicNumber
	clc
	adc validationCode
	sta validationCode
	
	inx					; repeat loop until end of configData
	cpx #validationCode - configData
	bne generateValidationLoop
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawBcdMini					draw 2 digits of BCD mini chr tiles
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			value to display as hex
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			drawChrMiniAddr		points to next mini chr tile position on screen
;			A			destroyed
;			X			preserved
;			y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		stack			1 byte stack space
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawBcdMini

	pha					; save value
	
	lsr a					; draw high nybble
	lsr a
	lsr a
	lsr a
	jsr drawChrMini
	
	pla					; get value
	
	and #%00001111				; draw low nybble

	; continue on into drawChrMini for the 2nd digit



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawChrMini					draw mini chr to screen, move to next chr position
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			chr to be drawn 0-9
;			drawChrMiniAddr		current screen location for chr
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		drawChrSaveX		storage to preserve X
;			drawChrSaveY		storage to preserve Y
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawChrMini

	stx drawChrSaveX			; save registers
	sty drawChrSaveY

	asl a					; tile address = A * 16 + miniFontBin
	asl a
	asl a
	asl a
	adc #lo(miniFontBin)			; clc not needed as its already clear
	sta drawChrMiniLoop + 1
	lda #0
	adc #hi(miniFontBin)
	sta drawChrMiniLoop + 2

	ldy #miniFontBytes - 1			; number of bytes to transfer
	
.drawChrMiniLoop

	lda dummy16, y				; get byte from minifont (address previously setup)
	
.drawChrMiniWrite

	sta dummy16, y				; store it on screen
	
	dey					; repeat until all bytes transferred
	bpl drawChrMiniLoop
	
	lda #miniFontBytes			; move to next chr position
	clc
	adc drawChrMiniWrite + 1
	sta drawChrMiniWrite + 1
	bcc drawChrMiniExit
	inc drawChrMiniWrite + 2

.drawChrMiniExit

	ldy drawChrSaveY			; restore registers
	ldx drawChrSaveX
	
	rts					; return

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; alias to self modifying code address
;-----------------------------------------------------------------------------------------------------------------------------------------------------

drawChrMiniAddr = drawChrMiniWrite + 1




;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawMapTileAddrAdvance			advance the screen address by value in accumilator
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawMapTileAddrAdvance

	clc					; add value in A to mapTileAddr
	adc drawMapTileAddr
	sta drawMapTileAddr
	bcc drawMapTileAddrAdvanceExit
	inc drawMapTileAddr + 1
	
.drawMapTileAddrAdvanceExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; levelAdvance					note: !!! all values are BCD except vegetableImage and mazeMap
;-----------------------------------------------------------------------------------------------------------------------------------------------------
;						if level < &99 then level = level + &01
;						if shield != &00 then shield = shield - &01
;						if vegetableScore < &9500 then vegetableScore = vegetableScore + &0500
;						vegetableImage = vegetableImage + 1
;						if vegetableImage >= horseradish + 1 then vegetableImage = cucumber
;						mazeMap = mazeMap + 1
;						if MazeMap >= 6 then mazeMap = 0
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.levelAdvance

	sed					; switch to bcd mode

	lda level				; if level < &99
	cmp #&99
	bcs levelAdvanceShield

	adc #&01				; add &01 to level (carry already clear from previous cmp result)
	sta level
	
.levelAdvanceShield

	lda shield				; if shield != &00
	beq levelAdvanceVegetableScore

	sec					; then shield = shield - &01
	sbc #&01
	sta shield

.levelAdvanceVegetableScore	

	lda vegetableScore			; if vegetableScore < &9500
	cmp #&95
	bcs levelAdvanceVegetableImage
	adc #&05				; then add &0500 to vegetableScore
	sta vegetableScore
	
.levelAdvanceVegetableImage

	cld					; switch to binary mode

	inc vegetableImg			; add 1 to vegetable image

	lda vegetableImg			; if vegetableImg >= horseRadish + 1 (last vegetable horse radish)
	cmp #centerHorseradish + 1
	bcc levelAdvanceMazeMap

	lda #centerCucumber			; then vegetableImg = cucumber (back to first vegetable cucumber)
	sta vegetableImg

.levelAdvanceMazeMap

	inc mazeMap				; add 1 to mazeMap

	lda mazeMap				; if mazeMap >= 6
	cmp #6
	bcc levelAdvanceExit
	
	lda #0					; then mazeMap = 0
	sta mazeMap

.levelAdvanceExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; updatePauseTimers				update ladybug and enemy pause timers
;						handle erasure of object score if required
;						handle erasure of vegetable score if required
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updatePauseTimers

	lda vsyncCounter			; if vsync counter and 1 = 0 (25Hz)
	and #1
	bne updatePauseTimersExit

	lda pauseLadybug			; if pauseLadybug timer > 0 then decrement
	beq updatePauseTimersEnemy
	dec pauseLadybug

	bne updatePauseTimersEnemy		; if pauseLadybug timer == 0 then
	
	lda spritesDir + 0			; unblank ladybug sprite
	and #spriteBlanking eor &ff
	sta spritesDir + 0

	lda objectScoreImg			; if there is currently an object score being displayed
	beq updatePauseTimersVegetableScore

	lda #0					; then disable it
	sta objectScoreImg

	lda objectScoreX			; and erase it from screen
	sta drawSpriteX
	lda objectScoreY
	sec
	sbc #2
	sta drawSpriteY
	jsr eraseBlock

	;---------------------------------------------------------------------------------------------------------------------------------------------

.updatePauseTimersVegetableScore

	lda vegetableScoreActive		; if vegetable score was also active then
	beq updatePauseTimersEnemy
	
	lda #0					; deactive vegetable score display
	sta vegetableScoreActive
	
						; position to left wall of center box

	lda #lo(screenAddr + vegetableScoreX * chrColumn + vegetableScoreY * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + vegetableScoreX * chrColumn + vegetableScoreY * chrRow)
	sta drawMapTileAddr + 1
	
	lda #mapTileVerticalBar			; draw vertical wall tile
	jsr drawMapTile
	
	lda #mapTileBlank			; draw blank tile
	jsr drawMapTile
	
	lda #mapTileVerticalBar			; draw vertical wall tile
	jsr drawMapTile

	;---------------------------------------------------------------------------------------------------------------------------------------------

.updatePauseTimersEnemy

	lda pauseEnemy				; if pauseEnemy timer > 0 then decrement
	beq updatePauseTimersExit
	dec pauseEnemy
	
.updatePauseTimersExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; updateAnimationFrame				; update the sprite animation frame timer and frame number
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateAnimationFrame

	dec spritesImgFrameCounter		; animation timer -= 1
	bne updateAnimationFrameExit		; if animation timer = 0
	
	lda #spritesAnimationSpeed		; then animation timer = animation speed
	sta spritesImgFrameCounter
	
	dec spritesImgFrame			; animation frame -= 1
	bpl updateAnimationFrameExit		; if animation frame < 0

	lda #3					; then animation frame = 3
	sta spritesImgFrame

.updateAnimationFrameExit

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; instructions					display game instructions and wait for start to be pressed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.instructionsLetters

	equb mapTileS, mapTileP, mapTileE, mapTileC, mapTileI, mapTileA, mapTileL
	equb -1, -1, -1
	equb mapTileYellowE, mapTileYellowX, mapTileYellowT, mapTileYellowR, mapTileYellowA

	;---------------------------------------------------------------------------------------------------------------------------------------------

.instructions

	jsr playSoundSilence			; kill any current sounds

	jsr playfieldMiddleWithTimer		; initialize and draw empty playfield with timer, initialize all sprites as blanked and erased

	lda #palObject + palRed			; set special letters to red
	sta ulaPalette

	lda #2					; draw two random flowers at screen row 2
	jsr drawFlowers

	jsr drawString				; draw "instruction" in red
	equb pixels1
	equw screenAddr + 2 + 8 + 5 * chrColumn + 4 * chrRow
	equs "INSTRUCTIONS", &ff
	
						; position ready for the 3 cyan hearts

	lda #lo(screenAddr + 8 + 2 * chrColumn + 7 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 8 + 2 * chrColumn + 7 * chrRow)
	sta drawMapTileAddr + 1

	lda #mapTilecyanHeart			; draw cyan heart
	jsr drawMapTile

	lda #column				; advance 1 column
	jsr drawMapTileAddrAdvance

	lda #mapTilecyanHeart			; draw cyan heart
	jsr drawMapTile

	lda #column				; advance 1 column
	jsr drawMapTileAddrAdvance

	lda #mapTilecyanHeart			; draw cyan heart
	jsr drawMapTile
	
	jsr drawString				; draw "multiply score" in green
	equb pixels2
	equw screenAddr + 2 + 16 + 6 * chrColumn + 7 * chrRow
	equs "MULTIPLY SCORE", &ff

	lda #lo(screenAddr + 8 + 3 * chrColumn + 10 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 8 + 3 * chrColumn + 10 * chrRow)
	sta drawMapTileAddr + 1

	ldx #0					; draw the special and extra letters

.instructionsLettersLoop

	lda instructionsLetters, x		; get a letter from table

	bmi instructionsLettersAddrAdjust	; if its a letter the draw it and advance 1 column else just advance 1 column

	jsr drawMapTile

.instructionsLettersAddrAdjust

	lda #column				; advance 1 column
	jsr drawMapTileAddrAdvance
	
	inx					; move to next entry in table

	cpx #15					; and repeat until all letters done
	bne instructionsLettersLoop

	jsr drawString				; draw "collect for bonus" in green
	equb pixels2
	equw screenAddr + 2 + 3 * chrColumn + 11 * chrRow
	equs "COLLECT FOR BONUS", &ff

	jsr drawString				; draw "garden prizes" in green
	equb pixels2
	equw screenAddr + 2 + 5 * chrColumn + 12 * chrRow
	equs "GARDEN PRIZES", &ff

	jsr drawString				; draw "return pauses" in magenta
	equb pixels5
	equw screenAddr + 2 + 5 * chrColumn + 15 * chrRow
	equs "RETURN PAUSES", &ff

	jsr drawString				; draw "move to unpause" in yellow
	equb pixels3
	equw screenAddr + 2 + 4 * chrColumn + 16 * chrRow
	equs "MOVE TO UNPAUSE", &ff

	jsr drawString				; draw "hold esc to quit" in red
	equb pixels1
	equw screenAddr + 2 + 3 * chrColumn + 17 * chrRow
	equs "HOLD ESC TO QUIT!", &ff

	jsr drawString				; draw ">" in flashing red/magenta
	equb pixelsSpecial0
	equw screenAddr + 2 + 8 + 4 * chrColumn + 20 * chrRow
	equs chrRight, &ff
	
	jsr drawString				; draw "start game" in flashing skull color
	equb pixelsSkull
	equw screenAddr + 2 + 8 + 6 * chrColumn + 20 * chrRow
	equs "START GAME", &ff
	
	jsr drawString				; draw "<" in flashing red/magenta
	equb pixelsSpecial0
	equw screenAddr + 2 + 8 + 17 * chrColumn + 20 * chrRow
	equs chrLeft, &ff

	lda #sfxObject				; play object sound effect
	jsr playSound


	;---------------------------------------------------------------------------------------------------------------------------------------------
	; process functions and wait key release
	;---------------------------------------------------------------------------------------------------------------------------------------------

.instructionsRelease

	jsr instructionsFunctions		; update colors and scan keyboard

	bne instructionsRelease			; if key is pressed then loop back and wait for release
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; process functions and wait key press
	;---------------------------------------------------------------------------------------------------------------------------------------------

.instructionsPress

	jsr instructionsFunctions		; update colors and scan keyboard

	beq instructionsPress			; if key not pressed then loop back and wait for key press

	cmp #keyBitStart			; if start key was pressed then exit with true status (start game)
	beq instructionsReturnTrue

	cmp #keyBitEsc				; if esc key was pressed then exit with false status (return to menu)
	beq instructionsReturnFalse
	
	bne instructionsRelease			; else loop back and wait for key release

	;---------------------------------------------------------------------------------------------------------------------------------------------

.instructionsReturnFalse

	jsr playSoundSilence			; kill any current sounds

	lda #sfxSkull				; play sound effect
	jsr playSound

	clc					; return false
	rts

	;---------------------------------------------------------------------------------------------------------------------------------------------

.instructionsReturnTrue

	jsr playSoundSilence			; kill any current sounds

	sec					; return true
	rts



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; functions used in instructions
	;---------------------------------------------------------------------------------------------------------------------------------------------

.instructionsFunctions

	jsr instructionsLadybugAnimation	; do the lady bug walking animation

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; upper sync
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr waitVsyncUpper			; wait upper half, read analogue joystick (if enabled)

	jsr animateLadybug			; update lady bug direction and frame counter
	
	jsr redrawSprites			; draw lady bug and enemy sprites

	jsr moveSprites				; move lady bug sprite

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; lower sync
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr waitVsyncLower			; wait lower half, read analogue joystick (if enabled)

	jsr processSound			; process sound effects and music

	jsr redrawSprites			; draw lady bug and enemy sprites

	jsr updateAnimationFrame		; update the animtion frame number

	jsr updateSkullColor			; update the skull palette color

	jsr updateBonusColor			; update the bonus letters palette colors

	jsr drawScore				; draw score (1 digit per loop)

	jsr inputScan				; read keyboard input and joystick (if enabled)

	lda playerInput				; return with input bits

	rts



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; initialize lady bug walking animation
	;---------------------------------------------------------------------------------------------------------------------------------------------

.instructionsLadybugAnimation

	lda animateLadybugActive		; if ladybug animation not active
	bne instructionsLadybugAnimationExit
	
	lda #animateLadybugInstructions		; then start the animation
	jsr animateLadybugInitialize

.instructionsLadybugAnimationExit

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; ladybugDeathAnimation
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.ladybugDeathAnimationTable

	equb -2,  0				; floating angel x,y directions
	equb -2, -1
	equb  0, -1
	equb  2,  0
	equb  2, -1
	equb  0, -1

	;---------------------------------------------------------------------------------------------------------------------------------------------

.ladybugDeathAnimation

	lda ladybugDeathEnable			; if ladybug death animation enabled
	beq ladybugDeathAnimationReturnFalse

	lda pauseLadybug			; if ladybug pause timer >= flash time
	cmp #256 - ladybugDeathFlashTime
	bcc ladybugDeathAnimationDrawAngel

	and #4					; then flash ladybug
	bne ladybugDeathAnimationBlank
	
	lda spritesDir + 0
	and #spriteBlanking eor 255
	sta spritesDir + 0

	lda #0					; zero death animation index for 2nd part
	sta ladybugDeathAnimationIndex

	jmp ladybugDeathAnimationCheckMusic
	
.ladybugDeathAnimationBlank

	lda spritesDir + 0
	ora #spriteBlanking
	sta spritesDir + 0

.ladybugDeathAnimationCheckMusic

	lda soundTimers + 0			; if music not playing
	bne ladybugDeathAnimationReturnFalse
	
	lda #&ff				; enable level entry animation
	sta ladybugEntryEnable

	sec					; return true (level restart required)
	
	rts					; return to game

.ladybugDeathAnimationReturnFalse

	clc					; return false (level restart not required)

	rts					; return to game

.ladybugDeathAnimationDrawAngelLower

	lda spritesY + 0
	cmp #upperLowerThreshold
	bcs ladybugDeathAnimationCheckMusic

	bcc ladybugDeathAnimationDrawAngelOk

.ladybugDeathAnimationDrawAngel

	lda pauseLadybug			; if ladybug pause timer == flash time
	cmp #(256 - ladybugDeathFlashTime) - 1
	bne ladybugDeathAnimationCheckUpperLower

	ldx #spritesTotal - 1			; erase all enemies
	lda #spriteBlanking

.ladybugDeathAnimationDrawAngelLoop

	sta spritesDir, x
	dex
	bne ladybugDeathAnimationDrawAngelLoop

	lda #centerBoxX				; erase center box (remove vegetable or diamond)
	sta drawSpriteX
	lda #centerBoxY
	sta drawSpriteY
	jsr eraseBlock

.ladybugDeathAnimationCheckUpperLower

	lda screenHalf				; get upper / lower flag
	bne ladybugDeathAnimationDrawAngelLower
	
	lda spritesY + 0
	cmp #upperLowerThreshold
	bcc ladybugDeathAnimationCheckMusic
	
.ladybugDeathAnimationDrawAngelOk

	lda spritesX + 0			; copy ladybug xy position for address conversion and angel sprite drawing
	sta spriteToAddrX
	sta drawSpriteX
	lda spritesY + 0
	sta spriteToAddrY
	sta drawSpriteY

	jsr spriteToAddr			; convert xy to tile map address and screen tile address
	
	ldy #0					; redraw top 3 background tiles
	lda (tileMapAddr), y
	jsr drawMapTile
	iny
	lda (tileMapAddr), y
	jsr drawMapTile
	iny
	lda (tileMapAddr), y
	jsr drawMapTile

	lda #lo(chrRow - 3 * chrColumn)		; move to next row
	clc
	adc drawMapTileAddr
	sta drawMapTileAddr
	lda #hi(chrRow - 3 * chrColumn)
	adc drawMapTileAddr + 1
	sta drawMapTileAddr + 1
	
	ldy #23					; redraw middle 3 background tiles
	lda (tileMapAddr), y
	jsr drawMapTile
	iny
	lda (tileMapAddr), y
	jsr drawMapTile
	iny
	lda (tileMapAddr), y
	jsr drawMapTile

	lda #lo(chrRow - 3 * chrColumn)		; move to next row
	clc
	adc drawMapTileAddr
	sta drawMapTileAddr
	lda #hi(chrRow - 3 * chrColumn)
	adc drawMapTileAddr + 1
	sta drawMapTileAddr + 1
	
	ldy #46					; redraw bottom 3 background tiles
	lda (tileMapAddr), y
	jsr drawMapTile
	iny
	lda (tileMapAddr), y
	jsr drawMapTile
	iny
	lda (tileMapAddr), y
	jsr drawMapTile

	lda spriteBaseImg + 9			; setup angel sprite image and position
	sta drawSpriteImg
	lda spritesX + 0
	sta drawSpriteX
	lda spritesY + 0
	sta drawSpriteY

	lda pauseLadybug			; animate angel wings
	and #4
	bne ladybugDeathAnimationDrawAngelSprite
	
	inc drawSpriteImg

.ladybugDeathAnimationDrawAngelSprite

	jsr drawSprite				; draw angel sprite

	lda pauseLadybug			; if its time to move
	cmp #256 - ladybugDeathFlashTime - ladybugDeathWaitTime
	bcs ladybugDeathAnimationDrawAngelExit

	ldx ladybugDeathAnimationIndex		; adjust angel x postion using direction table

	lda spritesX + 0			; get signed x direction and add to sprite x
	clc
	adc ladybugDeathAnimationTable, x

	cmp #1 * 8				; clip x to stay within the playfield area
	bcs ladybugDeathAnimationLimitXhi
	lda #1 * 8

.ladybugDeathAnimationLimitXhi

	cmp #21 * 8
	bcc ladybugDeathAnimationLimitXstore

	lda #21 * 8

.ladybugDeathAnimationLimitXstore

	sta spritesX + 0			; save new sprite x position

	inx					; adjust angel y position using direction table

	lda spritesY + 0			; get signed y direction and add to sprite y
	clc
	adc ladybugDeathAnimationTable, x

	cmp #8					; clip y to stay within the playfield area
	bcs ladybugDeathAnimationLimitYstore

	lda #8

.ladybugDeathAnimationLimitYstore

	sta spritesY + 0			; save new sprite y position

	inx					; index now points to next pair of directions

	lda vsyncCounter			; after 8 vsync frames have counted
	and #7
	bne ladybugDeathAnimationDrawAngelExit
	
	cpx #12					; if index at end of table
	bne ladybugDeathAnimationFrame

	ldx #0					; then set it back to start
	
.ladybugDeathAnimationFrame

	stx ladybugDeathAnimationIndex		; save updated index for next time around

.ladybugDeathAnimationDrawAngelExit

	jmp ladybugDeathAnimationCheckMusic	; go check if death animation music is complete (end of animation)



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; keyboardScan					scan all keys (used when redefining input keys)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			C			clear if no key pressed, set if key pressed
;			A			key index if key was pressed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.keyScanAscii

	equs "0123456789-^\", chrLeft, chrRight
	equs "QWERTYUIOP@[_", chrUp, chrDown
	equs "ASDFGHJKL;:]"
	equs "ZXCVBNM,./"

	;---------------------------------------------------------------------------------------------------------------------------------------------

.keyScanCodes

	equb key0, key1, key2, key3, key4, key5, key6, key7, key8, key9, keyMinus, keyRaise, keyBackslash, keyLeft, keyRight
	equb keyQ, keyW, keyE, keyR, keyT, keyY, keyU, keyI, keyO, keyP, keyAt, keyBracketOpen, keyUnderscore, keyUp, keyDown
	equb keyA, keyS, keyD, keyF, keyG, keyH, keyJ, keyK, keyL, keySemicolon, keyColon, keyBracketClosed
	equb keyZ, keyX, keyC, keyV, keyB, keyN, keyM, keyComma, keyPeriod, keySlash

.keyScanCodesEnd

	;---------------------------------------------------------------------------------------------------------------------------------------------

.keyboardScan

	stx keyboardScanSaveX			; save register

	lda #&7f				; set port A bit 7 as input ( from keyboard output )
	sta via1PortDdrA
	
	lda #sbKeyboard + sbLow			; keyboard -enable low
	sta via1PortB
	
	ldx #(keyScanCodesEnd - keyScanCodes) - 1; start at end of table
	
.keyboardScanLoop

	lda keyScanCodes, x			; get key scan code from table

	sta via1PortA				; select key in keyboard matrix

	lda via1PortA				; read key status

	bmi keyboardScanPressed			; if pressed then exit with scancode

	dex					; else try next code until all tested
	bpl keyboardScanLoop

	lda #sbKeyboard + sbHigh		; keyboard -enable high
	sta via1PortB
	
	ldx keyboardScanSaveX			; restore register

	clc					; no key pressed so return with false
	rts

.keyboardScanPressed

	lda #sbKeyboard + sbHigh		; keyboard -enable high
	sta via1PortB
	
	txa					; A = scan index
	
	ldx keyboardScanSaveX			; restore register

	sec					; return true
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; initLevelSettings				setup letters, skulls and enemy settings etc, everything for the current level
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.enemySpeedTable

						; enemy speed option 0
	equb 0.00 * 256				; 1.00 level 1-6
	equb 0.05 * 256				; 1.05 level 7-11
	equb 0.10 * 256				; 1.10 level 12-17
	equb 0.15 * 256				; 1.15 level 18-99
	
						; enemy speed option 1
	equb 0.00 * 256				; 1.00 level 1-6
	equb 0.07 * 256				; 1.07 level 7-11
	equb 0.14 * 256				; 1.14 level 12-17
	equb 0.21 * 256				; 1.21 level 18-99
	
						; enemy speed option 2
	equb 0.05 * 256				; 1.05 level 1-6
	equb 0.13 * 256				; 1.13 level 7-11
	equb 0.21 * 256				; 1.21 level 12-17
	equb 0.29 * 256				; 1.29 level 18-99

						; enemy speed option 3
	equb 0.10 * 256				; 1.10 level 1-6
	equb 0.20 * 256				; 1.20 level 7-11
	equb 0.30 * 256				; 1.30 level 12-17
	equb 0.40 * 256				; 1.40 level 18-99
	
						; enemy speed option 4
	equb 0.15 * 256				; 1.15 level 1-6
	equb 0.30 * 256				; 1.30 level 7-11
	equb 0.45 * 256				; 1.45 level 12-17
	equb 0.60 * 256				; 1.60 level 18-99
	
						; enemy speed option 5
	equb 0.20 * 256				; 1.20 level 1-6
	equb 0.40 * 256				; 1.40 level 7-11
	equb 0.60 * 256				; 1.60 level 12-17
	equb 0.80 * 256				; 1.80 level 18-99
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.initLevelSettings

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; set number of skulls
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda level				; get current level (bcd)

	ldx #&02				; 2 skulls
	cmp #&02				; level 1
	bcc initLevelSettingsSkulls
	
	inx					; 3 skulls
	cmp #&05				; level 2-4
	bcc initLevelSettingsSkulls
	
	inx					; 4 skulls
	cmp #&10				; level 5-9
	bcc initLevelSettingsSkulls

	inx					; 5 skulls
	cmp #&18				; level 10-17
	bcc initLevelSettingsSkulls
	
	inx					; 6 skulls level 18-99

.initLevelSettingsSkulls

	stx levelSkulls				; set the number of skulls

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; set enemy speed
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda optionEnemySpeed			; x = user selected speed option * 4
	asl a
	asl a
	tax

	lda level				; get current level (bcd)

	cmp #&07				; if level >= 7 then add 1 to x
	bcc initLevelSettingsSpeed
	inx

	cmp #&12				; if level >= 12 the add 1 to x
	bcc initLevelSettingsSpeed
	inx

	cmp #&18				; if level >= 18 then add 1 to x
	bcc initLevelSettingsSpeed
	inx

.initLevelSettingsSpeed

	lda enemySpeedTable, x			; set the enemy speed from the table, x
	sta enemySpeed

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; set enemy timer speed (number of vsync frames per tick)
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda level				; get current level (bcd)

	ldx #&08				; use 8 frames if level < 2 (level 1, slow enemy timer)
	cmp #&02
	bcc initLevelSettingsTimer
	
	ldx #&05				; use 5 frames if level < 5 (levels 2 to 4, medium enemy timer)
	cmp #&05
	bcc initLevelSettingsTimer
	
	ldx #&03				; else use 3 frames (level 5 to 99, fast enemy timer)

.initLevelSettingsTimer

	stx enemyTimerSpeed			; set enemy timer speed
	stx enemyTimerSpeedCounter
	
	lda #0					; zero enemy timer position
	sta enemyTimer

	sta enemyReleaseEnable			; disable enemy release

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; set object mode to start with red
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #objectModeCyan			; set object mode to cyan and object mode timer to 1
	sta objectMode				; to force the object mode to instantly change to red and the color to be updated
	lda #1
	sta objectModeTimer
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; clear the x2 x3 x5 flags, clear the score multiplier and the special/extra/diamond active flags
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda bonusBits				; clear the x2 x3 x5 bits bits 0,1,2 of bonusBits
	ora #bonusBitsMultiplier
	sta bonusBits

	lda #0					; start with no multiplier
	sta scoreMultiplier

	sta bonusSpecialActive			; special bonus deactivated

	sta bonusExtraActive			; extra bonus deactivated

	sta bonusDiamondActive			; diamond bonus deactivated

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; level not ended yet
	;---------------------------------------------------------------------------------------------------------------------------------------------

	sta levelEnd				; disable level end flag

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; game not paused
	;---------------------------------------------------------------------------------------------------------------------------------------------

	sta pauseGame				; unpause game

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; no turnstile to be drawn
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #&ff				; no pending turnstile to be drawn
	sta drawTurnstileAddr + 1

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; choose random letters
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jmp chooseLetters			; choose 3 random letters and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; mainMenu
;
;						this is a huge mess and needs rewriting much smaller
;
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.optionsMin

	equb  1,  0,  0,  0,  0			; minimum value for lives, speed, attack, volume, sound
	
.optionsMax

	equb  10, 6, 10,  4,  2			; maximum value + 1 for lives, speed, attack, volume, sound

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenu

	jsr mainMenuDraw			; draw the main menu screen

	lda #0					; make sure the "START GAME" text is flashing (skull color) by setting shield to 0
	sta shield

	sta pauseLadybug			; unpause ladybug so that it will animate

	sta mainMenuCursor			; select first entry
	sta mainMenuCursorOld			; make old cursor same for now

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; update cursor and validation code
	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuUpdateCursor

	jsr mainMenuDrawCursor			; draw updated cursor

	jsr generateValidation			; update the validation code

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; process functions and wait key release
	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuWaitRelease

	jsr mainMenuFunctions			; update animation, sprites, sound and scan keyboard

	bne mainMenuWaitRelease			; if key is pressed then loop back and wait for release
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; process functions and wait key press
	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuWaitPress

	jsr mainMenuFunctions			; update animation, sprites, sound and scan keyboard

	beq mainMenuWaitPress			; if key not pressed then loop back and wait for key press

	jsr mainMenuProcess			; process the key pressed option

	bcc mainMenuUpdateCursor		; loop back until start game selected

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; return to start a new game
	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuExit

	jmp generateValidation			; update the validation code and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; mainMenuLadybugAnimation			if ladybug isnt animated then trigger an animation
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuLadybugAnimation

	lda animateLadybugActive		; if ladybug animation not active
	bne mainMenuLadybugAnimationExit
	
	lda #animateLadybugMainMenu		; then start the animation
	jsr animateLadybugInitialize

.mainMenuLadybugAnimationExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; mainMenuDrawCursor				erase old cursor and draw new
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuDrawCursor

	ldy mainMenuCursorOld			; get old cursor position
	
	clc					; erase old cursor
	lda screenRowLo, y
	adc #lo(2 + 3 * chrColumn + 11 * chrRow)
	sta drawChrAddr
	lda screenRowHi, y
	adc #hi(2 + 3 * chrColumn + 11 * chrRow)
	sta drawChrAddr + 1
	lda #' '
	jsr drawChr

	clc
	lda screenRowLo, y
	adc #lo(2 + 19 * chrColumn + 11 * chrRow)
	sta drawChrAddr
	lda screenRowHi, y
	adc #hi(2 + 19 * chrColumn + 11 * chrRow)
	sta drawChrAddr + 1
	lda #' '
	jsr drawChr

	lda #pixelsSpecial0			; set color to flashing red/magenta
	sta drawChrColor

	ldy mainMenuCursor			; get current cursor position

	clc					; draw current cursor
	lda screenRowLo, y
	adc #lo(2 + 3 * chrColumn + 11 * chrRow)
	sta drawChrAddr
	lda screenRowHi, y
	adc #hi(2 + 3 * chrColumn + 11 * chrRow)
	sta drawChrAddr + 1
	lda #chrRight
	jsr drawChr

	clc
	lda screenRowLo, y
	adc #lo(2 + 19 * chrColumn + 11 * chrRow)
	sta drawChrAddr
	lda screenRowHi, y
	adc #hi(2 + 19 * chrColumn + 11 * chrRow)
	sta drawChrAddr + 1
	lda #chrLeft
	jsr drawChr

	lda mainMenuCursor			; update old cursor position with current cursor position
	sta mainMenuCursorOld

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; mainMenuFunctions				wait for syncs and do sprite redraw, color update, keyboard scan etc
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuFunctions

	jsr mainMenuLadybugAnimation		; check and initialise ladybug animation if needed

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; wait for upper sync and process
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr waitVsyncUpper			; wait upper half, read analogue joystick (if enabled)

	jsr animateLadybug			; update ladybug direction and frame counter
	
	jsr redrawSprites			; draw sprites

	jsr moveSprites				; move sprites

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; wait for lower sync and process
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr waitVsyncLower			; wait lower half, read analogue joystick (if enabled)

	jsr processSound			; process sound effects and music

	jsr redrawSprites			; draw sprites

	jsr updateAnimationFrame		; update the animtion frame number

	jsr updateSkullColor			; update the skull palette color

	jsr updateBonusColor			; update the bonus letters palette colors

	jsr drawScore				; draw score (1 digit per loop)

	jsr random				; guarentee that letters and layout are random when the player starts a game

	jsr inputScan				; read keyboard input and joystick (if enabled)

	lda mainMenuCursor			; if the cursor is on the timer volume selection
	cmp #6
	bne mainMenuFunctionsExit
	
	lda vsyncCounter			; if vsyncCounter & 7 == 0
	and #7
	bne mainMenuFunctionsExit

	jsr playSoundTimer			; play timer sound at correct volume

.mainMenuFunctionsExit

	lda playerInput				; return with input bits

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; mainMenuProcess				process keyboard input and run required function
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcess

	lda playerInput				; if up pressed then run up function
	cmp #keyBitUp
	beq mainMenuProcessUp
	
	cmp #keyBitDown				; if down pressed then run down function
	beq mainMenuProcessDown
	
	cmp #keyBitStart			; if start pressed then run start function
	beq mainMenuProcessStart

.mainMenuProcessFalse

	clc					; else return false
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; up button functions
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessUp

	dec mainMenuCursor			; mainMenuCursor -= 1

	lda mainMenuCursor			; get mainMenuCursor

	cmp #1					; if mainMenuCursor = 1 then we need to decrement again
	beq mainMenuProcessUp			; (skip over blank line between "start game" and first adjustable menu item

	lda mainMenuCursor			; if mainMenuCursor < 0
	bpl mainMenuProcessUpExit

	lda #8					; then mainMenuCursor = 8 (handle wrap around)
	
.mainMenuProcessUpExit

	sta mainMenuCursor			; update cursor value

	lda #sfxMunch				; play sound effect
	jsr playSound

	clc					; return false
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; down button functions
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessDown

	inc mainMenuCursor			; mainMenuCursor += 1

	lda mainMenuCursor			; get mainMenuCursor

	cmp #1					; if mainMenuCursor = 1 then increment again
	beq mainMenuProcessDown			; (skip over the blank line between "start game" and first adjustable menu item

	cmp #9					; if mainMenuCursor >= 9
	bne mainMenuProcessDownExit
	
	lda #0					; then mainMenuCursor == 0 (handle wrap around)

.mainMenuProcessDownExit

	sta mainMenuCursor			; update cursor
	
	lda #sfxMunch				; play sound effect
	jsr playSound

	clc					; return false
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; start button functions
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessStart

	ldx mainMenuCursor			; if mainMenuCursor = 0 then return true (start game)
	beq mainMenuProcessReturnTrue
	
	cpx #2					; if mainMenuCursor = 2 then display high score table
	beq mainMenuHighScores

	cpx #8					; if mainMenuCursor == 8 then redefine the keyboard
	beq mainMenuProcessKeyboard

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; add 1 to the current game setting pointed to by cursor index (x)
	; if its higher then allowed then reset it to the minimum value
	;---------------------------------------------------------------------------------------------------------------------------------------------

						; x index is offset by 3
						; first entrys in menu are "start game", "blank line", "high scores"
						; so subtract 3 from table addresses to compensate for the offset

	inc gameSettings - 3, x			; gameSettings[x - 3] += 1

	lda gameSettings - 3, x			; if gameSettings[x - 3] >= optionsMax[x - 3]
	cmp optionsMax - 3, x
	bcc mainMenuProcessStartExit
	
	lda optionsMin - 3, x			; then gameSettings[x - 3] = optionsMin[x - 3]
	sta gameSettings - 3, x

	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessStartExit

	jsr mainMenuDrawSettings		; draw the updated settings

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	lda mainMenuCursor			; get cursor
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessStartLadybugLives

	cmp #3					; if cursor == 3 (ladybugLives) then
	bne mainMenuProcessStartEnemySpeed

	lda optionLives				; lives = optionLives
	sta lives

	jsr drawPlayfieldLowerLives		; update the lives value in the lower playfield

	clc					; return false
	rts

	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessStartEnemySpeed

	cmp #4					; if cursor == 4 (enemySpeed) then
	bne mainMenuProcessStartEnemyAttack
	
	jsr mainMenuDrawEnemies			; place 4 random enemies on screen
	
	clc					; return false
	rts

	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessStartEnemyAttack

	cmp #5					; if cursor == 5 (enemyAttack) then
	bne mainMenuProcessReturnFalse
	
	jsr mainMenuDrawEnemies			; place 4 random enemies on screen
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessReturnFalse

	clc					; return false
	rts
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessReturnTrue

	sec					; return true
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; .mainMenuHighScores				; display high score table
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuHighScores

	jsr playSoundSilence			; silence current effect

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	jsr drawScoreTable			; draw the high scores page and wait for start or esc to be pressed
	
	jsr mainMenuDraw			; redraw main menu

	clc					; return
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; .mainMenuProcessKeyboard			; redefine the keys
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessKeyboard

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	jsr drawString				; draw text
	equb pixelsExtra0
	equw screenAddr + 2 + 4 * chrColumn + 20 * chrRow
	equs "PRESS          ", &ff

	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawString				; draw text
	equb pixelsSpecial1
	equw screenAddr + 2 + 10 * chrColumn + 20 * chrRow
	equs "UP", &ff
	
	jsr mainMenuProcessKeyboardKey		; get key index
	
	tay					; get key scan code
	lda keyScanCodes, y

	sta optionKeys + 3			; store up key scan code

	jsr drawString				; position for printing
	equb pixels7
	equw screenAddr + 2 + 15 * chrColumn + 20 * chrRow
	equb &ff

	lda keyScanAscii, y			; get ascii chr of key

	sta optionKeysAscii + 3			; store in list

	jsr drawChr				; draw it on screen

	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	jsr drawString				; draw text
	equb pixelsSpecial1
	equw screenAddr + 2 + 10 * chrColumn + 20 * chrRow
	equs "DOWN", &ff
	
.mainMenuProcessKeyboardDown

	jsr mainMenuProcessKeyboardKey		; get key index
	
	tay					; get key scan code
	lda keyScanCodes, y

	cmp optionKeys + 3			; if key is same as up then try again
	beq mainMenuProcessKeyboardDown

	sta optionKeys + 2			; store down key scan code

	jsr drawString				; position for printing
	equb pixels7
	equw screenAddr + 2 + 16 * chrColumn + 20 * chrRow
	equb &ff

	lda keyScanAscii, y			; get ascii chr of key

	sta optionKeysAscii + 2			; store in list

	jsr drawChr				; draw it on screen

	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	jsr drawString				; draw text
	equb pixelsSpecial1
	equw screenAddr + 2 + 10 * chrColumn + 20 * chrRow
	equs "LEFT", &ff
	
.mainMenuProcessKeyboardLeft

	jsr mainMenuProcessKeyboardKey		; get key index
	
	tay					; get key scan code
	lda keyScanCodes, y

	cmp optionKeys + 3			; if key is same as up then try again
	beq mainMenuProcessKeyboardLeft
	cmp optionKeys + 2			; if key is same as down then try again
	beq mainMenuProcessKeyboardLeft

	sta optionKeys + 1			; store left key scan code

	jsr drawString				; position for printing
	equb pixels7
	equw screenAddr + 2 + 17 * chrColumn + 20 * chrRow
	equb &ff

	lda keyScanAscii, y			; get ascii chr of key

	sta optionKeysAscii + 1			; store in list

	jsr drawChr				; draw it on screen

	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	jsr drawString				; draw text
	equb pixelsSpecial1
	equw screenAddr + 2 + 10 * chrColumn + 20 * chrRow
	equs "RIGHT", &ff
	
.mainMenuProcessKeyboardRight

	jsr mainMenuProcessKeyboardKey		; get key index
	
	tay					; get key scan code
	lda keyScanCodes, y

	cmp optionKeys + 3			; if key is same as up then try again
	beq mainMenuProcessKeyboardRight
	cmp optionKeys + 2			; if key is same as down then try again
	beq mainMenuProcessKeyboardRight
	cmp optionKeys + 1			; if key is same as left then try again
	beq mainMenuProcessKeyboardRight

	sta optionKeys + 0			; store right key scan code

	jsr drawString				; position for printing
	equb pixels7
	equw screenAddr + 2 + 18 * chrColumn + 20 * chrRow
	equb &ff

	lda keyScanAscii, y			; get ascii chr of key

	sta optionKeysAscii + 0			; store in list

	jsr drawChr				; draw it on screen

	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	jsr drawString				; restore original text
	equb pixels3
	equw screenAddr + 2 + 4 * chrColumn + 20 * chrRow
	equs "CONTROLS   ", &ff

	clc					; return
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; handle colors and animation, wait for key release
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessKeyboardKey

	jsr mainMenuFunctions			; update colors animation movement etc

	jsr keyboardScan			; wait for key release
	bcs mainMenuProcessKeyboardKey
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; handle colors and animation, wait for key press and return with key index in A
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessKeyboardKeyWaitPress

	jsr mainMenuFunctions			; update colors animation movement etc

	jsr keyboardScan			; wait for key press
	bcc mainMenuProcessKeyboardKeyWaitPress

	rts					; return with key press index in A



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; draw the ladybug logo
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuDrawLogoData

	equb -1,  0,  1, -1, -1,  2, -1, -1,  3,  4,  5, -1, -1, -1, -1
	equb  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
	equb 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35

	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuDrawLogo

	lda #lo(mainMenuDrawLogoData)		; set start address of logo tile data
	sta mainMenuDrawLogoX + 1
	lda #hi(mainMenuDrawLogoData)
	sta mainMenuDrawLogoX + 2

	ldy #0					; initialise line counter
	
.mainMenuDrawLogoY

	clc					; starting screen address
	lda #lo(4 * chrColumn)
	adc screenRowLo + 2, y
	sta drawMapTileAddr
	lda #hi(4 * chrColumn)
	adc screenRowHi + 2, y
	sta drawMapTileAddr + 1

	ldx #0
	
.mainMenuDrawLogoX

	lda dummy16				; get byte from logo tile data (address previously setup)

	bpl mainMenuDrawLogoTile		; if tile = -1
	
	lda #extraTileBlank			; then draw a blank tile
	bne mainMenuDrawLogoTileNow

.mainMenuDrawLogoTile

	clc					; else draw tile + extraTileLogo
	adc #extraTileLogo

.mainMenuDrawLogoTileNow

	jsr drawExtraTile
	
	inc mainMenuDrawLogoX + 1		; inc logo tile address
	bne mainMenuDrawLogoNext
	inc mainMenuDrawLogoX + 2

.mainMenuDrawLogoNext

	inx					; repeat until current row is complete
	cpx #15
	bne mainMenuDrawLogoX
	
	iny					; repeat until all rows are completed
	cpy #3
	bne mainMenuDrawLogoY

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; draw the menu text and user defined control keys
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuDrawText

	jsr drawString
	equb pixels1
	equw screenAddr + 2 + 8 + 4 * chrColumn + 7 * chrRow
	equs "UNIVERSAL", chrCopyright, "1981", &ff

	jsr drawString
	equb pixels5
	equw screenAddr + 2 + 5 * chrColumn + 9 * chrRow
	equs "PROGRAMMED BY", &ff

	jsr drawString
	equb pixels3
	equw screenAddr + 2 + 8 + 5 * chrColumn + 10 * chrRow
	equs "LOVEBUG 2021", &ff

	jsr drawString
	equb pixelsSkull
	equw screenAddr + 2 + 8 + 6 * chrColumn + 12 * chrRow
	equs "START GAME", &ff

	jsr drawString
	equb pixelsExtra1
	equw screenAddr + 2 + 4 * chrColumn + 14 * chrRow
	equs "HIGH SCORES", &ff

	jsr drawString
	equb pixels2
	equw screenAddr + 2 + 4 * chrColumn + 15 * chrRow
	equs "LADY BUGS", &ff
	
	jsr drawString
	equb pixels3
	equw screenAddr + 2 + 4 * chrColumn + 16 * chrRow
	equs "ENEMY SPEED", &ff
	
	jsr drawString
	equb pixels2
	equw screenAddr + 2 + 4 * chrColumn + 17 * chrRow
	equs "ENEMY ATTACK", &ff
	
	jsr drawString
	equb pixels3
	equw screenAddr + 2 + 4 * chrColumn + 18 * chrRow
	equs "TIMER VOLUME", &ff

	jsr drawString
	equb pixels2
	equw screenAddr + 2 + 4 * chrColumn + 19 * chrRow
	equs "SOUND", &ff
	
	jsr drawString
	equb pixels3
	equw screenAddr + 2 + 4 * chrColumn + 20 * chrRow
	equs "CONTROLS", &ff
	
	jsr drawString
	equb pixels7
	equw screenAddr + 2 + 15 * chrColumn + 20 * chrRow
	equb &ff

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; display the current user defined control keys
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #3					; start with up key
	
.mainMenuDrawTextKeys

	lda optionKeysAscii, x			; get ascii version of key and display it
	jsr drawChr
	
	dex					; continue until all 4 printed
	bpl mainMenuDrawTextKeys

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; draw the option settings
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuDrawSettings

	jsr drawString				; draw ladybug lives
	equb pixels7
	equw screenAddr + 2 + 18 * chrColumn + 15 * chrRow
	equb &ff
	
	lda optionLives
	ora #'0'

	jsr drawChr
	
	jsr drawString				; draw enemy speed
	equb pixels7
	equw screenAddr + 2 + 18 * chrColumn + 16 * chrRow
	equb &ff
	
	lda optionEnemySpeed
	ora #'0'
	jsr drawChr
	
	jsr drawString				; draw enemy attack
	equb pixels7
	equw screenAddr + 2 + 18 * chrColumn + 17 * chrRow
	equb &ff
	
	lda optionEnemyAttack
	ora #'0'
	jsr drawChr
	
	jsr drawString				; draw timer volume
	equb pixels7
	equw screenAddr + 2 + 18 * chrColumn + 18 * chrRow
	equb &ff
	
	lda optionTimerVolume
	ora #'0'
	jsr drawChr

						; draw sound on/off

	lda optionSound				; if sound enabled
	beq mainMenuDrawSettingsMute

	jsr drawString				; draw "ON"
	equb pixels7
	equw screenAddr + 2 + 16 * chrColumn + 19 * chrRow
	equs " ON", &ff

	rts					; return
	
.mainMenuDrawSettingsMute

	jsr drawString				; else draw "OFF"
	equb pixels7
	equw screenAddr + 2 + 16 * chrColumn + 19 * chrRow
	equs "OFF", &ff

	jmp playSoundSilence			; mute the sound and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; .mainMenuDrawEnemies				draw 4 random enemies on screen
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuEnemiesX

	equb 27, 149, 157, 19			; enemy x positions

.mainMenuEnemiesY

	equb 69, 69, 50, 50			; enemy y positions

	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuDrawEnemies

	jsr random				; pick a random direction and enemy for the draw loop

	ldx #3					; 4 enemies to draw
	
.mainMenuDrawEnemiesLoop

	lda mainMenuEnemiesX, x			; set X and Y position
	sta spritesX + 1, x
	lda mainMenuEnemiesY, x
	sta spritesY + 1, x
	
	lda randomSeed + 1			; set sprite image
	and #7
	tay
	lda spriteBaseImg + 1, y
	sta spritesImg + 1, x
	
	lda randomSeed				; set sprite direction (not moving)
	and #3
	ora #moveStop
	sta spritesDir + 1, x

	inc randomSeed				; increment direction for next sprite

	inc randomSeed + 1			; increment sprite image for next sprite

	dex					; repeat until all enemies placed
	bpl mainMenuDrawEnemiesLoop
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawFlowers					draw two flowers at supplied row
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			vertical row for flowers
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawFlowers

	pha					; save row position for 2nd flower
	tay

	lda screenRowLo, y			; calculate address
	clc
	adc #lo(1 * chrColumn)
	sta drawMapTileAddr + 0
	lda screenRowHi, y
	adc #hi(1 * chrColumn)
	sta drawMapTileAddr + 1

	jsr drawRandomFlower			; draw a random flower

	pla					; restore row position
	tay

	lda screenRowLo, y			; calculate address
	clc
	adc #lo(20 * chrColumn)
	sta drawMapTileAddr + 0
	lda screenRowHi, y
	adc #hi(20 * chrColumn)
	sta drawMapTileAddr + 1

	; continue down to drawRandomFlower



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; .drawRandomFlower				draw a single random flower from the current drawMapTileAddr
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawRandomFlower

	jsr random				; pick random number 0,4,8,12
	and #%00001100

	tay					; use as index for flower tile

	lda drawBonusGraphicsTile, y		; draw top left
	jsr drawExtraTile
	iny

	lda drawBonusGraphicsTile,y		; draw top right
	jsr drawExtraTile
	iny

	clc					; move to next row
	lda drawMapTileAddr
	adc #lo(chrRow - 2 * chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrRow - 2 * chrColumn)
	sta drawMapTileAddr + 1

	lda drawBonusGraphicsTile, y		; draw bottom left
	jsr drawExtraTile
	iny
	
	lda drawBonusGraphicsTile, y		; draw bottom right
	jsr drawExtraTile

	clc					; move to next row
	lda drawMapTileAddr
	adc #lo(chrRow - 2 * chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrRow - 2 * chrColumn)
	sta drawMapTileAddr + 1

	lda #extraTileLeavesL			; draw leaves and return
	jsr drawExtraTile

	lda #extraTileLeavesR
	jmp drawExtraTile



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; mainMenuDraw					draw everything on main menu screen
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuDraw

	jsr playfieldMiddleWithTimer		; initialize and draw empty playfield with timer, initialize all sprites as blanked and erased

	jsr mainMenuDrawLogo			; draw the ladybug logo

	lda #2					; draw two random flowers at screen row 2
	jsr drawFlowers

	jsr mainMenuDrawText			; draw the menu text

	jsr mainMenuDrawSettings		; draw the current settings

	jmp mainMenuDrawEnemies			; place 4 enemies on screen and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawScoreTable				; draw the high score table page and wait for start to be pressed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawScoreTable

	jsr playfieldMiddleWithTimer		; initialize and draw empty playfield with timer, initialize all sprites as blanked and erased

	jsr drawString				; draw 2 red hearts
	equb pixels1
	equw screenAddr + 2 + 8 + 1 * chrColumn + 3 * chrRow
	equs chrHeart,chrHeart,chrHeart,&ff
	
	jsr drawString				; draw text in skull color
	equb pixelsSkull
	equw screenAddr + 2 + 8 + 5 * chrColumn + 3 * chrRow
	equs "BEST PLAYERS",&ff
	
	jsr drawString				; draw 2 red hearts
	equb pixels1
	equw screenAddr + 2 + 8 + 18 * chrColumn + 3 * chrRow
	equs chrHeart,chrHeart,chrHeart,&ff

	jsr drawString				; position cursor
	equb pixels0
	equw screenAddr + 2 + 8 + 2 * chrColumn + 5 * chrRow
	equb &ff

	ldx #8					; 8 high scores and names to display

	lda #lo(highScoreTable)			; start at 1st entry
	sta highScorePtr
	lda #hi(highScoreTable)
	sta highScorePtr + 1
	
.drawScoreTableLoop

	jsr drawScoreTableBlanking		; draw score with leading zero blanking

	lda #' '				; 1 space
	jsr drawChr

.drawScoreTableName

	lda #pixelsSpecial0			; draw name
	sta drawChrColor
	lda highScorePtr
	clc
	adc #3
	sta drawTextAddr
	lda highScorePtr + 1
	sta drawTextAddr + 1
	jsr drawText

	lda drawScoreTableName + 1		; alternate name color
	eor #pixelsSpecial0 eor pixelsSpecial1
	sta drawScoreTableName + 1

	lda drawChrAddr				; advance two rows (46 characters) minus 18 characters (7 for score, 1 space, 10 for name)
	clc
	adc #lo((46 - 18) * chrColumn)
	sta drawChrAddr
	lda drawChrAddr + 1
	adc #hi((46 - 18) * chrColumn)
	sta drawChrAddr + 1

	lda highScorePtr			; advance to next score (3 bytes score, 10 bytes name, 1 byte terminator)
	clc
	adc #14
	sta highScorePtr
	
	dex					; repeat until all 8 processed
	bne drawScoreTableLoop

	jsr drawString				; draw ">MENU<"
	equb pixelsSpecial0
	equw screenAddr + 2 + 8 + 7 * chrColumn + 21 * chrRow
	equs chrRight, &ff
	
	jsr drawString
	equb pixelsSkull
	equw screenAddr + 2 + 8 + 9 * chrColumn + 21 * chrRow
	equs "MENU", &ff
	
	jsr drawString
	equb pixelsSpecial0
	equw screenAddr + 2 + 8 + 14 * chrColumn + 21 * chrRow
	equs chrLeft, &ff

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; do functions and wait for key release
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawScoreTableRelease

	jsr drawScoreTableFunctions		; update colors and scan keyboard

	bne drawScoreTableRelease		; if key is pressed then loop back and wait for release
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; do functions and wait for start/esc pressed
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawScoreTablePress

	jsr drawScoreTableFunctions		; update colors and scan keyboard

	cmp #keyBitStart			; if start or esc pressed then exit else loop back and wait for key press
	beq drawScoreTableExit

	cmp #keyBitEsc
	bne drawScoreTablePress

	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawScoreTableExit

	jsr playSoundSilence			; kill any sounds playing
	
	lda #sfxTurnstile			; play sound effect and return
	jmp playSound



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawScoreTableBlanking			draw the score with leading zero blanking
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawScoreTableBlanking

	lda #0					; enable leading zero blanking
	sta drawScoreTableZero

	ldy #2					; index to score bytes

.drawScoreTableBlankingColor

	lda #pixelsExtra0			; set score color
	sta drawChrColor

.drawScoreTableBlankingPrint

	lda (highScorePtr), y			; draw pair of digits
	pha
	lsr a
	lsr a
	lsr a
	lsr a
	jsr drawScoreTableBlankingDigit
	pla
	and #%00001111
	jsr drawScoreTableBlankingDigit
	
	dey					; repeat until all pairs displayed
	bpl drawScoreTableBlankingPrint
	
	lda #'0'				; draw final 0 digit
	jsr drawChr

	lda drawScoreTableBlankingColor + 1	; alternate score color
	eor #pixelsExtra0 eor pixelsExtra1
	sta drawScoreTableBlankingColor + 1

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawScoreTableBlankingDigit			draw a single digit with leading zero blanking
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawScoreTableBlankingDigit

	bne drawScoreTableBlankingDigitNotZero	; if digit is a zero and if leading zero blanking is enabled
	bit drawScoreTableZero
	bmi drawScoreTableBlankingDigitNotZero
	
	lda #' '				; then draw a space and return
	jmp drawChr

.drawScoreTableBlankingDigitNotZero

	ora #'0'				; else draw the digit
	jsr drawChr

	lda #&ff				; digit was drawn so disable leading zero blanking
	sta drawScoreTableZero

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawScoreTableFunctions			wait for syncs, update colors, scan keyboard etc
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawScoreTableFunctions

	jsr waitVsyncUpper			; wait upper half, read analogue joystick (if enabled)

	jsr waitVsyncLower			; wait lower half, read analogue joystick (if enabled)

	jsr processSound			; process sound effects and music

	jsr updateSkullColor			; update the skull palette color

	jsr updateBonusColor			; update the bonus letters palette colors

	jsr inputScan				; read keyboard input and joystick (if enabled)

	lda playerInput				; return with input bits

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; checkPauseGame
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.checkPauseGame

	lda ladybugEntryEnable			; if ladybug entry animation active then return false
	bne checkPauseGameReturnFalse

	lda ladybugDeathEnable			; if ladybug death animation active then return false
	bne checkPauseGameReturnFalse

	lda pauseGame				; if game not currently paused
	bne checkPauseGameTrue

	lda playerInput				; if start not pressed
	cmp #keyBitStart
	beq checkPauseGameActivate

.checkPauseGameReturnFalse

	clc					; then return false
	rts
	
.checkPauseGameActivate

	lda #&ff				; else pause game
	sta pauseGame
	
	jsr drawString				; draw paused message
	equb pixelsSpecial0
	equw screenAddr + 2 + 16 + 5 * chrColumn + 25 * chrRow
	equs "  PAUSED  ", &ff
	
.checkPauseGameReturnTrue

	sec					; return true
	rts

.checkPauseGameTrue

	lda playerInput				; if start is not pressed
	and #keyBitStart
	bne checkPauseGameReturnTrue

	lda playerInput				; if up down left right pressed
	and #keyBitUp + keyBitDown + keyBitLeft + keyBitRight
	beq checkPauseGameReturnTrue
	
	lda #0					; deactivate game pause
	sta pauseGame
	
	jsr drawHighScoreName			; redraw the high score name to replace the paused text

	clc					; return false
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; nameReg					draw the name registration screen and get high score name
;
;						this is a huge mess and needs rewriting much smaller
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.nameRegText

	equs "ABCDEFGHIJKLMNOPQRSTUVWXYZ", chrHeart, "!. "

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameReg

	jsr playfieldMiddleWithTimer		; initialize and draw empty playfield with timer, initialize all sprites as blanked and erased

	lda #nameRegTimer			; set enemy timer speed
	sta enemyTimerSpeed

	lda #0					; unpause enemies so that the timer will tick
	sta pauseEnemy

	sta pauseLadybug			; unpause ladybug so that it will animate

	sta shield				; clear shields so that skull color will sequence

	lda #0					; enable enemy release flag usage and warning sound
	sta enemiesActive
	sta enemyReleaseEnable			; disable enemy release flag (used later in timeout test)

	lda #4					; draw two random flowers at screen row 4
	jsr drawFlowers

	jsr drawString				; draw text and entry characters
	equb pixels1
	equw screenAddr + 2 + 16 + 4 * chrRow + 3 * chrColumn
	equs "CONGRATULATIONS!", &ff

	jsr drawString
	equb pixels5
	equw screenAddr + 2 + 6 * chrRow + 5 * chrColumn
	equs "REGISTER YOUR", &ff

	jsr drawString
	equb pixels3
	equw screenAddr + 2 + 8 * chrRow + 4 * chrColumn
	equs "HIGH SCORE NAME", &ff

	jsr drawString
	equb pixels4
	equw screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn
	equs "----------", &ff

	jsr drawString
	equb pixels2
	equw screenAddr + 2 + 14 * chrRow + 4 * chrColumn
	equs "A B C D E F G H", &ff
	
	jsr drawString
	equb pixels2
	equw screenAddr + 2 + 16 * chrRow + 4 * chrColumn
	equs "I J K L M N O P", &ff

	jsr drawString
	equb pixels2
	equw screenAddr + 2 + 18 * chrRow + 4 * chrColumn
	equs "Q R S T U V W X", &ff

	jsr drawString
	equb pixels2
	equw screenAddr + 2 + 20 * chrRow + 4 * chrColumn
	equs "Y Z ", chrHeart, " ! .   ", &ff

	lda #pixels1				; draw delete in red
	sta drawChrColor
	lda #chrLeft
	jsr drawChr
	
	lda #' '				; space
	jsr drawChr
	
	lda #pixelsSkull			; draw enter in skull color
	sta drawChrColor
	lda #chrDown
	jsr drawChr
	
	ldy #0					; store score into high score table
	lda score + 0
	sta (highScorePtr), y
	iny
	lda score + 1
	sta (highScorePtr), y
	iny
	lda score + 2
	sta (highScorePtr), y

	lda #3					; move ptr to high score name
	clc
	adc highScorePtr
	sta highScorePtr

	ldy #9					; clear high score name
	lda #' '
	
.nameRegClear

	sta (highScorePtr), y
	dey
	bpl nameRegClear

	lda #0					; position cursor over first letter
	sta nameRegCursor
	sta nameRegCursorOld

	sta nameRegCursorText			; position text cursor on 1st character

	lda #animateLadybugNameReg		; initialize the highScore animation
	jsr animateLadybugInitialize

	jsr nameRegCursorUpdate			; display the initial cursor position

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; do functions and wait for key release
	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegWaitRelease

	jsr nameRegFunctions			; update timer, colors and scan keyboard

	bcs nameRegExit				; if enemy timer timed out then exit

	bne nameRegWaitRelease			; if key is pressed then loop back and wait for release
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; do functions, wait for key press and process key press
	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegWaitPress

	jsr nameRegFunctions			; update timer, colors and scan keyboard

	bcs nameRegExit				; if enemy timer timed out then exit

	beq nameRegWaitPress			; if key not pressed then loop back and wait for key press

	jsr nameRegProcess			; process the key pressed functions

	bcc nameRegWaitRelease			; if end not selected then loop back and wait for key release

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegExit

	jmp playSoundSilence			; kill any sounds playing and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; nameRegFunctions				wait for sync, update sound and graphics, scan keyboard etc etc
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.nameRegFunctions

	jsr waitVsyncUpper			; wait upper half, read analogue joystick (if enabled)

	jsr animateLadybug			; update ladybug direction and frame counter
	
	jsr redrawSprites			; draw ladybug

	jsr moveSprites				; move ladybug

	jsr waitVsyncLower			; wait lower half, read analogue joystick (if enabled)

	jsr processSound			; process sound effects and music

	jsr redrawSprites			; draw ladybug

	jsr updateAnimationFrame		; update the animtion frame number

	jsr updateEnemyTimer			; update the enemy timer and draw tile when needed

	jsr updateSkullColor			; update the skull palette color

	jsr updateBonusColor			; update the bonus letters palette colors

	jsr inputScan				; read keyboard input and joystick (if enabled)

	lda enemyReleaseEnable			; if timer has passed the top left
	beq nameRegTimerActive
	lda enemyTimer				; and if timer is at position 1
	cmp #1
	beq nameRegTimerTimeout			; then exit with timer timeout status
	
.nameRegTimerActive

	lda playerInput				; read keyboard/joystick status and exit with timer active status
	clc
	rts

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegTimerTimeout

	lda playerInput				; read keyboard/joystick status and exit with timer timeout status
	sec
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; nameRegProcess and support functions		process key presses (handle insert delete characters in name, cursor update etc etc)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessAdd32

	clc					; add 32 to cursor position
	lda nameRegCursor
	adc #32
	sta nameRegCursor
	bpl nameRegProcessCursor		; branch always to processCursor

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessSub32

	sec					; sub 32 from cursor position
	lda nameRegCursor
	sbc #32
	sta nameRegCursor
	bpl nameRegProcessCursor		; branch always to processCursor

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcess

	lsr playerInput				; if start pressed
	bcc nameRegProcessLeft

	lda nameRegCursor			; if cursor = 31 (enter name)
	cmp #31
	bne nameRegProcessDelete

	sec					; exit with carry set (enter name)
	rts

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessLeft

	lsr playerInput				; if left pressed
	bcc nameRegProcessDown
	
	lda #sfxMunch				; play sound effect
	jsr playSound

	sec					; move back 1 position
	lda nameRegCursor
	sbc #1
	sta nameRegCursor

	bpl nameRegProcessCursor		; handle wrap around
	bmi nameRegProcessAdd32

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessDown

	lsr playerInput				; if down pressed
	bcc nameRegProcessUp
	
	lda #sfxMunch				; play sound effect
	jsr playSound

	clc					; move forward 8 positions
	lda nameRegCursor
	adc #8
	sta nameRegCursor

	cmp #32					; handle wrap around
	bcs nameRegProcessSub32
	bcc nameRegProcessCursor

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessUp

	lsr playerInput				; if up pressed
	bcc nameRegProcessRight
	
	lda #sfxMunch				; play sound effect
	jsr playSound

	sec					; move back 8 positions
	lda nameRegCursor
	sbc #8
	sta nameRegCursor

	bpl nameRegProcessCursor		; handle wrap around
	bmi nameRegProcessAdd32

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessRight

	lsr playerInput				; if right pressed
	bcc nameRegProcessCursor
	
	lda #sfxMunch				; play sound effect
	jsr playSound

	clc					; move forward 1 position
	lda nameRegCursor
	adc #1
	sta nameRegCursor

	cmp #32					; handle wrap around
	bcc nameRegProcessCursor
	bcs nameRegProcessSub32

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessCursor

	jsr nameRegCursorUpdate			; update new cursor position

	clc
	rts


	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegCursorUpdate

	jsr nameRegCursorAddr
	jsr nameRegCursorErase
	jsr nameRegCursorAddr
	jmp nameRegCursorDraw

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessDelete

	cmp #30					; if cursor = 30 (delete)
	bne nameRegProcessChr
	
	ldy nameRegCursorText			; if cursor != 0
	beq nameRegProcessCursor

	dey					; move back 1 position

	sty nameRegCursorText			; update cursor

	lda #' '				; replace chr in string with space
	sta (highScorePtr), y

	clc					; print blue -
	lda screenColumnLo, y
	adc #lo(screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn)
	sta drawChrAddr
	lda screenColumnHi, y
	adc #Hi(screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn)
	sta drawChrAddr + 1
	lda #pixels4
	sta drawChrColor
	lda #'-'
	jsr drawChr

	lda #sfxObject				; play sound
	jsr playSound

	jmp nameRegProcessCursor
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessChr

	ldy nameRegCursorText			; if cursor != 10
	cpy #10
	beq nameRegProcessCursor
	
	ldx nameRegCursor			; get selected character
	lda nameRegText, x

	sta (highScorePtr), y			; store character in string

	clc					; print chr
	lda screenColumnLo, y
	adc #lo(screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn)
	sta drawChrAddr
	lda screenColumnHi, y
	adc #Hi(screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn)
	sta drawChrAddr + 1
	lda #pixels7
	sta drawChrColor
	lda (highScorePtr), y
	jsr drawChr

	inc nameRegCursorText			; move cursor forward 1

	lda #sfxTurnstile			; play sound
	jsr playSound

	jmp nameRegProcessCursor		; exit

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegCursorErase

	lda #extraTileBlank			; erase the cursor box
	jsr drawExtraTile
	lda #extraTileBlank
	jsr drawExtraTile
	lda #extraTileBlank
	jsr drawExtraTile

	clc
	lda drawMapTileAddr
	adc #lo(chrRow - 3 * chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrRow - 3 * chrColumn)
	sta drawMapTileAddr + 1
	
	lda #extraTileBlank
	jsr drawExtraTile
	
	clc
	lda drawMapTileAddr
	adc #lo(chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrColumn)
	sta drawMapTileAddr + 1
	
	lda #extraTileBlank
	jsr drawExtraTile
	
	clc
	lda drawMapTileAddr
	adc #lo(chrRow - 3 * chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrRow - 3 * chrColumn)
	sta drawMapTileAddr + 1
	
	lda #extraTileBlank
	jsr drawExtraTile
	lda #extraTileBlank
	jsr drawExtraTile
	lda #extraTileBlank
	jmp drawExtraTile

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegCursorDraw

	lda #registrationTL		; draw the box
	jsr drawExtraTile
	lda #registrationTH
	jsr drawExtraTile
	lda #registrationTR
	jsr drawExtraTile

	clc
	lda drawMapTileAddr
	adc #lo(chrRow - 3 * chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrRow - 3 * chrColumn)
	sta drawMapTileAddr + 1
	
	lda #registrationVL
	jsr drawExtraTile
	
	clc
	lda drawMapTileAddr
	adc #lo(chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrColumn)
	sta drawMapTileAddr + 1
	
	lda #registrationVR
	jsr drawExtraTile
	
	clc
	lda drawMapTileAddr
	adc #lo(chrRow - 3 * chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrRow - 3 * chrColumn)
	sta drawMapTileAddr + 1
	
	lda #registrationBL
	jsr drawExtraTile
	lda #registrationBH
	jsr drawExtraTile
	lda #registrationBR
	jmp drawExtraTile

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegCursorAddr

	lda nameRegCursorOld			; convert cursor value to screen position
	and #&38
	lsr a
	lsr a
	clc
	adc #12
	tay
	lda nameRegCursorOld
	and #&07
	asl a
	clc
	adc #3
	tax
	clc
	lda screenRowLo, y
	adc screenColumnLo, x
	sta drawMapTileAddr
	lda screenRowHi, y
	adc screenColumnHi, x
	sta drawMapTileAddr + 1

	lda nameRegCursor			; copy new cursor to old
	sta nameRegCursorOld

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; checkBonus					check if diamond bonus, special bonus or extra bonus is required
;						call drawBonusScreen , return with carry set to indicate end of level
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.checkBonus

	lda ladybugEntryEnable			; if ladybug entry animation is enabled then exit
	bne checkBonusExit



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; handle the diamond bonus stuff
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda bonusDiamondActive
	bne checkBonusDiamondActive



.checkBonusSpecial

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; handle the special bonus stuff
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda bonusSpecialActive			; if special bonus not active
	bne checkBonusSpecialActive
	
	lda bonusBits + 1			; check if all special letters are active
	and #bonusBitsSpecial
	bne checkBonusExtra

	lda #letterBonusTime			; pause ladybug and enemies
	sta pauseLadybug
	sta pauseEnemy

	lda soundTimers + 0			; if sound effect playing on channel 0 (music) then exit
	bne checkBonusExit

	lda soundTimers + 3			; if sound effects playing on channel 3 (object) then exit
	bne checkBonusExit

	lda #&ff				; flag special bonus as active
	sta bonusSpecialActive

	lda #sfxMusicLetters			; play bonus letters music
	jsr playSound

	jmp checkBonusExit			; return to game



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; handle the extra bonus stuff
	;---------------------------------------------------------------------------------------------------------------------------------------------

.checkBonusExtra

	lda bonusExtraActive			; if extra bonus not active
	bne checkBonusExtraActive

	lda bonusBits + 0			; check if all extra letters are active
	and #bonusBitsExtra
	bne checkBonusExit
	
	lda #letterBonusTime			; pause ladybug and enemies
	sta pauseLadybug
	sta pauseEnemy

	lda soundTimers + 0			; if sound effect playing on channel 0 (music) then exit
	bne checkBonusExit

	lda soundTimers + 3			; if sound effects playing on channel 3 (object) then exit
	bne checkBonusExit

	lda #&ff				; flag special bonus as active
	sta bonusExtraActive

	lda #sfxMusicLetters			; play bonus letters music
	jsr playSound



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; return to game (no bonus)
	;---------------------------------------------------------------------------------------------------------------------------------------------

.checkBonusExit

	clc					; return to game (no bonus)
	rts



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; handle the active diamond bonus
	;---------------------------------------------------------------------------------------------------------------------------------------------

.checkBonusDiamondActive

	lda soundTimers + 0			; if sound effect not playing on channel 0 (music)
	bne checkBonusExit

	lda soundTimers + 3			; and if sound effects not playing on channel 3 (object)
	bne checkBonusExit

	jsr playSoundSilence			; silence all effects and music

	jsr drawBonusScreen			; draw the diamond bonus screen

	jsr levelAdvance			; advance game to next level

	jsr addScoreDiamond			; add the diamond bonus score (bcd)

	lda #sfxTwinkle				; play the twinkle sound
	jsr playSound

	sec					; end the current level and return
	rts

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; handle the active special bonus
	;---------------------------------------------------------------------------------------------------------------------------------------------

.checkBonusSpecialActive

	lda pauseLadybug			; if pause is over (ladybug and enemy unpaused)
	ora pauseEnemy
	bne checkBonusExit

	jsr drawBonusScreen			; draw the special bonus screen

	lda shield				; if theres currently shield active
	beq checkBonusSpecialLevel
	
	sed					; then add &01 to shield (bcd) as the call to levelAdvance will reduce shield by &01
	clc					; so the player does not lose a shield for gaining extra shields
	lda #&01
	adc shield
	sta shield
	cld

.checkBonusSpecialLevel

	jsr levelAdvance			; advance game to next level

	jsr addScoreSpecial			; add the special bonus score (bcd)

	sed					; add the shield bonus to shield (bcd)
	lda #specialBonusShield
	adc shield
	sta shield
	cld

	lda #bonusBitsSpecial			; clear the special letters
	ora bonusBits + 1
	sta bonusBits + 1

	lda #sfxTwinkle				; play the twinkle sound
	jsr playSound

	sec					; end the current level and return
	rts

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; handle the active extra bonus
	;---------------------------------------------------------------------------------------------------------------------------------------------

.checkBonusExtraActive

	lda pauseLadybug			; if pause is over (ladybug and enemy unpaused)
	ora pauseEnemy
	bne checkBonusExit

	jsr drawBonusScreen			; draw the extra bonus screen

	lda shield				; if theres currently shield active
	beq checkBonusExtraLevel
	
	sed					; then add &01 to shield (bcd) as the call to levelAdvance will reduce shield by &01
	clc					; so the player does not lose a shield for gaining extra lives
	lda #&01
	adc shield
	sta shield
	cld

.checkBonusExtraLevel

	jsr levelAdvance			; advance game to next level

	sed					; add bonus lives (bcd)
	clc
	lda #extraBonusLives
	adc lives
	sta lives
	cld

	lda #bonusBitsExtra			; clear the extra letters
	ora bonusBits + 0
	sta bonusBits + 0

	lda #sfxTwinkle				; play the twinkle sound
	jsr playSound

	sec					; end the current level and return
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; changeTimerTile				change timer tile in tileMap (eor #&01) and redraw tile on screen
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

; timer value 0-87

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.changeTimerTile

	lda enemyTimer				; get current timer tick * 2
	asl a

	tay					; get tileMap x and y position
	
	ldx timerXYTable, y
	lda timerXYTable + 1, y
	tay
	
	clc					; convert x and y to tileMap Address
	txa
	adc tileMapRowsLo, y
	sta changeTimerTileFlip + 1
	sta changeTimerTileFlip + 4
	lda #0
	adc tileMapRowsHi, y
	sta changeTimerTileFlip + 2
	sta changeTimerTileFlip + 5
	
	lda #&01				; flip bit 1 of tile in tileMap timer position

.changeTimerTileFlip

	eor dummy16				; (address previously setup)
	sta dummy16

	sta changeTimerTileDraw + 1		; store tile number for drawing

	clc
	lda screenRowLo, y			; convert x and y to screen address
	adc screenColumnLo, x
	sta drawMapTileAddr
	lda screenRowHi, y
	adc screenColumnHi, x
	sta drawMapTileAddr + 1
	
.changeTimerTileDraw

	lda #dummy8				; tile number (replaced by tile number from previous code)

	cpx #0					; if tile is column 0
	bne changeTimerTileDraw6

	cpy #0					; and tile row is not 0 or 22
	beq changeTimerTileDraw6

	cpy #22
	beq changeTimerTileDraw6

.changeTimerTileDraw4

	jmp drawMapTile4Pixel			; then draw 4 pixel wide tile to not erase objects in column 1 and return

.changeTimerTileDraw6

	jmp drawMapTile				; else draw 6 pixel wide tile and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; spriteToScreen				convert x y coordinates to screen address
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			drawSpriteX		sprite x coordinate
;			drawSpriteY		sprite y coordinate
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;			carry			set if sprite is onscreen
;			drawSpriteScreenAddr	screen address to the nearest row
;			drawSpriteX		adjusted for offset
;			drawSpriteY		adjusted for offset
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.spriteToScreen

	stx spriteToScreenSaveX			; save registers
	sty spriteToScreenSaveY

	lda #0					; zero top bits of offset
	sta drawSpriteScreenAddr + 1

	lda drawSpriteX				; get X position

	sec					; x offset correction to place sprite center at center of tileMap location
	sbc #4 - ((16 - ((spriteTileWidth * 4) / 3)) / 2)

	sta drawSpriteX

	asl a					; offset = X position * 2 (x position is scaled down to screen pixels 3/4 or 0.75)
	rol drawSpriteScreenAddr + 1

	clc					; offset = offset + X position (offset = X position * 3)
	adc drawSpriteX
	sta drawSpriteScreenAddr
	bcc spriteToScreenCheckOdd
	inc drawSpriteScreenAddr + 1

.spriteToScreenCheckOdd

	and #&04				; if the final screen pixel offset is an odd number
	beq spriteToScreenTruncateX

	clc					; then we need to draw the alternate sprite thats shifted 1 pixel to the right (spriteID + 3)
	lda drawSpriteImg
	adc #3
	sta drawSpriteImg

.spriteToScreenTruncateX

	lda drawSpriteScreenAddr		; truncate offset to nearest multiple of 8 bytes (column)
	and #&f8
	sta drawSpriteScreenAddr

	lda drawSpriteY				; get Y coodinate

	sec					; y offset correction to place sprite center at center of tileMap location
	sbc #4 - ((16 - spriteTileHeight) / 2)
	sta drawSpriteY

	lsr a					; divide by 8 to get row index
	lsr a
	lsr a
	
	tay					; get screen row address from table and add to previously calculated X offset
	clc
	lda drawSpriteScreenAddr
	adc screenRowLo, y
	sta drawSpriteScreenAddr

	lda drawSpriteScreenAddr + 1
	adc screenRowHi, y
	sta drawSpriteScreenAddr + 1		; drawSpriteScreenAddr = screen address for the nearest row
	
.spriteToScreenValid

	sec					; flag as valid

.spriteToScreenExit

	ldy spriteToScreenSaveY			; restore registers
	ldx spriteToScreenSaveX

	rts					; and return
	
.spriteToScreenNotValid

	clc					; flag as not valid

	bcc spriteToScreenExit			; and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; updateSkullColor				update the skull palette color
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateSkullColorTable

	equb palSkull + palRed
	equb palSkull + palMagenta
	equb palSkull + palGreen
	equb palSkull + palCyan
	equb palSkull + palYellow
	equb palSkull + palWhite
	equb palSkull + palWhite
	equb palSkull + palWhite
	equb palSkull + palWhite
	equb palSkull + palYellow
	equb palSkull + palCyan
	equb palSkull + palGreen
	equb palSkull + palMagenta
	equb palSkull + palRed
	equb palSkull + palBlue
	equb palSkull + palBlue

	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateSkullColor

	ldx #0					; if shield > 0 then use index 0 (red) for skull color
	lda shield
	bne updateSkullColorFromTable

	lda vsyncCounter			; else use index = (vsyncCounter / 4) & 15 for flashing color sequence
	lsr a
	lsr a
	and #15
	tax

.updateSkullColorFromTable

	lda updateSkullColorTable,x		; set skull palette to color from table using index
	sta ulaPalette

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; enemyRelease					if release is enabled then
;						release an enemy from the center box
;						increase active enemy count
;						disable enemy release (enabled by timer hitting top left when there is a pending release)
;
;						enemy release is delayed by the staggered release frame so allow a window of 5 timer blocks
;						to ensure that the enemy does get released
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.enemyRelease

	lda enemyReleaseEnable			; if enemy release is enabled
	beq enemyReleaseExit

	lda enemyTimer				; if enemyTimer < 6 (make sure delayed frame enemy release isnt skiped)
	cmp #6
	bcs enemyReleaseExit

	lda enemiesActive			; if enemiesActive < spritesTotal - 1
	cmp #spritesTotal - 1
	bcs enemyReleaseExit

	ldx #spritesTotal - 1			; start with last enemy in list

	;---------------------------------------------------------------------------------------------------------------------------------------------

.enemyReleaseLoop

	lda spritesDir, x			; if pending enemy not found
	and #spriteBlanking + moveStop
	cmp #moveStop
	beq enemyReleaseFound

	dex					; then try next enemy
	bne enemyReleaseLoop			; until all checked

	; the above branch is a branch always as an enemy will be found before x = 0

	;---------------------------------------------------------------------------------------------------------------------------------------------

.enemyReleaseMax

	lda #&ff				; maximum enemies released so activate center bonus item (vegetable/diamond)
	sta bonusItemActive
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.enemyReleaseExit

	rts					; return

	;---------------------------------------------------------------------------------------------------------------------------------------------

.enemyReleaseFound
	
	txa					; calculate frame for delayed enemy release (0, 4, 8 or 12)
	and #3
	asl a
	asl a
	sta enemyReleaseFrame
	
	lda vsyncCounter			; if (vsyncCounter & 15) != enemyReleaseFrame then exit (delay the release)
	and #15
	cmp enemyReleaseFrame
	bne enemyReleaseExit

	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #moveUp				; pending enemy found so release it by setting it moving upwards out of the box
	sta spritesDir, x

	inc enemiesActive			; increase enemies active count

	lda #0					; disable enemy release until timer re-enables it
	sta enemyReleaseEnable

	lda enemiesActive			; if maximum enemies not active yet
	cmp #spritesTotal - 1
	beq enemyReleaseMax
	
	; continue down to enemySpawn



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; enemySpawn					if the box is empty then place enemy waiting to be released
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.enemySpawn

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; first check that there are currently no enemies in the box
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #spritesTotal - 1			; check that there are currently no enemies in the box

.enemySpawnCheckEmpty

	lda spritesDir, x			; if enemy is active but stopped
	and #spriteBlanking + moveStop
	cmp #moveStop
	beq enemySpawnExit			; then its waiting in the box so exit this function

	dex					; else check next
	bne enemySpawnCheckEmpty		; until all checked

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; next find an inactive enemy and use it
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #spritesTotal - 1			; find an inactive sprite

.enemySpawnFindInactive

	lda spritesDir, x			; if enemy is blanked
	and #spriteBlanking
	bne enemySpawnCurrent			; then go spawn an enemy
	
	dex					; else try next
	bne enemySpawnFindInactive		; (this is a branch always, enemy will be found before x = 0)
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; an inactive enemy was found so we spawn it into the center box with the correct image for current level
	;---------------------------------------------------------------------------------------------------------------------------------------------

.enemySpawnCurrent

	lda level				; if level >= 9
	cmp #&09
	bcc enemySpawnLevelImage

	lda #&ff				; mark current sprite as invalid image number
	sta spritesImg, x

	stx enemySpawnSaveX			; save current sprite index

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; level is 9 or higher so pick a random enemy type that isnt already active to make sure that all 4 enemys on screen will be different
	;---------------------------------------------------------------------------------------------------------------------------------------------

.enemySpawnRandom

	jsr random				; choose random enemy type 1 to 8
	and #7
	tay
	iny
	lda spriteBaseImg, y
	
	ldx #spritesTotal - 1			; check all enemy sprites make sure that random enemy isnt already in list
	
.enemySpawnCheckImage

	cmp spritesImg, x
	beq enemySpawnRandom

	dex
	bne enemySpawnCheckImage

	ldx enemySpawnSaveX			; restore the current sprite index

	bne enemySpawnImage			; and store sprite

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; level is 1 to 8 so pick the enemy type to match the level number
	;---------------------------------------------------------------------------------------------------------------------------------------------

.enemySpawnLevelImage

	tay					; pick enemy type from level number 1 to 8
	lda spriteBaseImg, y			; set enemy image

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup the chosen enemy type and exit
	;---------------------------------------------------------------------------------------------------------------------------------------------

.enemySpawnImage

	sta spritesImg, x			; set enemy image to chosen enemy type
	
	lda #centerBoxX				; set enemy location to center box
	sta spritesX, x
	lda #centerBoxY
	sta spritesY, x

	lda #moveUp + moveStop			; set enemy direction up + stopped
	sta spritesDir, x

	lda #0					; deactivate center bonus item vegetable/diamond
	sta bonusItemActive

.enemySpawnExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; ladybugSpawn
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.ladybugSpawn

	lda #0					; disable ladybug death animation
	sta ladybugDeathEnable

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; if entry animation disabled then spawn ladybug at regular start position
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda spriteBaseImg + 0			; set ladybug image
	sta spritesImg + 0

	lda ladybugEntryEnable			; if ladybug entry animation disabled
	bne ladybugSpawnEntry

	lda #moveUp + moveStop			; set ladybug direction up + stopped
	sta spritesDir + 0

	lda #ladybugStartX			; set ladybug starting position
	sta spritesX + 0
	lda #ladybugStartY
	sta spritesY + 0

	rts					; return

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; if entry animation enabled then initialize ladybug entry animation
	;---------------------------------------------------------------------------------------------------------------------------------------------

.ladybugSpawnEntry

	lda #animateLadybugEntry		; initialize the walk on entry animation and return
	jmp animateLadybugInitialize



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; redrawSprites					process sprite list for upper or lower area, erase and redraw each sprite
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

						; lines 0-91 upper, lines 92-183 lower

						; if sprite is 1/2 across then switch to other side
upperLowerThreshold	= 92 - (spriteTileHeight / 2)

spritesPerFrame		= 3			; maximum number of sprites in each half of the screen that can be safely erased and drawn

	;---------------------------------------------------------------------------------------------------------------------------------------------

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup maximum number of sprites and choose upper or lower half
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSprites

	lda #spritesTotal			; get sprite list length
	sta redrawSpritesCount			; store it in counter
	
	lda #spritesPerFrame			; maximum number of sprites to process within 1 frame before using frame skip
	sta redrawSpritesMax
	
	lda screenHalf				; check for upper/lower half
	bmi redrawSpritesUpper
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; set threshold and index for lower half
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesLower

	lda #opcodeBCC				; BCC instruction for lower threshold test
	sta redrawSpritesEraseThreshold
	sta redrawSpritesDrawThreshold

	lda #redrawSpritesIndexLower		; use lower index
	sta redrawSpritesErase + 1
	sta redrawSpritesExit + 1

	bne redrawSpritesErase
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; set threshold and index for upper half
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesUpper
	
	lda #opcodeBCS				; BCS instruction for upper threshold test
	sta redrawSpritesEraseThreshold
	sta redrawSpritesDrawThreshold

	lda #redrawSpritesIndexUpper		; use upper index
	sta redrawSpritesErase + 1
	sta redrawSpritesExit + 1

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; erase a sprite
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesErase

	; --------------------------------------------------------------------------------------------------------------------------------------------
	; the following address is modified to point to either the upper or lower sprite index
	; --------------------------------------------------------------------------------------------------------------------------------------------

	ldx dummy8				; get sprite index so we can continue (address previously setup)

.redrawSpritesEraseLoop

	lda spritesEraseY, x
	cmp #upperLowerThreshold		; following branch is modified to test either y < upperLowerThreshold or y >= upperLowerThreshold

.redrawSpritesEraseThreshold

	; --------------------------------------------------------------------------------------------------------------------------------------------
	; the following branch instruction is modified into BCC for lower threshold test or BCS for upper threshold test
	; --------------------------------------------------------------------------------------------------------------------------------------------

	bne redrawSpritesDraw			; bne instruction replaced by bcc or bcs from above code

	lda spritesErased, x			; and if sprite needs to be erased
	bne redrawSpritesDraw
	
.redrawSpritesEraseSprite

	jsr eraseSprite				; erase sprite and redraw tile

	lda #&ff				; mark it as erased
	sta spritesErased, x

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw a sprite
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesDraw

	lda spritesDir, x			; if sprite is blanked then skip drawing it and go on to the next sprite
	and #spriteBlanking
	bne redrawSpritesNext

	lda spritesY, x				; get sprite y
	cmp #upperLowerThreshold		; following branch is modified to test either y < upperLowerThreshold or y >= upperLowerThreshold

.redrawSpritesDrawThreshold

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; the following branch instruction is modified into BCC for lower threshold test or BCS for upper threshold test
	;---------------------------------------------------------------------------------------------------------------------------------------------

	beq redrawSpritesNext			; beq instruction replaced by bcc or bcs from code above
	
	lda spritesErased, x			; if theres still a pending erase caused by the sprite crossing upper/lower boundary
	beq redrawSpritesEraseSprite		; then we must ignore the sprite y and do an emergency erase
						; otherwise part of the sprite will be left on screen just below the boundary crossing

	lda spritesX, x				; store sprite x for drawing and erasing
	sta drawSpriteX
	sta spritesEraseX, x
	
	lda spritesY, x				; store sprite y for drawing and erasing
	sta drawSpriteY
	sta spritesEraseY, x

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; calculate animation img offset
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda spritesImgFrame			; get current animation frame number, add to sprite img value and draw
	cmp #3					; unless its animation frame 3, use frame 1 instead. gives the sequence 0 1 2 1
	bne redrawSpritesAnimation
	lda #1

.redrawSpritesAnimation

	clc					; add animation to spriteImg
	adc spritesImg, x
	sta drawSpriteImg			; store sprite image number

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; calculate direction img offset
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesDirection

	lda spritesDir, x			; get sprite direction
	
	sta spritesEraseDir, x			; save direction for later erasure

	and #&03				; use just the direction bits only

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; direction = up so draw sprite normal
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesDirectionUp

	cmp #moveUp				; if direction = up
	beq redrawSpritesNormal			; then go draw the sprite

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; direction = left so offset img by 3 and draw sprite normal
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesDirectionLeft

	cmp #moveLeft				; if direction = left
	bne redrawSpritesDirectionRight
	
	clc					; then add 3 to sprite img to point to left facing sprites
	lda drawSpriteImg
	adc #3
	sta drawSpriteImg
	bne redrawSpritesNormal

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; direction = right so offset img by 9 and draw sprite normal
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesDirectionRight

	cmp #moveRight				; if direction = right
	bne redrawSpritesDirectionDown

	clc					; then add 9 to sprite img to point to right facing sprites
	lda drawSpriteImg
	adc #9
	sta drawSpriteImg
	bne redrawSpritesNormal

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; direction = down so draw sprite flipped
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesDirectionDown

	jsr drawSpriteFlipped			; if direction = down then draw sprite vertically flipped

	jmp redrawSpritesGetEraseInfo

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; direction = up/left/right so draw srpite normal (not flipped)
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesNormal

	jsr drawSprite				; draw the sprite

.redrawSpritesGetEraseInfo

	lda spritesX, x				; copy sprite info into erase list
	sta spritesEraseX, x
	lda spritesY, x
	sta spritesEraseY, x

	lda #0					; and enable erasure
	sta spritesErased, x

	dec redrawSpritesMax			; reduce the drawn sprites counter by 1

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; move on to next sprite
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesNext

	dex					; point index to next sprite

	bpl redrawSpritesCheckDrawn		; if index < 0
	ldx #spritesTotal - 1			; then index = spritesTotal - 1 (wrap around the sprite buffer)

.redrawSpritesCheckDrawn

	lda redrawSpritesMax			; if max sprites drawn then exit (remaining sprites will continue to be processed next frame)
	beq redrawSpritesExit

.redrawSpritesCheckDone	

	dec redrawSpritesCount			; continue until all sprites processed
	beq redrawSpritesExit
	jmp redrawSpritesEraseLoop
	
.redrawSpritesExit

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; following instruction address replaced with lower/upper index address
	;---------------------------------------------------------------------------------------------------------------------------------------------

	stx dummy8				; save current sprite index for continuation next frame
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawLevelIntro
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawLevelIntro

	lda #levelIntroTime			; set display time
	sta pauseCounter

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; initialize screen, sprites and palette
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr playfieldMiddleWithTimer		; initialize and draw empty playfield with timer, initialize all sprites as blanked and erased

	lda #palObject + palCyan		; set letters and hearts to cyan
	sta ulaPalette

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw the level text and number
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawString				; draw level text
	equb pixels2
	equw screenAddr + 2 + 8 + 7 * chrColumn + 4 * chrRow
	equs "LEVEL ", &ff
	
	lda #pixels5				; draw level number
	sta drawChrColor
	
	lda level
	jsr drawBcd
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw the vegetable image and score value
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #72					; draw vegetable sprite
	sta drawSpriteX
	lda #5 * 8 + 2
	sta drawSpriteY
	jsr drawBonusItemVegetable

	jsr drawString				; draw vegetable score
	equb pixels7
	equw screenAddr + 2 + 8 + 10 * chrColumn + 6 * chrRow
	equb &ff

	lda vegetableScore
	jsr drawBcd
	lda #&00
	jsr drawBcd

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw vegetable name centered
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #pixels3				; draw vegetable name
	sta drawChrColor

	lda vegetableImg
	asl a
	asl a
	tax

	lda vegetableAddr, x
	sta drawChrAddr
	lda vegetableAddr + 1, x
	sta drawChrAddr + 1

	lda vegetableAddr + 2, x
	sta drawTextAddr
	lda vegetableAddr + 3, x
	sta drawTextAddr + 1

	jsr drawText

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; calculate the address to center the skulls
	;---------------------------------------------------------------------------------------------------------------------------------------------

						; start with center

	lda #lo(screenAddr + 12 * chrColumn + 11 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 12 * chrColumn + 11 * chrRow)
	sta drawMapTileAddr + 1
	
	ldx levelSkulls				; get number of skulls

.drawLevelIntroSkullAddr

	sec					; for each skull subtract chrColumn from the address
	lda drawMapTileAddr
	sbc #chrColumn
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	sbc #0
	sta drawMapTileAddr + 1

	dex					; until all skulls done
	bne drawLevelIntroSkullAddr

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw the skulls
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx levelSkulls				; get the number of skulls

.drawLevelIntroSkullImg

	lda #mapTileSkull			; draw the skull
	jsr drawMapTile

	lda #chrColumn				; leave space between skull
	jsr drawMapTileAddrAdvance

	dex					; until all skulls done
	bne drawLevelIntroSkullImg

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw the letters
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #lo(screenAddr + 9 * chrColumn + 14 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 9 * chrColumn + 14 * chrRow)
	sta drawMapTileAddr + 1
	
	ldx #2					; start at last letter

.drawLevelIntroLettersImg

	lda levelLetters, x			; draw a letter from the list
	jsr drawMapTile

	lda #chrColumn				; leave space between letters
	jsr drawMapTileAddrAdvance

	dex					; until all letters done
	bpl drawLevelIntroLettersImg

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw the hearts
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #lo(screenAddr + 9 * chrColumn + 17 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 9 * chrColumn + 17 * chrRow)
	sta drawMapTileAddr + 1
	
	ldx #3					; 3 hearts to draw

.drawLevelIntroHeartsImg

	lda #mapTileHeart			; draw a heart
	jsr drawMapTile

	lda #chrColumn				; leave space between hearts
	jsr drawMapTileAddrAdvance

	dex					; until all hearts done
	bne drawLevelIntroHeartsImg

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw shield and number of rounds if shield != 0
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda shield				; draw good luck or shield text
	beq drawLevelIntroGoodLuck

	jsr drawString
	equb pixels1
	equw screenAddr + 2 + 7 * chrColumn + 20 * chrRow
	equs "SHIELD ", &ff
	
	lda #pixels7
	sta drawChrColor
	lda shield
	jsr drawBcd

	jmp drawLevelIntroWait

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw good luck if shield == 0
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawLevelIntroGoodLuck

	jsr drawString
	equb pixels1
	equw screenAddr + 2 + 7 * chrColumn + 20 * chrRow
	equs "GOOD LUCK", &ff
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; process sound and colors until timer expires
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawLevelIntroWait

	jsr waitVsyncUpper			; wait upper half, read analogue joystick (if enabled)
	jsr waitVsyncLower			; wait lower half, read analogue joystick (if enabled)

	jsr processSound			; process sound effects and music

	jsr updateSkullColor			; update the skull palette color

	jsr updateBonusColor			; update the bonus letters palette colors

	jsr drawScore				; draw score (1 digit per loop)

	lda pauseCounter			; repeat until time expires
	bne drawLevelIntroWait

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawBonusGraphics				draw the bonus screen graphics from the list
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusGraphicsList

	equb  4,  1,  3,  3,  2,  7,  2,  9,  2, 12,  2, 14,  3, 18,  4, 20
	equb  9,  1, 10,  3, 11,  7, 11,  9, 11, 12, 11, 14, 10, 18,  9, 20
	equb &ff

.drawBonusGraphicsTile

	equb extraTileFlower0TL, extraTileFlower0TR, extraTileFlower0BL, extraTileFlower0BR
	equb extraTileFlower1TL, extraTileFlower1TR, extraTileFlower1BL, extraTileFlower1BR
	equb extraTileFlower2TL, extraTileFlower2TR, extraTileFlower2BL, extraTileFlower2BR
	equb extraTileFlower3TL, extraTileFlower3TR, extraTileFlower3BL, extraTileFlower3BR

	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusGraphics

	lda bonusSpecialActive			; if special bonus is active then draw some skulls in ladybugs path
	beq drawBonusGraphicsFlowers

	lda #lo(screenAddr + 9 * chrRow + 9 * chrColumn)
	sta drawMapTileAddr
	lda #hi(screenAddr + 9 * chrRow + 9 * chrColumn)
	sta drawMapTileAddr + 1

	ldx #3
	
.drawBonusGraphicsSkulls

	lda #mapTileSkull
	jsr drawMapTile

	lda #chrColumn
	jsr drawMapTileAddrAdvance

	dex
	bne drawBonusGraphicsSkulls

	lda #mapTileSkull			; and place them in the tile map so ladybug can pass over and redraw them
	sta tileMap + 8 * 23 + 9
	sta tileMap + 8 * 23 + 11
	sta tileMap + 8 * 23 + 13

.drawBonusGraphicsFlowers

	ldx #0					; index into graphics positions
		
.drawBonusGraphicsLoop

	ldy drawBonusGraphicsList, x		; get byte from table
	bpl drawBonusGraphicsXY
	rts

.drawBonusGraphicsXY

	clc					; convert Y and X from table into screen address
	lda screenRowLo, y
	inx
	ldy drawBonusGraphicsList, x
	adc screenColumnLo, y
	sta drawMapTileAddr
	dex
	ldy drawBonusGraphicsList, x
	lda screenRowHi, y
	inx
	ldy drawBonusGraphicsList, x
	adc screenColumnHi, y
	sta drawMapTileAddr + 1

.drawBonusGraphicsRandom

	jsr drawRandomFlower			; draw a single random flower

	inx					; continue until all flowers drawn
	beq drawBonusGraphicsExit
	jmp drawBonusGraphicsLoop

.drawBonusGraphicsExit

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawBonusScreen
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusScreen

	lda #bonusTime				; set display time
	sta pauseCounter

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; initialize playfieldMiddle
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr playfieldMiddleWithTimer		; initialize and draw empty playfield with timer, initialize all sprites as blanked and erased

	jsr drawBonusGraphics			; draw the bonus screen graphics

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; special bonus text, skulls, play special bonus music
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda bonusSpecialActive			; if special bonus active then draw special bonus text
	bne drawBonusScreenSpecialActive
	jmp drawBonusScreenExtra

.drawBonusScreenSpecialActive

	lda #palSkull + palRed			; make sure skulls are red
	sta ulaPalette

	jsr drawString				; draw the bonus text
	equb pixels1
	equw screenAddr + 2 + 8 + 3 * chrColumn + 16 * chrRow
	equs "CONGRATULATIONS!", &ff

	jsr drawString
	equb pixels5
	equw screenAddr + 2 + 8 + 2 * chrColumn + 18 * chrRow
	equs "YOU WIN ", &ff

	lda #pixels7
	sta drawChrColor
	
	lda #specialBonusScore + '0'
	jsr drawChr
	lda #0
	jsr drawBcd
	lda #0
	jsr drawBcd
	lda #'0'
	jsr drawChr

	jsr drawString
	equb pixels5
	equw screenAddr + 2 + 8 + 17 * chrColumn + 18 * chrRow
	equs "PTS", &ff

	jsr drawString
	equb pixels3
	equw screenAddr + 2 + 4 * chrColumn + 20 * chrRow
	equs "AND", &ff

	jsr drawString
	equb pixels7
	equw screenAddr + 2 + 8 * chrColumn + 20 * chrRow
	equb &ff

	lda #specialBonusShield + '0'
	jsr drawChr
	
	jsr drawString
	equb pixels3
	equw screenAddr + 2 + 10 * chrColumn + 20 * chrRow
	equs "SHIELDS", &ff

	lda #lo(screenAddr + 18 * chrColumn + 20 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 18 * chrColumn + 20 * chrRow)
	sta drawMapTileAddr + 1

	lda #mapTileSkull
	jsr drawMapTile

	lda #sfxMusicSpecial			; play special bonus music
	jsr playSound

	jmp drawBonusScreenAnimation

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; extra bonus text, play extra bonus music
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusScreenExtra

	lda bonusExtraActive			; if extra bonus active
	beq drawBonusScreenDiamond

	jsr drawString				; draw the bonus text
	equb pixels1
	equw screenAddr + 2 + 16 + 3 * chrColumn + 16 * chrRow
	equs "CONGRATULATIONS!", &ff

	jsr drawString
	equb pixels5
	equw screenAddr + 2 + 7 * chrColumn + 18 * chrRow
	equs "YOU WIN ", &ff

	lda #pixels7
	sta drawChrColor

	lda #extraBonusLives + '0'
	jsr drawChr

	if extraBonusLives = 1

	jsr drawString
	equb pixels3
	equw screenAddr + 2 + 8 + 4 * chrColumn + 20 * chrRow
	equs "EXTRA LADY BUG", &ff

	else

	jsr drawString
	equb pixels3
	equw screenAddr + 2 + 4 * chrColumn + 20 * chrRow
	equs "EXTRA LADY BUGS", &ff

	endif

	lda #sfxMusicExtra			; play extra bonus music
	jsr playSound

	jmp drawBonusScreenAnimation

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; diamond bonus text, play extra bonus music
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusScreenDiamond

	lda bonusDiamondActive			; if diamond bonus active
	bne drawBonusScreenDiamondGraphics
	jmp drawBonusScreenExit

.drawBonusScreenDiamondGraphics

	lda #lo(screenAddr + 8 * chrRow + 8 * chrColumn + 8)
	sta drawMapTileAddr
	lda #hi(screenAddr + 8 * chrRow + 8 * chrColumn + 8)
	sta drawMapTileAddr + 1
	
	ldx #extraTileDiamond
	
.drawBonusScreenDiamondGraphicsLoop1

	txa
	jsr drawExtraTile			; draw tile from extra set

	inx
	cpx #extraTileDiamond + 6
	bne drawBonusScreenDiamondGraphicsLoop1

	lda #lo(screenAddr + 9 * chrRow + 8 * chrColumn + 8)
	sta drawMapTileAddr
	lda #hi(screenAddr + 9 * chrRow + 8 * chrColumn + 8)
	sta drawMapTileAddr + 1
	
.drawBonusScreenDiamondGraphicsLoop2

	txa
	jsr drawExtraTile			; draw tile from extra set

	inx
	cpx #extraTileDiamond + 12
	bne drawBonusScreenDiamondGraphicsLoop2

	lda #lo(screenAddr + 10 * chrRow + 9 * chrColumn + 8)
	sta drawMapTileAddr
	lda #hi(screenAddr + 10 * chrRow + 9 * chrColumn + 8)
	sta drawMapTileAddr + 1
	
.drawBonusScreenDiamondGraphicsLoop3

	txa
	jsr drawExtraTile			; draw tile from extra set

	inx
	cpx #extraTileDiamond + 16
	bne drawBonusScreenDiamondGraphicsLoop3

	jsr drawString				; draw the bonus text
	equb pixels1
	equw screenAddr + 2 + 8 + 2 * chrColumn + 16 * chrRow
	equs "YOU DISCOVERED THE", &ff

	jsr drawString
	equb pixels3
	equw screenAddr + 2 + 8 + 4 * chrColumn + 18 * chrRow
	equs "DIAMOND GARDEN", &ff

	jsr drawString
	equb pixels5
	equw screenAddr + 2 + 2 * chrColumn + 20 * chrRow
	equs "AND WIN ", &ff

	lda #pixels7
	sta drawChrColor
	
	lda #diamondBonusScore
	jsr drawBcd
	lda #0
	jsr drawBcd
	lda #0
	jsr drawBcd
	lda #'0'
	jsr drawChr

	jsr drawString
	equb pixels5
	equw screenAddr + 2 + 18 * chrColumn + 20 * chrRow
	equs "PTS", &ff

	lda #sfxMusicExtra			; play extra bonus music
	jsr playSound

	jmp drawBonusScreenWaitUpper		; skip the animation

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup animation for ladybug
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusScreenAnimation

	lda #animateLadybugBonus		; initialize a ladybug animation
	jsr animateLadybugInitialize

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; wait for upper sync and process
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusScreenWaitUpper

	jsr waitVsyncUpper			; wait upper half, read analogue joystick (if enabled)

	jsr animateLadybug			; update ladybug direction and frame counter
	
	jsr redrawSprites			; draw ladybug

	jsr moveSprites				; move ladybug

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; wait for lower sync and process
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusScreenWaitLower

	jsr waitVsyncLower			; wait lower half, read analogue joystick (if enabled)

	jsr processSound			; process sound effects and music

	jsr redrawSprites			; draw ladybug

	jsr updateAnimationFrame		; update the animtion frame number

	jsr updateObjectTimer			; update object timer, mode and palette

	jsr updateBonusColor			; update the bonus letters palette colors

	jsr drawScore				; draw score (1 digit per loop)

	lda pauseCounter			; repeat until time expires
	bne drawBonusScreenWaitUpper

	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusScreenExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; ladybug animation tables
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.animateLadybugEntryTable

ladybugEntryX		= 8			; starting position
ladybugEntryY		= 168

	equb (ladybugStartX - ladybugEntryX) - 1, moveRight
	equb (ladybugEntryY - ladybugStartY) - 1, moveUp
	equb 1, moveUp + moveStop
	equb 0

	;---------------------------------------------------------------------------------------------------------------------------------------------

.animateLadybugBonusTable

ladybugBonusX		= 168			; starting position
ladybugBonusY		= 11

	equb 34, moveLeft
	equb 53, moveDown
	equb 87, moveLeft
	equb 43, moveDown
	equb 36, moveLeft
	equb 56, moveDown
	equb 1, moveDown + moveStop + spriteBlanking
	equb 0

	;---------------------------------------------------------------------------------------------------------------------------------------------

.animateLadybugNameRegTable

ladybugHighScoreX	= 96			; starting position
ladybugHighScoreY	= 69

	equb 65, moveRight
	equb 98, moveDown
	equb 145, moveLeft
	equb 98, moveUp
	equb 145, moveRight
	equb 98, moveDown
	equb 145, moveLeft
	equb 98, moveUp
	equb 145, moveRight
	equb 98, moveDown
	equb 145, moveLeft
	equb 98, moveUp
	equb 145, moveRight
	equb 98, moveDown
	equb 145, moveLeft
	equb 98, moveUp
	equb 145, moveRight
	equb 98, moveDown
	equb 145, moveLeft
	equb 98, moveUp
	equb 145, moveRight
	equb 98, moveDown
	equb 145, moveLeft
	equb 98, moveUp
	equb 145, moveRight
	equb 98, moveDown
	equb 145, moveLeft
	equb 98, moveUp
	equb 145, moveRight
	equb 98, moveDown
	equb 145, moveLeft
	equb 98, moveUp
	equb 145, moveRight
	equb 11, moveDown
	equb 16, moveLeft
	equb 1, moveLeft + moveStop
	equb 0

	;---------------------------------------------------------------------------------------------------------------------------------------------

.animateLadybugMainMenuTable

ladybugMainMenuX	= 165			; starting position
ladybugMainMenuY	= 89

	equb 200, moveUp + moveStop
	equb 75, moveDown
	equb 151, moveLeft
	equb 75, moveUp
	equb 200, moveUp + moveStop
	equb 76, moveDown
	equb 151, moveRight
	equb 75, moveUp
	equb 0

	;---------------------------------------------------------------------------------------------------------------------------------------------

.animateLadybugInstructionsTable

ladybugInstructionsX	= 96			; starting position
ladybugInstructionsY	= 141

	equb 57, moveRight
	equb 23, moveDown
	equb 132, moveLeft
	equb 23, moveUp
	equb 74, moveRight
	equb 0

	;---------------------------------------------------------------------------------------------------------------------------------------------

animateLadybugEntry		= 0		; entry animation index
animateLadybugBonus		= 1		; bonus animation index
animateLadybugNameReg		= 2		; high score animation index
animateLadybugMainMenu		= 3		; main menu animation index
animateLadybugInstructions	= 4		; instructions animation index

	;---------------------------------------------------------------------------------------------------------------------------------------------

.animateLadybugInitTable

	equw animateLadybugEntryTable
	equb ladybugEntryX
	equb ladybugEntryY
	
	equw animateLadybugBonusTable
	equb ladybugBonusX
	equb ladybugBonusY
	
	equw animateLadybugNameRegTable
	equb ladybugHighScoreX
	equb ladybugHighScoreY

	equw animateLadybugMainMenuTable
	equb ladybugMainMenuX
	equb ladybugMainMenuY

	equw animateLadybugInstructionsTable
	equb ladybugInstructionsX
	equb ladybugInstructionsY



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; animateLadybugInitialize			setup everything ready for a ladybug animation
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.animateLadybugInitialize

	asl a					; create index into table
	asl a
	tay
	
	lda animateLadybugInitTable, y		; setup start address for animation
	sta animateLadybugAddr
	iny
	lda animateLadybugInitTable, y
	sta animateLadybugAddr + 1
	iny
	
	lda animateLadybugInitTable, y		; set initial position of ladybug
	sta spritesX + 0
	iny
	lda animateLadybugInitTable, y
	sta spritesY + 0

	lda spriteBaseImg			; set image for ladybug
	sta spritesImg + 0
	
	lda #0					; initialize counter
	sta animateLadybugCounter

	lda #&ff				; enable animation
	sta animateLadybugActive

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; animateLadybug				read frame count and direction from animate tables
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.animateLadybug

	lda animateLadybugActive		; if animation enabled
	beq animateLadybugExit

	lda animateLadybugCounter		; if ladybug animate counter != 0
	bne animateLadybugNextFrame		; then bump counter

	ldy #0					; else get byte from animation table
	lda (animateLadybugAddr), y

	beq animateLadybugEnd			; if byte = 0 then end the animation
	
	sta animateLadybugCounter		; else store byte into animation counter
	
	ldy #1
	lda (animateLadybugAddr),y		; get next byte and store in ladybug direction
	sta spritesDir + 0
	
	clc					; advance to next table entry
	lda animateLadybugAddr
	adc #2
	sta animateLadybugAddr
	bcc animateLadybugExit
	inc animateLadybugAddr + 1

.animateLadybugExit

	rts					; return
	
.animateLadybugNextFrame

	dec animateLadybugCounter		; bump counter
	
	rts					; return
	
.animateLadybugEnd

	sta animateLadybugActive		; deactivate animation

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; updateBonusColor				update the special/extra letter color palette
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateBonusColor

	lda vsyncCounter			; if vsyncCounter & 0x08 == 0 then use color set 0 else use color set 1
	and #%00001000
	bne updateBonusColorSet1
	
.updateBonusColorSet0

	lda #palSpecial0 + palRed		; color set 0 red magenta yellow green
	sta ulaPalette
	lda #palSpecial1 + palMagenta
	sta ulaPalette
	lda #palExtra0 + palYellow
	sta ulaPalette
	lda #palExtra1 + palGreen
	sta ulaPalette
	
	rts					; return

.updateBonusColorSet1

	lda #palSpecial0 + palMagenta		; color set 1 magenta red green yellow
	sta ulaPalette
	lda #palSpecial1 + palRed
	sta ulaPalette
	lda #palExtra0 + palGreen
	sta ulaPalette
	lda #palExtra1 + palYellow
	sta ulaPalette
	
	rts					; return
	



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; clearTileMap					fill tileMap with blank tile (23 * 23 tiles = &211 bytes)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.clearTileMap

	ldx #0
	
	lda #mapTileBlank

.clearTileMapLoop

	sta tileMap, x				; store blank tile in &211 bytes
	sta tileMap + &100, x
	sta tileMap + &111, x
	inx
	bne clearTileMapLoop
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawString					draw text string terminanated by byte with bit 7 set
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			string			string is read from memory following the jsr
;						format is
;						1 byte color mask
;						2 bytes screen address
;						text string terminated with a byte with bit 7 set (not printed)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			PC			returns to address following string terminator
;			drawChrAddr		points to next chr position on screen
;			A			destroyed
;			X			preserved
;			y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawString

	sty drawStringSaveY			; preserve register

	pla					; pop return address and add 1 (6502 quirk) to get address of text
	clc
	adc #1
	sta drawTextAddr
	pla
	adc #0
	sta drawTextAddr + 1

	ldy #0					; get color
	lda (drawTextAddr), y
	sta drawChrColor
	
	iny					; get screen address
	lda (drawTextAddr), y
	sta drawChrAddr
	iny
	lda (drawTextAddr), y
	sta drawChrAddr + 1
	
	tya					; point drawTextAddr to the text
	sec
	adc drawTextAddr
	sta drawTextAddr
	bcc drawStringText
	inc drawTextAddr + 1

.drawStringText

	jsr drawText				; draw the text until negative byte
	
	ldy drawStringSaveY			; restore register

	lda drawTextAddr + 1			; push string terminator address (return address - 1 as the 6502 adds 1 to the popped address)
	pha
	lda drawTextAddr
	pha
	
	rts					; jump to terminator address + 1



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawText					draw text string terminanated by byte with bit 7 set
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			drawTextAddr		points to first character of string
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			drawChrAddr		points to next chr position on screen
;			A			destroyed
;			X			preserved
;			y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawText

	sty drawTextSaveY			; preserve register

	ldy #0					; start at first text chr

.drawTextLoop

	lda (drawTextAddr), y			; get chr from text string

	bmi drawTextAdjustAddr			; if bit 7 set then end of text string
	
	cmp #&0d				; if its a cr or lf (from included text file) then skip it
	beq drawTextNextChr
	cmp #&0a
	beq drawTextNextChr
	
	jsr drawChr				; else draw chr

.drawTextNextChr

	iny					; do next chr
	bne drawTextLoop

.drawTextAdjustAddr

	tya					; add text length to addr
	clc
	adc drawTextAddr
	sta drawTextAddr
	bcc drawTextExit
	inc drawTextAddr + 1

.drawTextExit

	ldy drawTextSaveY			; restore register

	rts					; done
	
	
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; processSound					; process sound effect/music tables and send data to psg
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------


.processSound

	ldy #soundChannels - 1			; process from soundChannels - 1 to 0
	ldx #2 * (soundChannels - 1)		; offset to pointers

.processSoundLoop

	lda soundTimers, y			; if soundTimer = 0 then skip it
	beq processSoundNext

	sec					; timer = timer - 1
	sbc #1
	sta soundTimers, y
	bne processSoundNext			; if timer != 0 then skip it
	
.processSoundGetData

	lda (soundAddrPtr, x)			; get byte from sound table

	and #&c0				; if its timer data then do the timer stuff
	cmp #&40
	beq processSoundGetTimer

	lda optionSound				; else if sound enabled
	beq processSoundNextByte

	lda (soundAddrPtr, x)			; write data to psg chip
	jsr psgWrite

.processSoundNextByte

	inc soundAddrPtr, x			; inc pointer and get another byte from table
	bne processSoundGetData
	inc soundAddrPtr + 1, x
	bne processSoundGetData			; bne used here as branch always

.processSoundGetTimer

	lda (soundAddrPtr, x)			; get new timer value
	and #&3f

	sta soundTimers, y			; store new timer value

	inc soundAddrPtr, x			; inc pointer
	bne processSoundNext
	inc soundAddrPtr + 1, x

.processSoundNext

	dex					; goto next sound effect
	dex
	dey

	bpl processSoundLoop			; continue until all channels processed

.processSoundExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; playSoundSilence				; silence all psg and software channels
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.playSoundSilence

	lda #0					; shut down all software sound channels
	sta soundTimers + 0
	sta soundTimers + 1
	sta soundTimers + 2
	sta soundTimers + 3
	sta soundTimers + 4
	sta soundTimers + 5

	lda #&9f				; silence all psg channels
	
.playSoundSilenceLoop

	pha					; save current channel
	
	jsr psgWrite				; silence channel
	
	pla					; get current channel
	
	clc					; next channel
	adc #&20
	
	bmi playSoundSilenceLoop		; until done
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; .playSoundTimer				play the enemy timer sound at the correct volume
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.playSoundTimer

	lda optionTimerVolume			; if optionTimerVolume != 0
	bne playSoundTimerVolume

	rts
	
.playSoundTimerVolume

	clc					; then select timer bleep volume
	adc #sfxTimerLow - 1

	; contine to play sound			; play timer sound effect and exit



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; playSound					send sound data to soundEffect to trigger sound
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		playSoundAddr		1st entry to sound data
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.playSound

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; save registers
	;---------------------------------------------------------------------------------------------------------------------------------------------

	sta playSoundSaveA			; save sound number

	stx playSoundSaveX			; save registers
	sty playSoundSaveY

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; create table address from sound number and get sound effect information
	;---------------------------------------------------------------------------------------------------------------------------------------------

	asl a					; make table offset
	tay
	
	lda sfxAddrTable, y			; get sound effect data address from table
	sta playSoundAddr
	iny
	lda sfxAddrTable, y
	sta playSoundAddr + 1

	ldy #0					; get sound channel number from table
	lda (playSoundAddr), y

	tay					; y = channel number
	
	asl a					; x = channel index
	tax

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; channel 0 high priority (music)
	;---------------------------------------------------------------------------------------------------------------------------------------------

	cpy #0					; if channel = 0 (music)
	bne playSoundLowPriority
	
	sty soundTimers + 0			; then shut down all channels except 5 (sfxMunch)
	sty soundTimers + 1
	sty soundTimers + 2
	sty soundTimers + 3
	sty soundTimers + 4

	lda #&9f				; silence psg channels 0,1,2
	jsr psgWrite
	lda #&bf
	jsr psgWrite
	lda #&df
	jsr psgWrite

	jmp playSoundNow			; play the music

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; channels 1 to 5 low priorty (sound effects)
	;---------------------------------------------------------------------------------------------------------------------------------------------

.playSoundLowPriority				; channels 1 to 5 (sound effects)

	lda soundTimers + 0			; if channel 0 (music) is currently active
	beq playSoundCheckChannel3

	lda playSoundSaveA			; then only allow sfxMunch
	cmp #sfxMunch
	beq playSoundNow
	bne playSoundExit

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check channel 3 is active (sfxObject/sfxSkull) and if so then skip playing turnstile or timer sounds if requested
	;---------------------------------------------------------------------------------------------------------------------------------------------

.playSoundCheckChannel3

	lda soundTimers + 3			; if sound channel 3 active (sfxObject/sfxSkull)
	beq playSoundCheckObjectSkull

	lda playSoundSaveA			; then get sound number

	cmp #sfxTurnstile			; if sfxTurnstile then skip it
	beq playSoundExit

	cmp #sfxTimerLow			; if sfxTimerLow then skip it
	beq playSoundExit
	
	cmp #sfxTimerMedium			; if sfxTimerMedium then skip it
	beq playSoundExit

	cmp #sfxTimerHigh			; if sfxTimerHigh then skip it
	beq playSoundExit

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check for sfxObject and sfxSkull
	;---------------------------------------------------------------------------------------------------------------------------------------------

.playSoundCheckObjectSkull

	lda playSoundSaveA			; get sound number
	
	cmp #sfxObject				; if sfxObject then shut down channels 1 (sfxTimer) and 4 (sfxTurnstile)
	beq playSoundDisable14
	
	cmp #sfxSkull				; if sfxSkull then shut down channels 1 (sfxTimer) and 4 (sfxTurnstile)
	beq playSoundDisable14

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; copy sound data address and set timer to 1 to trigger start of sound
	;---------------------------------------------------------------------------------------------------------------------------------------------

.playSoundNow

	clc					; copy sound data address to channel soundPtr
	lda playSoundAddr
	adc #1
	sta soundAddrPtr, x
	lda playSoundAddr + 1
	adc #0
	sta soundAddrPtr + 1, x

	lda #1					; set channel soundTimer to 1 to trigger sound on the next game loop
	sta soundTimers, y

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; restore registers and exit
	;---------------------------------------------------------------------------------------------------------------------------------------------

.playSoundExit

	ldy playSoundSaveY			; restore registers
	ldx playSoundSaveX

	rts					; return

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; disable channel 1 (sfxTimer) and channel 4 (sfxTurnstile)
	;---------------------------------------------------------------------------------------------------------------------------------------------

.playSoundDisable14

	lda #0

	sta soundTimers + 1			; disable channel 1 (sfxTimer)
	sta soundTimers + 4			; disable channel 4 (sfxTurnstile)

	beq playSoundNow			; and play sound



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; checkForObject				check if ladybug is over the bonus vegetable/diamond
;						also read tile under ladybug and handle any objects found
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.bonusBitsMultiplierFlags			; multiplier bit flags

	equb &fb, &f9, &f8

.objectScore 

	equb &10, &80, &30			; object multiplier score cyan 100, red 800, yellow 300

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObject

	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; check for vegetable/diamond is active and that ladybug is in the center box
	;-------------------------------------------------------------------------------------------------------------------------------------------------

	lda bonusItemActive			; if bonus item is active
	beq checkForObjectTile

	lda spritesX + 0			; and ladybug is in the center box
	cmp #centerBoxX
	bne checkForObjectTile
	lda spritesY + 0
	cmp #centerBoxY
	bne checkForObjectTile
	
	lda #0					; deactivate the bonus item
	sta bonusItemActive
	
	lda bonusDiamondEnable			; if diamond is enabled then
	beq checkForObjectVegetableScore

	lda level				; if level >= bonusDiamondLevel
	cmp #bonusDiamondLevel
	bcc checkForObjectVegetableScore

	lda #&ff				; enable diamond bonus
	sta bonusDiamondActive
	
	lda #0					; disable the diamond after this
	sta bonusDiamondEnable
	
	lda #letterBonusTime			; pause ladybug and enemies
	sta pauseLadybug
	sta pauseEnemy

	lda #sfxMusicLetters			; play bonus letters music and return
	jmp playSound



	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; vegetable score
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectVegetableScore

	lda #&ff				; activate the vegetable score display
	sta vegetableScoreActive

	lda spritesDir + 0			; blank ladybug sprite
	ora #spriteBlanking
	sta spritesDir + 0

	lda #vegetableLadybugTime		; pause ladybug
	sta pauseLadybug

	lda #vegetableEnemyTime			; pause enemies
	sta pauseEnemy

	lda #sfxMusicVegetable			; play vegetable bonus sound
	jsr playSound

	jmp addScoreVegetable			; add the vegetable bonus score and return



	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; check for object tiles (dots, hearts, letters, skulls)
	;-------------------------------------------------------------------------------------------------------------------------------------------------
	
.checkForObjectTile

	jsr offsetDrawMapTileAddr		; adjust drawMapTileAddress to be location underneath ladybug center

	ldy #24					; read tile at ladybug location
	lda (tileMapAddr), y

	cmp #mapTileDot				; if tile = dot then use dot function
	beq checkForObjectDot
	
	cmp #mapTileSkull			; if tile = skull then use skull function
	beq checkForObjectSkull

	cmp #mapTileHeart			; if tile = heart then use heart function
	beq checkForObjectHeart

	cmp #mapTileS				; if tile >= mapTileS and < mapTileR + 1 (letter tiles S P E C I A L E X T R)
	bcc checkForObjectTileExit
	
	cmp #mapTileR + 1
	bcc checkForObjectLetter		; then use letter function

.checkForObjectTileExit

	rts					; return



	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; dot tile
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectDot

	lda #mapTileBlank			; replace dot with blank tile
	sta (tileMapAddr), y
	
	dec levelEdibles			; dec edibles

	lda #sfxMunch				; play munch sound
	jsr playSound

	lda #1					; bump score 10 points x multiplier and return
	jmp addScoreMultiply



	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; skull tile
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectSkull

	lda shield				; if we are vulnerable (skull shield = 0)
	bne checkForObjectSkullExit

	lda #mapTileBlank			; replace skull with blank tile
	sta (tileMapAddr), y
	
	lda #mapTileBlankObj			; erase skull from screen
	jsr drawMapTile

	jsr ladybugKill				; kill ladybug

.checkForObjectSkullExit

	rts					; return



	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; heart tile
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectHeart

	lda #mapTileBlank			; replace heart with blank tile
	sta (tileMapAddr), y

	dec levelEdibles			; dec edibles

	stx checkForObjectSaveX			; preserve X register

	lda objectMode				; if object mode is cyan then update the multiplier
	bne checkForObjectScore

	ldx scoreMultiplier			; get the current score multiplier

	lda bonusBitsMultiplierFlags, x		; enable multiplier bit in bonus bits
	and bonusBits
	sta bonusBits

	jsr drawPlayfieldUpperBonus		; update playfield upper to show the multiplier changes

	inc scoreMultiplier			; inc the score multiplier

	; continue down to add heart value to score



	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; object score
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectScore

	ldx objectMode				; add score value for object cyan = 100, red = 500, yellow = 300
	lda objectScore, x
	jsr addScoreMultiply

	cpx #objectModeCyan			; if the object was not cyan
	beq checkForObjectScoreNow

	lda #0					; then remove the possibility of getting a diamond bonus
	sta bonusDiamondEnable

.checkForObjectScoreNow

	jsr displayobjectScore			; setup object score display

	lda spritesDir + 0			; blank ladybug sprite
	ora #spriteBlanking
	sta spritesDir + 0

	lda #objectTime				; pause ladybug
	sta pauseLadybug

	cmp pauseEnemy				; only set enemy pause time if its currently less than object time
	bcc checkForObjectExit			; (enemy might already be paused after vegetable bonus so dont overwrite it with a shorter object pause time)
	sta pauseEnemy

.checkForObjectExit

	lda #sfxObject				; play object sound
	jsr playSound

	ldx checkForObjectSaveX			; restore X register

	rts					; return



	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; letter tile
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectLetter

	pha					; save letter on stack

	lda #mapTileBlank			; replace letter with blank tile
	sta (tileMapAddr), y

	dec levelEdibles			; dec edibles

	stx checkForObjectSaveX			; preserve X register

	pla					; pull letter from stack

	sec					; convert letter into index range 0-9
	sbc #objectTileIndex

	ldx objectMode				; get object color mode



	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; red letter mode
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectLetterRed

	cpx #objectModeRed			; if objectMode = objectModeRed

	bne checkForObjectLetterYellow
	
	tax					; get red letter bit value and highlight it
	lda objectLetterBitsRed, x
	and bonusBits + 1
	sta bonusBits + 1
	
	jsr drawPlayfieldUpperBonus		; update upper bonus display

	jmp checkForObjectScore			; add the object score



	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; yellow letter mode
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectLetterYellow

	cpx #objectModeYellow			; if objectMode = objectModeYellow
	bne checkForObjectScore

	tax					; get yellow letter bit value and highlight it
	lda objectLetterBitsYellow, x
	and bonusBits
	sta bonusBits
	
	jsr drawPlayfieldUpperBonus		; update upper bonus display

	jmp checkForObjectScore



	;-------------------------------------------------------------------------------------------------------------------------------------------------

						; bonus bits + 1 = 7 6 5 4 3 2 1 0
						;                    S P E C I A L

.objectLetterBitsRed				; letter tile index and matching bonus bit

	equb %10111111				; "S" clear bit 6 of bonusBits + 1
	equb %11011111				; "P" clear bit 5 of bonusBits + 1
	equb %11101111				; "E" clear bit 4 of bonusBits + 1
	equb %11110111				; "C" clear bit 3 of bonusBits + 1
	equb %11111011				; "I" clear bit 2 of bonusBits + 1
	equb %11111101				; "A" clear bit 1 of bonusBits + 1
	equb %11111110				; "L" clear bit 0 of bonusBits + 1
	equb %11111111				; "X" no effect in red mode
	equb %11111111				; "T" no effect in red mode
	equb %11111111				; "R" no effect in red mode

	;-------------------------------------------------------------------------------------------------------------------------------------------------

						; bonus bits + 0 = 7 6 5 4 3 2 1 0
						;                  E X T R A
.objectLetterBitsYellow

	equb %11111111				; "S" no effect in yellow mode
	equb %11111111				; "P" no effect in yellow mode
	equb %01111111				; "E" clear bit 7 of bonusBits + 0
	equb %11111111				; "C" no effect in yellow mode
	equb %11111111				; "I" no effect in yellow mode
	equb %11110111				; "A" clear bit 3 of bonusBits + 0
	equb %11111111				; "L" clear bit 0 of bonusBits + 1
	equb %10111111				; "X" clear bit 6 of bonusBits + 0
	equb %11011111				; "T" clear bit 5 of bonusBits + 0
	equb %11101111				; "R" clear bit 4 of bonusBits + 0



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; displayobjectScore				setup the img and position to display the object score
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.displayobjectScore

	lda spritesX + 0			; get ladybug position to the nearest 16 pixels in x and y, offset it by 8, 10 for correct positioning
	and #&f0
	ora #8
	sta objectScoreX
	lda spritesY + 0
	and #&f0
	ora #10
	sta objectScoreY

	lda objectMode				; calculate which object score image to display based on color mode and multiplier
	asl a					; objectScoreImg = objectMode * 4 + scoreMultiplier + pointsImgBase
	asl a
	adc scoreMultiplier			; no need for clc, carry is already clear
	adc #pointsBaseImg
	sta objectScoreImg

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; ladybugKill					reduce lives by 1 and initiate death animation
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.ladybugKill

	lda #spriteBlanking			; blank ladybug sprite
	ora spritesDir + 0
	sta spritesDir + 0

	lda #&ff				; enable death animation
	sta ladybugDeathEnable

	lda #0					; remove the possibility of getting a diamond bonus
	sta bonusDiamondEnable

	sta bonusItemActive			; deactivate vegetable/diamond

	sed					; reduce lives by 1 (BCD)
	sec
	lda lives
	sbc #1
	sta lives
	cld

	lda #ladybugDeathTime			; pause ladybug and enemies
	sta pauseLadybug
	sta pauseEnemy

	lda #sfxMusicDeath			; play ladybug death music and return
	jmp playSound



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; updateLadybug					set direction of ladybug if requested direction is valid
;						handle objects under ladybug
;						handle turnstile movement (rewrite background map tiles only, drawing is handled by mainloop)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; return if ladybug is paused or entry animation is enabled
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybug

	lda pauseLadybug			; if ladybug movement is paused then exit
	bne updateLadybugReturn

	lda ladybugEntryEnable			; if ladybug entry movement is enabled then exit
	beq updateLadybugInit
	
.updateLadybugReturn

	rts
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; initialize everything needed for direction, gridlock and tile tests
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugInit

	lda spritesDir + 0			; stop ladybug moving
	ora #moveStop
	sta spritesDir + 0

	and #moveStop eor &ff			; save current direction and blanking (stop bit removed)
	sta updateLadybugOldDir

	lda #&ff				; clear new x and y directions
	sta updateLadybugNewDirX
	sta updateLadybugNewDirY

	sta updateLadybugTileX			; clear found tiles
	sta updateLadybugTileY

	lda spritesX + 0			; save ladybug x for map and screen address conversion
	sta spriteToAddrX

	and #15					; create ladybug grid x flag, on grid = 0, off grid != 0
	eor #8
	sta updateLadybugGridX
	
	lda spritesY + 0			; store ladybug y for map and screen address conversion
	sta spriteToAddrY

	and #15				; create ladybug grid y flag, on grid = 0, off grid != 0
	eor #8
	sta updateLadybugGridY
	
	jsr spriteToAddr			; convert ladybug xy to tileMapAddr and drawTileAddr

	jsr checkForObject			; check for object under ladybug (dots, hearts, letters, skulls) and do required action (points/death)

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check inputs and store selected directions
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugInputStart

	lsr playerInput				; shift start (fire) input into carry and discard

	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugInputLeft

	lsr playerInput				; shift left input into carry

	bcc updateLadybugInputDown		; if left was requested then

	ldx #moveLeft				; set direction left

	lda updateLadybugGridY			; if ladybug y is on exact grid (left direction allowed) then
	bne updateLadybugInputLeftAlign

	stx updateLadybugNewDirX		; set new direction to left
	beq updateLadybugInputDown		; check for down
	
.updateLadybugInputLeftAlign

	lda updateLadybugOldDir			; use old direction to align ladybug with turn
	sta updateLadybugNewDirY

	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugInputDown

	lsr playerInput				; shift down input into carry

	bcc updateLadybugInputUp		; if down was requested then

	ldx #moveDown				; set direction down

	lda updateLadybugGridX			; if ladybug x is on exact grid (down direction allowed) then
	bne updateLadybugInputDownAlign

	stx updateLadybugNewDirY		; set new direction to down
	beq updateLadybugInputUp		; check for up

.updateLadybugInputDownAlign

	lda updateLadybugOldDir			; use old direction to align ladybug with turn
	sta updateLadybugNewDirX

	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugInputUp

	lsr playerInput				; shift up input into carry

	bcc updateLadybugInputRight		; if up was requested then

	ldx #moveUp				; set direction up

	lda updateLadybugGridX			; if ladybug y is on exact grid (up direction allowed) then
	bne updateLadybugInputUpAlign

	stx updateLadybugNewDirY		; set new direction to up
	beq updateLadybugInputRight		; check for right

.updateLadybugInputUpAlign

	lda updateLadybugOldDir			; else use old direction to align ladybug with turn
	sta updateLadybugNewDirX
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugInputRight

	lsr playerInput				; shift right input into carry

	bcc updateLadybugCheckGrid		; if right was requested

	ldx #moveRight				; set direction right

	lda updateLadybugGridY			; if ladybug y is on exact grid (right direction allowed) then
	bne updateLadybugInputRightAlign

	stx updateLadybugNewDirX		; set new x direction to right
	beq updateLadybugCheckGrid		; go check for grid alignment

.updateLadybugInputRightAlign

	lda updateLadybugOldDir			; else use old direction to align ladybug with turn
	sta updateLadybugNewDirY

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check if ladybug is off or on grid
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugCheckGrid

	lda updateLadybugGridX			; check ladybug off or on grid
	ora updateLadybugGridY
	beq updateLadybugOnGrid
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; off grid check for horizontal
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugOffGridX

	ldx updateLadybugNewDirX		; if ladybug x direction chosen then set new direction
	bmi updateLadybugOffGridY
	bpl updateLadybugNewDirection

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; off grid check for vertical
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugOffGridY

	ldx updateLadybugNewDirY		; if ladybug y direction chosen then set new direction
	bmi updateLadybugExit

	; continue down to set new direction

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; set ladybug new direction combined with original blanking
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugNewDirection

	lda spritesDir + 0			; get blanking bit
	and #spriteBlanking
	
	stx spritesDir + 0			; save new direction

	ora spritesDir + 0			; and combine with blanking flag
	sta spritesDir + 0

	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugExit

	rts					; return

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; on grid handle path tiles, turnstiles and direction. if two directions selected then choose the one that isnt the original direction
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugOnGrid

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; on grid check horizontal
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugOnGridX

	ldx updateLadybugNewDirX		; if ladybug x direction chosen then
	bmi updateLadybugOnGridY

	jsr updateLadybugCheckPath		; check tile in front of ladybug
	sty updateLadybugTileX

	bne updateLadybugOnGridY		; if tile was not a path/turnstile then
	
	lda #&ff				; remove x tile (cannot turn this direction)
	sta updateLadybugTileX

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; on grid check vertical
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugOnGridY

	ldx updateLadybugNewDirY		; if ladybug y direction chosen then
	bmi updateLadybugCheckDualTile

	jsr updateLadybugCheckPath		; check tile in front of ladybug
	sty updateLadybugTileY

	bne updateLadybugCheckDualTile		; if tile was not a path/turnstile then
	
	lda #&ff				; remove y tile (cannot turn this direction)
	sta updateLadybugTileY

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; if two directions were pressed at the junction and both tiles are valid then choose direction thats not the current direction
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugCheckDualTile

	lda updateLadybugTileX			; if tileX is a valid path then
	cmp #&ff
	beq updateLadybugCheckTileY
	
	lda updateLadybugTileY			; if tileY is a valid path then
	cmp #&ff
	beq updateLadybugCheckTileX
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugCheckDualTileX

	lda updateLadybugNewDirX		; remove x direction if its the same as the current direction
	cmp updateLadybugOldDir
	bne updateLadybugCheckDualTileY
	
	lda #&ff
	sta updateLadybugTileX

	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugCheckDualTileY

	lda updateLadybugNewDirY		; remove y direction if its the same as the current direction
	cmp updateLadybugOldDir
	bne updateLadybugCheckTileX

	lda #&ff
	sta updateLadybugTileY

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check if we can use x direction
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugCheckTileX

	lda updateLadybugTileX			; if x direction tile is a valid path
	cmp #&ff
	beq updateLadybugCheckTileY

	sta updateLadybugSave			; save tile

	ldx updateLadybugNewDirX		; set x direction
	jsr updateLadybugNewDirection
	
	jmp updateLadybugTurnstile		; and check if turnstile push required

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check if we can use y direction
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugCheckTileY

	lda updateLadybugTileY			; if y direction tile is a valid path
	cmp #&ff
	beq updateLadybugExit

	sta updateLadybugSave			; save tile

	ldx updateLadybugNewDirY		; set y direction
	jsr updateLadybugNewDirection

	; continue down to check if turnstile push is required



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check if tile is a turnstile and if it is then push it in the required direction
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugTurnstile

	lda updateLadybugSave			; if saved tile is not a turnstile then exit
	and #wallSolid
	cmp #wallTurnstile
	bne updateLadybugExit

	lda updateLadybugSave			; get tile

	ldy #0

	cmp #mapTileTurnstileU + wallTurnstile	; if up tile found then y = 0
	beq updateLadybugTurnstilePush

	iny
	cmp #mapTileTurnstileD + wallTurnstile	; if down tile found then y = 1
	beq updateLadybugTurnstilePush

	iny
	cmp #mapTileTurnstileL + wallTurnstile	; if left tile found then y = 2
	beq updateLadybugTurnstilePush

	iny					; remaining tile must be right, y = 3

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; push the turnstile in the required direction (rewrite background tile buffer and setup screen address for drawing (handled in gameloop))
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugTurnstilePush

	sty updateLadybugSave			; save the tileDir
	
	txa					; index = new ladybug direction * 4 + tileDir
	and #&03
	asl a
	asl a
	ora updateLadybugSave
	tay

	lda spritesX + 0			; calculate address for turnstile tileMap alteration (vertical or horizontal)
	clc
	adc updateLadybugTurnstileX, y
	sta spriteToAddrX
	lda spritesY + 0
	clc
	adc updateLadybugTurnstileY, y
	sta spriteToAddrY
	jsr spriteToAddr

	lda drawMapTileAddr			; copy screen tile address to turnstile screen address
	sta drawTurnstileAddr
	lda drawMapTileAddr + 1
	sta drawTurnstileAddr + 1

	txa					; choose horizontal or vertical from ladybug direction
	and #moveLeft				; up/down = 0/1, left/right = 2/3 so checking for left also check for right !!
	bne updateLadybugTurnstileHorizontal	
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; vertical turnstile
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugTurnstileVertical

	lda #mapTileTurnstileU + wallTurnstile	; place vertical turnStile into tileMap
	ldy #1
	sta (tileMapAddr), y

	lda #mapTileTurnstileCV + wallSolid
	ldy #24
	sta (tileMapAddr), y

	lda #mapTileTurnstileD + wallTurnstile
	ldy #47
	sta (tileMapAddr), y

	lda #mapTileBlank			; remove horizontal turnstile from tileMap
	ldy #23
	sta (tileMapAddr), y
	ldy #25
	sta (tileMapAddr), y

	lda #0					; set turnstile direction to vertical for drawTurnstile
	sta drawTurnstileDir

	bpl updateLadybugTurnstileExit		; (bpl used as branch always)

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; horizontal turnstile
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugTurnstileHorizontal

	lda #mapTileTurnstileL + wallTurnstile	; place horizontal turnstile into tileMap
	ldy #23
	sta (tileMapAddr), y

	lda #mapTileTurnstileCH + wallSolid
	ldy #24
	sta (tileMapAddr), y

	lda #mapTileTurnstileR + wallTurnstile
	ldy #25
	sta (tileMapAddr), y

	lda #mapTileBlank			; remove vertical turnstile from tileMap
	ldy #1
	sta (tileMapAddr), y
	ldy #47
	sta (tileMapAddr), y

	lda #1					; set turnstile direction to horizontal for drawTurnstile
	sta drawTurnstileDir

	; continue down to updateLadybugTurnstileExit

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; play the turnstile sound and exit
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugTurnstileExit

	lda #sfxTurnstile			; play turnstile sound and return
	jmp playSound

	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugTurnstileX

	equb 0,0,8,-8,0,0,8,-8,-8,-8,0,0,8,8,0,0
	
.updateLadybugTurnstileY

	equb 0,0,-8,-8,0,0,8,8,8,-8,0,0,8,-8,0,0

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; updateLadybugCheckPath
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugCheckPath

	ldy moveDirMap, x			; get tile in front of ladybug from supplied direction in x
	lda (tileMapAddr), y
	tay

	and #wallSolid				; return with 0 if solid wall or not 0 if path/turnstile
	eor #wallSolid

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawTurnstile					draw a vertical or horizontal turnstile if required
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			drawTurnstileAddr	screen address for top left corner of 3x3 turnstile block or high byte >= &80 for no draw
;			drawTurnstileDir	direction to draw turnstile, 0=vertical
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawTurnstile

	lda drawTurnstileAddr + 1		; if draw required
	bpl drawTurnstileCheckDir
	rts
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawTurnstileCheckDir

	sta drawMapTileAddr + 1			; then set drawTileAddr
	lda drawTurnstileAddr
	sta drawMapTileAddr

	lda #&ff				; mark as done after this draw is complete
	sta drawTurnstileAddr + 1

	lda drawTurnstileDir			; check direction and draw vertical or horizontal
	bne drawTurnstileHorizontal

	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawTurnstileVertical

	clc					; jump to 1,0
	lda drawMapTileAddr
	adc #lo(chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrColumn)
	sta drawMapTileAddr + 1

	lda #mapTileTurnstileU			; draw up tile
	jsr drawMapTile
	
	clc					; jump to 0,1
	lda drawMapTileAddr
	adc #lo(chrRow - 2 * chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrRow - 2 * chrColumn)
	sta drawMapTileAddr + 1

	lda #mapTileBlank			; draw blank tile
	jsr drawMapTile
	
	lda #mapTileTurnstileCV			; draw center vertical tile
	jsr drawMapTile
	
	lda #mapTileBlank			; draw blank tile
	jsr drawMapTile
	
	clc					; jump to 1,2
	lda drawMapTileAddr
	adc #lo(chrRow - 2 * chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrRow - 2 * chrColumn)
	sta drawMapTileAddr + 1

	lda #mapTileTurnstileD			; draw down tile
	jmp drawMapTile				; and exit
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawTurnstileHorizontal

	clc					; jump to 1,0
	lda drawMapTileAddr
	adc #lo(chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrColumn)
	sta drawMapTileAddr + 1

	lda #mapTileBlank			; draw blank
	jsr drawMapTile
	
	clc					; jump to 0,1
	lda drawMapTileAddr
	adc #lo(chrRow - 2 * chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrRow - 2 * chrColumn)
	sta drawMapTileAddr + 1

	lda #mapTileTurnstileL			; draw left tile
	jsr drawMapTile
	
	lda #mapTileTurnstileCH			; draw center horizontal tile
	jsr drawMapTile
	
	lda #mapTileTurnstileR			; draw right tile
	jsr drawMapTile
	
	clc					; jump to 1,2
	lda drawMapTileAddr
	adc #lo(chrRow - 2 * chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrRow - 2 * chrColumn)
	sta drawMapTileAddr + 1

	lda #mapTileBlank			; draw blank tile
	jmp drawMapTile				; and exit



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; offsetDrawMapTileAddr				add offset to drawMapTileAddr to 1 character and 1 row to be under center of sprite
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.offsetDrawMapTileAddr

	lda #lo(chrRow + chrColumn)		; offset drawMapTileAddr to be under sprite
	clc
	adc drawMapTileAddr
	sta drawMapTileAddr
	lda #hi(chrRow + chrColumn)
	adc drawMapTileAddr + 1
	sta drawMapTileAddr + 1

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; spriteToAddr					convert spriteX + offset, spriteY + offset to tileMapAddr and drawTileAddr
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			spriteToAddrX		sprite x position
;			spriteToAddrY		sprite y position
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			tileMapAddr		tile map address for top left corner of sprite
;			drawTileAddr		screen tile address for top left corner of sprite
;			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

spriteToAddrOffset	= 4			; correction factor for center of tile

	;---------------------------------------------------------------------------------------------------------------------------------------------

.spriteToAddr

	stx spriteToAddrSaveX			; preserve registers
	sty spriteToAddrSaveY

	lda spriteToAddrY			; Y = ((spriteToAddrY + spriteToAddrOffset) / 8) - 1
	clc
	adc #spriteToAddrOffset
	lsr a
	lsr a
	lsr a
	tay
	dey

	lda spriteToAddrX			; A, X = ((spriteToAddrX + spriteToAddrOffset) / 8) - 1
	clc
	adc #spriteToAddrOffset
	lsr a
	lsr a
	lsr a
	tax
	dex
	txa
	
	clc					; convert raw grid coordinates to background buffer (tileMap) address
	adc tileMapRowsLo, y
	sta tileMapAddr
	lda tileMapRowsHi, y
	adc #0
	sta tileMapAddr + 1

	clc					; convert raw grid coordinates to screen address (drawMapTile)
	lda screenColumnLo, x
	adc screenRowLo, y
	sta drawMapTileAddr
	lda screenColumnHi, x
	adc screenRowHi, y
	sta drawMapTileAddr + 1

	ldy spriteToAddrSaveY			; restore registers
	ldx spriteToAddrSaveX				

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; updateHighScoreFirstPlace			; copy first place high score from table to high score and display it
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateHighScoreFirstPlace

	lda highScoreTable + 0			; copy 1st place high score table entry to highScore for display
	sta highScore + 0
	lda highScoreTable + 1
	sta highScore + 1
	lda highScoreTable + 2
	sta highScore + 2

	jmp drawPlayfieldLower			; display it and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; vegetable names
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.vegetableCucumber

	equs "CUCUMBER", &ff

.vegetableEggplant

	equs "EGGPLANT", &ff

.vegetableCarrot

	equs "CARROT", &ff

.vegetableRadish

	equs "RADISH", &ff

.vegetableParsley

	equs "PARSLEY", &ff

.vegetableTomato

	equs "TOMATO", &ff

.vegetablePumpkin

	equs "PUMPKIN", &ff

.vegetableBambooShoot

	equs "BAMBOO SHOOT", &ff

.vegetableJapaneseRadish

	equs "JAPANESE RADISH", &ff

.vegetableMushroom

	equs "MUSHROOM", &ff

.vegetablePotato

	equs "POTATO", &ff

.vegetableOnion

	equs "ONION", &ff

.vegetableChineseCabbage

	equs "CHINESE CABBAGE", &ff

.vegetableTurnip

	equs "TURNIP", &ff

.vegetableGreenChilli

	equs "GREEN CHILLI", &ff

.vegetableCelery

	equs "CELERY", &ff

.vegetableSweetPotato

	equs "SWEET POTATO", &ff

.vegetableHorseradish

	equs "HORSERADISH", &ff



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; vegetable name screen address (centered) and name string address
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.vegetableAddr

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 8) * chrColumn) / 2) and &fff8)
	equw vegetableCucumber;, 8

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 8) * chrColumn) / 2) and &fff8)
	equw vegetableEggplant;, 8

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 6) * chrColumn) / 2) and &fff8)
	equw vegetableCarrot;, 6

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 6) * chrColumn) / 2) and &fff8)
	equw vegetableRadish;, 6

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 7) * chrColumn) / 2) and &fff8)
	equw vegetableParsley;, 7

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 6) * chrColumn) / 2) and &fff8)
	equw vegetableTomato;, 6

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 7) * chrColumn) / 2) and &fff8)
	equw vegetablePumpkin;, 7

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 12) * chrColumn) / 2) and &fff8)
	equw vegetableBambooShoot;, 12

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 15) * chrColumn) / 2) and &fff8)
	equw vegetableJapaneseRadish;, 15

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 8) * chrColumn) / 2) and &fff8)
	equw vegetableMushroom;, 8

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 6) * chrColumn) / 2) and &fff8)
	equw vegetablePotato;, 6

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 5) * chrColumn) / 2) and &fff8)
	equw vegetableOnion;, 5

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 15) * chrColumn) / 2) and &fff8)
	equw vegetableChineseCabbage;, 15

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 6) * chrColumn) / 2) and &fff8)
	equw vegetableTurnip;, 6

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 12) * chrColumn) / 2) and &fff8)
	equw vegetableGreenChilli;, 12

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 6) * chrColumn) / 2) and &fff8)
	equw vegetableCelery;, 6

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 12) * chrColumn) / 2) and &fff8)
	equw vegetableSweetPotato;, 12

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 11) * chrColumn) / 2) and &fff8)
	equw vegetableHorseradish;, 11



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; font ascii
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.fontBin
	skip 0					; show this address in listing

	incbin "img-font.bin"			; main font 6x6 pixels (1 bit per pixel)

.fontBinEnd
	skip 0					; show this address in listing



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; minifont used for vegetable bonus
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.miniFontBin
	skip 0					; show this address in listing
	
	incbin "img-font-vegetable.bin"		; mini vegetable font 4x8 pixels (4 bits per pixel)

.miniFontBinEnd
	skip 0					; show this address in listing



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; mapTile data					maze tiles used in map
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mapTileBin
	skip 0					; show this address in listing

	incbin "img-tiles.bin"			; maze map tiles 6x8 pixels (4 bits per pixel)

.mapTileBinEnd
	skip 0					; show this address in listing



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; objectTile data				object tiles used in map
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.objectTileBin
	skip 0					; show this address in listing

	incbin "img-objects.bin"		; maze object tiles hearts, letters, skulls 8x8 pixels (4 bits per pixel)

.objectTileBinEnd
	skip 0					; show this address in listing



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; extra tile data				extra tiles used on screen only (logo, flowers etc, not stored in background map)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.extraTileBin
	skip 0					; show this address in listing

	incbin "img-extra.bin"			; extra tiles 6x8 pixels (4 bits per pixel)

.extraTileBinEnd
	skip 0					; show this address in listing



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; tables to tiles
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mapTileAddrLo					; generate mapTiles address
	for n, mapTileBin, mapTileBinEnd -1, mapTileBytes
	equb lo(n)
	next

.mapTileAddrHi					; generate mapTiles address
	for n, mapTileBin, mapTileBinEnd -1, mapTileBytes
	equb hi(n)
	next

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.objectTileAddrLo				; generate objectTiles address

	for n, objectTileBin, objectTileBinEnd -1, objectTileBytes
	equb lo(n)
	next

.objectTileAddrHi				; generate objectTiles address

	for n, objectTileBin, objectTileBinEnd -1, objectTileBytes
	equb hi(n)
	next

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.extraTileAddrLo				; generate extraTiles address
	for n, extraTileBin, extraTileBinEnd -1, mapTileBytes
	equb lo(n)
	next

.extraTileAddrHi				; generate extraTiles address
	for n, extraTileBin, extraTileBinEnd -1, mapTileBytes
	equb hi(n)
	next



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; screen rows and columns
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.screenRowLo

	for n, 1, 25				; generate screen row low byte lookup table
	equb lo(screenAddr + n * chrRow)
	next

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.screenRowHi

	for n, 1, 25				; generate screen row high byte lookup table
	equb hi(screenAddr + n * chrRow)
	next

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.screenColumnLo

	for n, 0, 22				; generate column offset low byte lookup table
	equb lo(n * chrColumn)
	next

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.screenColumnHi

	for n, 0, 22				; generate column offset low byte lookup table
	equb hi(n * chrColumn)
	next



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; enemy timer tile XY position
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.timerXYTable

	for x,11,22				; enemy timer tile x,y positions clockwise from top middle
	equb x
	equb 0
	next

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	for y,1,22
	equb 22
	equb y
	next
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	for x,21,0,-1
	equb x
	equb 22
	next
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	for y,21,0,-1
	equb 0
	equb y
	next

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	for x,1,10
	equb x
	equb 0
	next



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; storage for the tileMap virtual screen (use as background buffer for background redraw behind sprites and game logic)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.tileMap

	skip 23 * 23				; storage for tile id's for the middle screen

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; tileMap row address table
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.tileMapRowsLo					; tileMap row low byte addresses

	for y,0,22
	equb lo(tileMap + y * 23)
	next

.tileMapRowsHi					; tileMap row high byte address

	for y,0,22
	equb hi(tileMap + y * 23)
	next



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; game sound effects and music tables
;-----------------------------------------------------------------------------------------------------------------------------------------------------

include "soundtables.asm"



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; sprite address tables
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.spriteBaseImg					; img index for base sprite of ladybug, the 8 enemies and the angel

	for n, 0, 9
	equb n * 15 + sprite10x10
	next

	;---------------------------------------------------------------------------------------------------------------------------------------------

.spriteImgAddrLo

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; generate lo address table for the 10x10 pixel vegetable and points sprites
	;---------------------------------------------------------------------------------------------------------------------------------------------

	for n, sprite10x10Bin, sprite10x10BinEnd - 1, sprite10x10Bytes
	equb lo(n)
	next

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; generate lo address table for the 10x14 pixel ladybug and enemies
	;---------------------------------------------------------------------------------------------------------------------------------------------

	for n, ladybugBin, spriteBinEnd - 1, spriteTileBytes
	equb lo(n)
	next

	;---------------------------------------------------------------------------------------------------------------------------------------------

.spriteImgAddrHi

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; generate hi address table for vegetable and point tiles
	;---------------------------------------------------------------------------------------------------------------------------------------------

	for n,  sprite10x10Bin, sprite10x10BinEnd - 1, sprite10x10Bytes
	equb hi(n)
	next

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; generate hi address table for ladybug and enemies
	;---------------------------------------------------------------------------------------------------------------------------------------------

	for n, ladybugBin, spriteBinEnd - 1, spriteTileBytes
	equb hi(n)
	next



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; maze data files loaded here by loader.asm
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mazeData					; 21 rows of 11 bytes (left half of maze only, right half generated by mirror flip)

.maze1	skip 21*11
.maze2	skip 21*11
.maze3	skip 21*11



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; joystickAnalogue				read analogue joystick values, convert to player input bits
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
; exit			A			destroyed
;			X			destroyed
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		joystickAnalogueSave	temporary storage for value read from analogue register
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.joystickBitSet

	equb keyBitDown				; y channel bit set mask
	equb 0
	equb 0
	equb keyBitUp

	equb keyBitRight			; x channel bit set mask
	equb 0
	equb 0
	equb keyBitLeft

	;---------------------------------------------------------------------------------------------------------------------------------------------

.joystickBitClear

	equb &ff eor (keyBitUp + keyBitDown)	; y channel bit clear mask
	equb &ff eor (keyBitUp + keyBitDown)
	equb &ff eor (keyBitUp + keyBitDown)
	equb &ff eor (keyBitUp + keyBitDown)

	equb &ff eor (keyBitLeft + keyBitRight)	; x channel bit clear mask
	equb &ff eor (keyBitLeft + keyBitRight)
	equb &ff eor (keyBitLeft + keyBitRight)
	equb &ff eor (keyBitLeft + keyBitRight)

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; analogue joystick function, read channel and convert to input bits
	;---------------------------------------------------------------------------------------------------------------------------------------------

.joystickAnalogue

	lda joystickEnable			; if analogue joystick enabled
	cmp #1
	bne joystickAnalogueExit

	;---------------------------------------------------------------------------------------------------------------------------------------------

.joystickAnalogueChannelRead

	lda dummy16				; read value from adc and save (addr setup by relocator.asm)
	sta joystickAnalogueSave
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.joystickAnalogueControlRead

	lda dummy16				; read current channel and flip bit to select other channel (addr setup by relocator)
	and #%00000001
	eor #%00000001

.joystickAnalogueControlWrite

	sta dummy16				; start new adc conversion (addr setup by relocator)

	lsr a					; put channel (0 or 1) into carry (note channel has been exored with 1 !)

	rol joystickAnalogueSave		; rotate analogue value so that
	rol joystickAnalogueSave		; bit 2 contains the channel
	rol joystickAnalogueSave		; bits 1,0 contain the msb's of the analogue value

	lda joystickAnalogueSave		; mask bits for index value
	and #%00000111
	tax

	lda joystickInput			; convert joystick direction and channel index into keyboard bit flags using tables
	and joystickBitClear, x
	ora joystickBitSet, x
	sta joystickInput

.joystickAnalogueExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
;
.programEnd					; end of main program
	skip 0					; show this address in listing
;
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print



