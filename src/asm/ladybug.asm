;-----------------------------------------------------------------------------------------------------------------------------------------------------
; ladybug main program
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " ladybug.asm"
	print "----------------------------------------------------"
	print


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; page0100 functions and stack
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org page0100



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; random					generate an 8 bit random number
;						random sequence length = 65535
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			8 bit random number from randomSeed + 1
;			X			preserved
;			Y			preserved
;			randomSeed		updated 16 bit seed value
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.random

	lda randomSeed + 1			; get hi 8 bits of seed
	lsr a					; shift it right to put bit 0 of hi into carry
	lda randomSeed				; get lo 8 bits of seed
	ror a					; rotate it right putting carry (bit 0 of hi) into bit 7 and bit 0 into carry
	eor randomSeed + 1			; eor with hi 8 bits
	sta randomSeed + 1			; store in hi 8 bits
	ror a					; rotate it right putting carry (bit 0 of lo) into bit 7 and bit 0 into carry (unused)
	eor randomSeed				; eor with lo 8 bits
	sta randomSeed				; store in lo 8 bits
	eor randomSeed + 1			; eor with hi 8 bits
	sta randomSeed + 1			; store in hi 8 bits

	rts					; return with random number in A



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawVegetableScore				draws vegetable score in the center box (if enabled)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
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

	lda #&00				; draw "00" and return
	jmp drawBcdMini

.drawVegetableScoreExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; stack variables				storage for variables preserved after clean reset
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org page0130

.cleanResetBank

	skip 1					; swr bank number for program and data
	
.cleanResetMachine

	skip 1					; machine type index

.cleanResetValidation

	skip 1					; validation of cleanResetBank and cleanResetMachine (checksum)

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; storage for keyboard/joystick mode and game mode (not preserved after reset but just in a handy place for menu.bas)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.joystickEnable

	skip 1					; control input mode 0 = no joystick, 1 = analogue joystick, 2 = userport joystick
						; note: keyboard is always active even when joystick is enabled
.gameMode

	skip 1					; storage for arcade/challenge game mode type bit 7 = 1 high score challenge mode, = 0 arcade mode


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; cleanReset (break key)			simulated power on reset while preserving the stack and high ram
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			jumps into high ram to select one of the following clean reset functions for each machine (see loader.asm)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.cleanReset

	sei					; disable irq interrupts
	
	lda cleanResetBank			; page in high ram bank
	sta bankSelect
	
	ldx cleanResetMachine			; get machine index

	jmp swrCleanReset			; run reset code in high ram to clear memory (see loader.asm) which then jumps into
						; one of the following functions to setup the jump into the original os
						; so that the beeb behaves normally



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; cleanResetMaster320				page in bank 15, jump into mos to continue
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			jumps into os rom to continue with machine setup
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
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			jumps into os rom to continue with machine setup
;-----------------------------------------------------------------------------------------------------------------------------------------------------

continueMaster350	= &fc76			; master 3.50 entry point

	;---------------------------------------------------------------------------------------------------------------------------------------------

.cleanResetMaster350

	lda #%01001001				; page in extra mos code at fc00
	sta acccon

	lda #&0f				; page in bank 15 extra mos code at 8000
	sta bankSelectCopy
	sta bankSelect
	
	jmp continueMaster350			; continue with master mos reset code



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; cleanResetCompact				page in bank 15, jump into mos to continue
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			jumps into os rom to continue with machine setup
;-----------------------------------------------------------------------------------------------------------------------------------------------------

continueCompact		= &8068			; master compact entry point

	;---------------------------------------------------------------------------------------------------------------------------------------------

.cleanResetCompact

	lda #&0f				; page in bank 15 extra mos code at 8000
	sta bankSelectCopy
	sta bankSelect
	
	jmp continueCompact			; continue with compact mos reset code



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; irq interrupt					handle vsync and timer1 interrupts
;						setting screenHalf upper/lower flag
;						bump counters
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit;			P			preserved
;			A			preserved
;			X			preserved
;			Y			preserved
;			screenHalf		set to 00 for upper half (raster lines 0-155) or ff for lower half (raster lines 156-311)
;			vsyncCounter		incremented every vsync (50Hz)
;			pauseCounter		decremented every 2 * vsync (25Hz)
;			idleCounter		decremented every 8 * vsync (6.25Hz)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

rasterTimer		= (312 / 2) * 64	; timer1 interupt raster (312 / 2) * 64uS (half way down the screen, line 156)

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check which interrupt occurred 
	;---------------------------------------------------------------------------------------------------------------------------------------------

.irqInterrupt

	lda via1Ifr				; if interrupt flag = vsync
	and #%00000010
	bne irqVsync				; then go do the upper vsync interrupt
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; interrupt (timer 1)
	;---------------------------------------------------------------------------------------------------------------------------------------------

.irqTimer					; else its a timer interrupt so do the lower interrupt

	lda #%01000000				; clear timer interrupt flag
	sta via1Ifr

	lda #&ff				; screenHalf = lower (raster now at line 156)
	sta screenHalf

	lda irqA				; restore A

	rti					; return to main program

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; interrupt (vsync)
	;---------------------------------------------------------------------------------------------------------------------------------------------

.irqVsync

	sta via1Ifr				; clear vsync interrupt flag

	lda #lo(rasterTimer)			; set timer 1 for lower interrupt
	sta via1T1CounterLo
	lda #hi(rasterTimer)
	sta via1T1CounterHi

	lda #&00				; screenHalf = upper (raster now at line 0)
	sta screenHalf

	inc vsyncCounter			; vsyncCounter += 1

	lda vsyncCounter			; if vsyncCounter & 7 == 0
	and #7
	bne irqVsyncPause

	dec idleCounter				; then idleCounter -= 1

.irqVsyncPause

	and #1					; if vsyncCounter & 1 == 0
	bne irqVsyncExit
	
	dec pauseCounter			; then pauseCounter -= 1

	;---------------------------------------------------------------------------------------------------------------------------------------------

 .irqVsyncExit

	lda irqA				; restore A
	
	rti					; return to main program



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; psgWrite 					write to sound chip
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	A			data to be written to 76489 psg
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		stack			1 byte used for delay
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
; waitIntUpper					wait for interrupt for upper raster lines 0-155
;						read analogue joystick (if enabled)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed (.joytickAnalogue function)
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.waitIntUpper					; repeat

	bit screenHalf				; until raster is in upper area lines 0-155
	bmi waitIntUpper
	
	jmp joystickAnalogue			; read analogue joystick (if enabled) and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; waitIntLower					wait for interrupt for lower raster lines 156-311
;						read analogue joystick (if enabled)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed (.joystickAnalog function)
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.waitIntLower					; repeat

	bit screenHalf				; until raster is in lower area lines 156-311
	bpl waitIntLower
	
	jmp joystickAnalogue			; read analogue joystick (if enabled) and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; updateObjectTimer				update object timer (25Hz)
;						handle change object mode and palette color when needed
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateObjectTimerPalette			; palette colors for letters and hearts

	equb palObject + palCyan
	equb palObject + palRed
	equb palObject + palYellow

	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateObjectTimerDuration			; duration of the colors for letters and hearts

	equb objectModeCyanTime
	equb objectModeRedTime
	equb objectModeYellowTime

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; decrement timer and if = 0 then change object mode/object color and reset timer for new mode
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateObjectTimer

	lda vsyncCounter			; if vsync counter and 1 = 0 (25Hz)
	and #1
	bne updateObjectTimerExit
	
	dec objectModeTimer			; then objectModeTimer -= 1
	bne updateObjectTimerExit		; if objectModeTimer = 0

	stx updateObjectTimerSaveX		; save register
	
	ldx objectMode				; get current objectMode

	inx					; add 1
	cpx #objectModeYellow + 1		; if objectMode > objectModeYellow then objectMode = objectModeCyan
	bne updateObjectTimerColor
	ldx #objectModeCyan
	
.updateObjectTimerColor

	stx objectMode				; update new objectMode
	
	lda updateObjectTimerPalette, x		; update new object color palette
	sta ulaPalette
	
	lda updateObjectTimerDuration, x	; set objectModeTimer for new mode duration
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

	; 4 unused bytes here



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; set irq1 interrupt vector
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org irqVector

.irq1vector
	equw irqInterrupt			; set bbc os irq1v interrupt vector to our irqInterrupt function



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; inputScan					check up down left right start and esc keys
;						combine with joystickInput so either keyboard or joystick can control the game
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;			playerInput		bit 0=start 1=left 2=down 3=up 4=right 5=esc
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.inputScan

	lda #%01111111				; set port A bit 7 as input (from keyboard output)
	sta via1PortDdrA
	
	lda #sbKeyboard + sbLow			; keyboard output -enable low (connect keyboard output to port A bit 7)
	sta via1PortB
	
	lda #0					; clear player input flags
	sta playerInput

	lda #keyEsc				; read esc key into playerInput bits
	jsr readKey

	ldx #3					; read 4 user defined movement keys
	
.inputScanLoop					; repeat

	inx					; change order from 3 2 1 0 to 0 3 2 1 so that key input order matches joystick order
	txa
	and #3
	tay
	dex

	lda optionKeys, y			; read user defined key matrix code from optionKeys

	jsr readKey				; read key into playerInput bits
	
	dex					; until all 4 keys are read in
	bpl inputScanLoop
	
	lda #keyReturn				; read start key (return) into playerInput bits
	jsr readKey

	lda #sbKeyboard + sbHigh		; slow bus keyboard -enable high (disconnect keyboard output)
	sta via1PortB
	
	lda #%11111111				; set port A all bits output
	sta via1PortDdrA

	jmp swrJoystickControl			; combine keyboard input with joystick input (if enabled, see loader.asm)



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawObjectScore				draws objectScoreImg at objectScoreX, objectScoreY (if enabled)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawObjectScore

	lda objectScoreX			; set position of object score
	sta drawSpriteX
	lda objectScoreY
	sta drawSpriteY

	lda objectScoreImg			; if objectScoreImg != 0
	beq drawObjectScoreExit

	jmp drawSprite10x10			; then draw object score image
	
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
;						so that our break key intercept can run the clean reset code
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org pagefx200

.osFx200
	
	equb 0



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; chooseLetters					choose 3 random letters, make sure there are no duplicates
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;			levelLetters + 0	random letter tile id
;			levelLetters + 1	random letter tile id
;			levelLetters + 2	random letter tile id
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.chooseLetters

	jsr chooseLetterRandom			; pick 1st random letter
	sta levelLetters + 0			; and store it in 1st
	
.chooseLetters2nd

	jsr chooseLetterRandom			; pick 2nd random letter

	cmp levelLetters + 0			; if its the same as 1st then try again
	beq chooseLetters2nd
		
	sta levelLetters + 1			; else store it in 2nd
	
.chooseLetters3rd

	jsr chooseLetterRandom			; pick 3rd random letter
	
	cmp levelLetters + 0			; if its the same as 1st then try again
	beq chooseLetters3rd
		
	cmp levelLetters + 1			; if its the same as 2nd then try again
	beq chooseLetters3rd

	sta levelLetters + 2			; else store it in 3rd

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; chooseLetterRandom				pick a random number (0-9) and return with one of the 10 letter tile id's in A
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			random letter tile id
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.chooseLetterRandom

	jsr random				; pick a random number 0-15
	and #15
	cmp #10					; if its >= 10 then pick another
	bcs chooseLetterRandom
			 			; (carry is clear here so no need for clc)
	adc #objectTileIndex			; add object tile index for letters

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; move direction table squeezed in here
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mapDir	equb 1, 47, 23, 25, 24	; tileMap offset from top left corner for up, down, left, right, center



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
;						handle setting of the enemy release and zero crossing flags
;						also the enemy release warning sound
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed (call to changeTimerTile)
;			Y			destroyed (call to changeTimerTile)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateEnemyTimer

	lda pauseEnemy				; if enemy movement is paused then return
	bne updateEnemyTimerExit

	dec enemyTimerSpeedCounter		; bump enemy timer speed counter
	bne updateEnemyTimerExit		; exit if not zero

	lda enemyTimerSpeed			; reset the enemy timer speed counter
	sta enemyTimerSpeedCounter

	jsr changeTimerTile			; change enemy timer tile color

	inc enemyTimer				; bump enemy timer position

	lda enemyTimer				; if enemyTimer >= enemyTimerMax + 1
	cmp #enemyTimerMax + 1
	bcc updateEnemyTimerSound

	lda #0					; then reset enemy timer back to 0
	sta enemyTimer

	lda enemyReleaseEnable			; if enemy release is enabled
	beq updateEnemyTimerSound
	sta enemyTimerZero			; then set enemyTimerZero flag

.updateEnemyTimerSound

	jsr playSoundTimer			; play timer sound at selected volume

	lda enemyTimer				; if enemyTimer = top left
	cmp #enemyTimerTopLeft
	bne updateEnemyTimerExit
	
	lda enemiesActive			; if all enemies are not yet active
	cmp #spritesTotal - 1
	beq updateEnemyTimerExit

	lda #sfxEnemyWarning			; then play enemy release warning and set enemy release flag
	jsr playSound

	lda #&ff
	sta enemyReleaseEnable

.updateEnemyTimerExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; initPlayfieldMiddle				copy map maze tiles to tileMap, count the number of dots and store in levelEdibles
;						place hearts in the map replacing dots (hearts are edible so no change to levelEdibles)
;						place letters in the map replacing dots (letters are edible so no change to levelEdibles)
;						place skulls in map replacing dots
;						(skulls are not edible so decrease levelEdibles by the number of skulls (in .placeTileMapSkulls))
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.initPlayfieldMiddleTilesLeft			; left side tile translation table (left/right order normal)

	equb mapTileBlank
	equb mapTileDot
	equb mapTileTurnstileCV + wallSolid
	equb mapTileTurnstileCH + wallSolid
	equb mapTileTurnstileD + wallTurnstile
	equb mapTileTurnstileU + wallTurnstile
	equb mapTileTurnstileR + wallTurnstile	; right
	equb mapTileTurnstileL + wallTurnstile	; left
	equb mapTileTopLeft + wallSolid		; left
	equb mapTileTopRight + wallSolid	; right
	equb mapTileBottomLeft + wallSolid	; left
	equb mapTileBottomRight + wallSolid	; right
	equb mapTileVertical + wallSolid
	equb mapTileHorizontal + wallSolid
	equb mapTileVerticalD + wallSolid
	equb mapTileVerticalU + wallSolid
	equb mapTileHorizontalR + wallSolid	; right
	equb mapTileHorizontalL + wallSolid	; left

	;---------------------------------------------------------------------------------------------------------------------------------------------

.initPlayfieldMiddleTilesRight			; right side tile translation table (left/right order switched) 

	equb mapTileBlank
	equb mapTileDot
	equb mapTileTurnstileCV + wallSolid
	equb mapTileTurnstileCH + wallSolid
	equb mapTileTurnstileD + wallTurnstile
	equb mapTileTurnstileU + wallTurnstile
	equb mapTileTurnstileL + wallTurnstile	; left
	equb mapTileTurnstileR + wallTurnstile	; right
	equb mapTileTopRight + wallSolid	; right
	equb mapTileTopLeft + wallSolid		; left
	equb mapTileBottomRight + wallSolid	; right
	equb mapTileBottomLeft + wallSolid	; left
	equb mapTileVertical + wallSolid
	equb mapTileHorizontal + wallSolid
	equb mapTileVerticalD + wallSolid
	equb mapTileVerticalU + wallSolid
	equb mapTileHorizontalL + wallSolid	; left
	equb mapTileHorizontalR + wallSolid	; right

	;---------------------------------------------------------------------------------------------------------------------------------------------

.initPlayfieldMiddleMazeTable

	equw map1, map2, map3			; address locations of the three maze maps

	;---------------------------------------------------------------------------------------------------------------------------------------------

.initPlayfieldMiddle

	lda #0					; initialize edibles to 0
	sta levelEdibles

	lda mazeMap				; get maze map number (0 to 5)
	and #%0110				; remove bit 0 so that 0,1,2,3,4,5 becomes 0,0,2,2,4,4 (index into the 3 word maze list above)

	tay					; set start address of maze data using masked map number as index
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

.initPlayfieldMiddleLoop			; repeat

	ldy #0

	lda initPlayfieldMiddleWriteLeft + 1	; calculate end of row address
	clc
	adc #20
	sta initPlayfieldMiddleWriteRight + 1
	lda initPlayfieldMiddleWriteLeft + 2
	adc #0
	sta initPlayfieldMiddleWriteRight + 2

	;---------------------------------------------------------------------------------------------------------------------------------------------

.initPlayfieldMiddleRead			; repeat

	ldx dummy16, y				; get byte from maze (address previously setup)

	lda initPlayfieldMiddleTilesLeft, x	; convert to tile left side (normal)

	;---------------------------------------------------------------------------------------------------------------------------------------------

.initPlayfieldMiddleWriteLeft

	sta dummy16, y				; store tile in map (address previously setup)
	
	cmp #mapTileDot				; if its a dot
	bne initPlayfieldMiddleRight

	inc levelEdibles			; then increment levelEdibles (count the dots)

	cpy #10					; if not middle column (we dont want to count the middle column dot twice)
	beq initPlayfieldMiddleRight
	
	inc levelEdibles			; then increment levelEdibles again (we count the left side twice to include mirrored right side)

	;---------------------------------------------------------------------------------------------------------------------------------------------

.initPlayfieldMiddleRight

	lda initPlayfieldMiddleTilesRight, x	; convert to tile id for right side (mirrored)

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

	iny					; until row transferred
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

	dec initPlayfieldMiddleRows		; until all rows done
	bne initPlayfieldMiddleLoop

	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr placeTileMapHearts			; place 3 hearts at random positions in the tileMap (replacing dots)

	jsr placeTileMapLetters			; place 3 random letters at random positions in the tileMap (replacing dots)

	jmp placeTileMapSkulls			; place the correct number of skulls at random positions in the tileMap (replacing dots) and exit



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; initTimerTiles				fill the outer edges of tileMap with timer tiles and initialize enemyTimer to 0
;						clear enemy release flag and timer crossed zero flag
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
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

	ldx #21					; 21 columns
	
.initTimerTilesHorizontal			; repeat

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

	ldx #21					; 21 rows

.initTimerTilesVertical				; repeat

	lda #mapTileTimerLeft + wallSolid	; store left timer tile in column 0 of tileMap
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

	dex					; until rows 1-21 done
	bne initTimerTilesVertical
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #0
	sta enemyTimer				; set enemy timer position to 0
	sta enemyReleaseEnable			; clear enemy release enable
	sta enemyTimerZero			; clear enemy timer crossed zero flag

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; tileMapfindDot				find a random tileMap location that contains a dot and isnt near a turnstile
;						if location hasnt been found within 0.16 seconds then timeout (bad maze design)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exits			A			destroyed
;			X			preserved
;			Y			destroyed
;			carry			set if location found, clear if not found (timed out)
;			tileMapAddr		contains the address of the dot in the tileMap (if found)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.tileMapFindDot

	lda #pause * 0.16			; set timeout for 0.16 seconds
	sta pauseCounter

	;---------------------------------------------------------------------------------------------------------------------------------------------

.tileMapFindDotY

	lda pauseCounter			; if timed out then return with failed status
	beq tileMapFindDotFailed

	jsr random				; get random value 0-255, mask to become 0-28 step 4
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

	jsr random				; get random value 0-255, mask it to become 0-28 step 4
	and #%00011100

	cmp #21					; if its higher than 20 then try again
	bcs tileMapFindDotX
						; (carry is clear so no need for clc)
	adc tileMapAddr				; add to tileMapAddr so that it points to the top left of the 3x3 tile square to investigate
	sta tileMapAddr
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

	sec					; return with carry set (found) and tile address in tileMapAddr
	rts

	;---------------------------------------------------------------------------------------------------------------------------------------------

.tileMapFindDotFailed

	clc					; return with carry clear (timed out, failed)
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; placeTileMapHearts				place 3 hearts at random locations in the map
;						exit if location not found within 0.16 seconds
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.placeTileMapHearts

	ldx #3					; 3 hearts

.placeTileMapHeartsLoop				; repeat

	jsr tileMapFindDot			; pick a random tileMap location containing a dot that isnt near a turnstile

	bcc placeTileMapHeartsExit		; if location not found (bad maze design) then return

	lda #mapTileHeart			; else replace dot with a heart
	ldy #0
	sta (tileMapAddr), y
	
	dex					; until all hearts placed
	bne placeTileMapHeartsLoop
	
.placeTileMapHeartsExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; placeTileMapLetters				place 3 letters at random locations in the map
;						exit if location not found within 0.16 seconds
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.placeTileMapLetters

	ldx #3					; 3 letters

.placeTileMapLettersLoop			; repeat

	jsr tileMapFindDot			; pick a random tileMap location containing a dot that isnt near a turnstile

	bcc placeTileMapLettersExit		; if location not found (bad maze design) then return

	lda levelLetters - 1, x			; replace dot with a letter from the levelLetters table
	ldy #0					; use levelLetters - 1 address because x index is 3,2,1 not 2,1,0
	sta (tileMapAddr), y
	
	dex					; until all letters placed
	bne placeTileMapLettersLoop

.placeTileMapLettersExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; placeTileMapSkulls				place skulls at random locations in the map, decrement edibles for each skull placed
;						exit if location not found within 0.16 seconds
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.placeTileMapSkulls

	ldx levelSkulls				; get number of skulls for level

.placeTileMapSkullsLoop				; repeat

	jsr tileMapFindDot			; pick a random tileMap location containing a dot that isnt near a turnstile

	bcc placeTileMapSkullsExit		; if location not found (bad maze design) then return

	lda #mapTileSkull			; replace dot with a skull
	ldy #0
	sta (tileMapAddr), y
	
	dec levelEdibles			; decrement number of edible objects

	dex					; until all skulls placed
	bne placeTileMapSkullsLoop
	
.placeTileMapSkullsExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; initSprites					set all sprites as blanked and not moving
;						set all sprites as erased
;						set no active enemies
;						disable ladybug movement animation
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------


.initSprites

	ldx #spritesTotal - 1			; total number of sprites to initialise

.initSpritesLoop				; repeat

	lda #spriteBlanking + moveStop		; sprite blanked and not moving
	sta spritesDir, x

	lda #&ff				; sprite marked as already erased
	sta spritesErased, x

	dex
	bpl initSpritesLoop			; until all sprites initialised

	lda #0					; no active enemies yet
	sta enemiesActive
	
	sta animateLadybugActive		; disable ladybug movement animation

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawScore					draw a single score digit allowing for leading zero blanking
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;			drawScoreAddr		points to next tile position on screen
;			drawScoreIndex		points to the next score digit
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		drawDigitsEnable	flag for leading zero blanking
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawScore

	lda #score				; set address to score location
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
	lsr a					; carry now = 1 (10's digit) or 0 = (units digit)
	tax
	
.drawScoreDigitRead

	lda dummy8, x				; get byte from score  (address previously setup)
	
	bcc drawScoreDigitPrint			; if carry is set shift bits to get the 10's bcd digit
	
	lsr a
	lsr a
	lsr a
	lsr a

.drawScoreDigitPrint

	and #%00001111				; use lowest 4 bits of bcd digit

	beq drawScoreDigitCheckBlanking		; if digit != 0 then disable leading zero blanking
	dec drawScoreBlanking

.drawScoreDigitCheckBlanking

	bit drawScoreBlanking			; if leading zero blanking enabled
	bmi drawScoreNext

	lda #extraTileBlank			; then print a blank tile instead of digit 0

.drawScoreNext

	jsr drawExtraTile			; print digit or blank tile
	
	lda drawMapTileAddr			; save screen address for next digit
	sta drawScoreAddr
	lda drawMapTileAddr + 1
	sta drawScoreAddr + 1
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawHighScore					draw highScore using 6 calls to drawscore
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawHighScore

	lda #highScore				; set address to highScore location
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

.drawHighScoreLoop				; repeat

	jsr drawScoreDigit			; print a digit of high score
	
	dec drawScoreIndex			; move to next digit
	bpl drawHighScoreLoop			; until all 6 digits done
	
	lda #0					; draw final 0 digit and return
	jmp drawExtraTile



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; eraseSprite					erase the sprite block of 10x14 pixels on screen
;						redraw tile at the tail end of sprite from tile map (redraw background)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	X			sprite number (index into spritesErase table containing x, y, dir information)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
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
	
	lda spritesEraseDir, x			; if sprite was moving left
	and #3
	cmp #moveLeft
	bne eraseSpriteCheckUp

	ldy #1					; then check the next tile to the right
	lda (tileMapAddr), y

	bmi eraseSpriteExit			; if its a maze tile then return

	cmp #objectTileIndex			; if its not an object then then return
	bcc eraseSpriteExit

	jsr drawMapTile				; else draw the object tile to prevent cropping a column on objects caused by the previous tile draw

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

	lda spritesEraseY, x			; and if sprite isnt on the bottom row
	cmp #20 * 8
	bcs eraseSpriteExit

	ldy #23					; then check tile below
	lda (tileMapAddr), y

	bmi eraseSpriteExit			; if its a maze tile then return

	cmp #objectTileIndex			; if its an object tile
	bcc eraseSpriteExit

	lda drawMapTileAddr			; adjust screen address to 1 tile below
	adc #lo(chrRow - chrColumn - 1)		; (carry is set here so use value - 1)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrRow - chrColumn - 1)
	sta drawMapTileAddr + 1

	lda (tileMapAddr), y			; draw tile to prevent cropping the top of the object tile
	jsr drawMapTile

	jmp eraseSpriteExit			; restore registers and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; eraseBlock					erase a 10x14 block of pixels
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	spriteX			sprite coordinates for erasure
;			spriteY
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
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

	lda #pixelsBlack			; fill byte = black

.eraseBlockWrite				; repeat

	sta dummy16				; write to screen (address previously setup)

	dec eraseBlockBytes			; if all bytes written then return
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

	lda #pixelsBlack			; reload A with black

.eraseBlockContinue

	dey					; until column done
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
; entry parameters	A			tile img to be drawn
;			drawMapTileAddr		current screen location for tile
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			preserved
;			X			preserved
;			Y			preserved
;			drawMapTileAddr		points to next tile position on screen
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
; entry parameters	A			tile img to be drawn
;			drawMapTileAddr		current screen location for tile
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;			drawMapTileAddr		points to next tile position on screen
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawMapTile4Pixel

	sta drawMapTileSaveA			; preserve registers
	sty drawMapTileSaveY

	and #%00111111				; remove wall flags bits 7,6

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
; entry parameters	A			tile img to be drawn (bits 7,6 not used)
;			drawMapTileAddr		current screen location for tile
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;			drawMapTileAddr		points to next tile position on screen
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawMapTile

	sta drawMapTileSaveA			; preserve registers
	sty drawMapTileSaveY

	and #%00111111				; remove wall flag bits 7,6

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
	
.drawMapTileRead				; repeat

	lda dummy16, y				; read byte from tile (address previously setup)
	
.drawMapTileWrite

	sta dummy16, y				; write byte to screen (address previously setup)
	
.drawMapTileContinue

	dey					; until all bytes copied
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
; .drawMapTileSpace				draw map tile and advance screen address an extra tile to leave a space
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawMapTileSpace

	jsr drawMapTile				; draw the tile

	lda #chrColumn				; leave space
	jmp drawMapTileAddrAdvance



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawObjectTile				draw 8 pixel wide object tile to screen, move to next tile position
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	A			tile img to be drawn
;			drawObjectTileAddr	current screen location for tile
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			destroyed
;			drawObjectTileAddr	points to next tile position on screen
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawObjectTile

	sec					; subtract starting index so that tile is indexed from 0
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
; entry parameters	A			bcd value to display
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			drawChrAddr		points to next chr position on screen
;			A			destroyed
;			X			preserved
;			Y			preserved
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

	; continue into drawChr with low nybble



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawChr					draw chr to screen, move to next chr position ready for next
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	A			chr to be drawn
;			drawChrAddr		current screen location for chr
;			drawChrColor		bit mask for pixel colors
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;			drawChrAddr		points to next chr position on screen
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		drawChrFontData		data read from font to be converted to pixels
;-----------------------------------------------------------------------------------------------------------------------------------------------------

; chrs are 6 x 6 pixels stored in bit pairs from top to bottom, left to right order using 5 bytes per character, the last 4 bits are unused
;
; [byte,bit]
;
; [0,7][0,6] [1,3][1,2] [3,7][3,6]
; [0,5][0,4] [1,1][1,0] [3,5][3,4]
; [0,3][0,2] [2,7][2,6] [3,3][3,2]
; [0,1][0,0] [2,5][2,4] [3,1][3,0]
; [1,7][1,6] [2,3][2,2] [4,7][4,6]
; [1,5][1,4] [2,1][1,0] [4,5][4,4]

;-----------------------------------------------------------------------------------------------------------------------------------------------------

pixelLeft		= &aa			; bit mask for left pixel
pixelRight		= &55			; bit mask for right pixel

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawChr

	stx drawChrSaveX			; save registers
	sty drawChrSaveY

	ldy drawChrColor			; store chr color
	sty drawChrWriteColor + 1

	ldx #0					; zero bit counter
	
	stx drawChrFontRead + 2			; zero high byte of chr value

	sec					; set character code origin to 0 (ascii 32-95 becomes 0-63)
	sbc #' '

	sta drawChrFontRead + 1			; multiply A by 5 (5 bytes per character)
	asl a
	asl a
	adc drawChrFontRead + 1			; (carry is clear here so no need for clc)
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

	txa					; every 4 bit pairs get a byte from chr table
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
; alias to draw address
;-----------------------------------------------------------------------------------------------------------------------------------------------------

drawChrAddr = drawChrWriteScreen + 1		; screen address to write chr






;*****************************************************************************************************************************************************
;
; main						main entry point to program
;
;*****************************************************************************************************************************************************

.main

	cli					; enable interrupts

	jsr swrInitScreen			; full screen erase, setup palette colors (see loader.asm)
	
	lda #%11110100				; put ula into 16 color mode
	sta ulaMode

	lda #pause * 0.5			; set pause time to 0.5 seconds
	sta pauseCounter
	
.mainPauseLoop					; repeat

	lda pauseCounter			; until pause time has expired
	bne mainPauseLoop

	lda #1					; enable display
	sta crtcAddr
	lda #screenWidth
	sta crtcData

	jsr drawPlayfieldUpper			; display the upper playfield bonus letters and multipliers

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; display the main screen with player options, wait for start to be pressed or idle timeout
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.gameIntroScreen

	jsr updateHighScoreFirstPlace		; copy first place high score from table to lower panel and display it

	jsr mainMenu				; display the main menu screen, handle game setting adjustments and return when start game is selected



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; setup a new game, Level 1 for regular game or random level for demo mode
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.gameNew

	jsr swrGameLevel			; choose the game level (see loader.asm)

	jsr initLevelSettings			; setup skulls, letters, enemy settings etc for current level

	jsr drawPlayfieldUpperBonus		; update the upper bonus top panel

	jsr drawPlayfieldLower			; draw playfield lower section (info panel)

	jsr instructions			; display the game instructions and wait for return or esc to be pressed (if demo mode it returns carry set)
	bcc gameIntroScreen			; if esc was pressed then return to intro

	lda #sfxTwinkle				; play twinkle sound effect for first level
	jsr playSound

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; start current game level, setup level settings and initialize the tileMap with mazeMap layout
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.gameLevelStart

	jsr initLevelSettings			; setup skulls, letters, enemy settings etc for current level

	jsr drawPlayfieldUpperBonus		; update the upper bonus top panel (multipliers are cleared for new round)

	jsr drawPlayfieldLower			; draw playfield lower section (info panel)

	jsr drawLevelIntro			; draw the level intro screen showing information about current level

	jsr initPlayfieldMiddle			; copy current maze map to the background tileMap and count number of edible items (dots/hearts/letters)

	ldx #objectModeRed			; red object mode 
	jsr updateObjectTimerColor

	lda levelEdibles			; if there are no edible objects (bad maze design)
	bne gameLevelContinue
	jmp gameOver				; then end the game

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; continue current level			for start of level and to continue level after ladybug dies
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.gameLevelContinue

	jsr drawPlayfieldLowerLives		; update the lives value on screen

	jsr initSprites				; disable all sprites and mark all erased

	lda lives				; if lives = 0 then its game over
	bne gameLevelContinueDrawMiddle
	jmp gameOver

.gameLevelContinueDrawMiddle

	jsr initTimerTiles			; fill tileMap edges with timer tiles and initialize timer to starting point center top
						; and clear enemyReleaseEnable and enemyTimerZero flags 
	
	jsr drawPlayfieldMiddle			; draw playfield middle section from tileMap

	jsr enemySpawn				; spawn 1st enemy in center box ready to be active on release timer

	jsr ladybugSpawn			; spawn ladybug

	lda #0

	sta bonusItemActive			; disable center bonus item

	sta vegetableScoreActive		; disable center vegetable score display

	sta objectScoreImg			; disable object score display

	sta playerInput				; clear any previous pending player control inputs

	sta demoDir				; set demo direction to up to match the direction of ladybug at the entry position

	sta pauseLadybug			; unpause ladybug
	
	lda #escTime				; reset esc key timer
	sta escCounter

	sta enemySpeedCounter			; reset enemy speed fraction counter
	sta enemyMoveCounter			; reset enemy move counter used by enemy release delay

;*****************************************************************************************************************************************************
;
; game main loop
;
;*****************************************************************************************************************************************************

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; gameLoopWaitIntUpper			wait for upper interrupt (raster lines 0-155) then process game functions
	;---------------------------------------------------------------------------------------------------------------------------------------------

.gameLoopWaitIntUpper

	jsr waitIntUpper			; wait for vsync interrupt and read analogue joystick (if enabled)

	jsr ladybugEntryAnimation		; ladybug entry movement animation (if enabled)

	jsr checkBonus				; check if special, extra or diamond bonus screens are required
	bcs gameLevelStart			; if bonus was awarded then start a new level (level was advanced by checkBonus)

	jsr checkLevelEnd			; check if current level has ended
	bcs gameLevelStart			; if level has ended then start new level (level was advanced by checkLevelEnd)

	jsr redrawSprites			; erase and redraw sprites (below the center line split lines 156-311)

	jsr ladybugDeathAnimation		; draw ladybug death movement animation (if enabled)
	bcs gameLevelContinue			; continue current level if ladybug death movement animation has just completed

	jsr drawVegetableScore			; draw the vegetable bonus score in center (if enabled)

	jsr drawObjectScore			; draw object score (if enabled)

	jsr drawTurnstile			; draw new turnstile position (if enabled)

	jsr updateSkullColor			; update the skull palette color

	jsr updateBonusColor			; update the bonus letters and object points palette colors

	jsr checkPauseGame			; if game is paused then skip sprite movement and timer stuff
	bcs gameLoopWaitIntLower

	jsr updateLadybug			; update ladybug direction, handle turnstile and object detection

	jsr moveSprites				; move all sprites, handle enemy collision with skulls and ladybug

	jsr updateObjectTimer			; update object timer, object mode and palette

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; gameLoopWaitIntLower			wait for lower interrupt (raster lines 156-311) then process game functions
	;---------------------------------------------------------------------------------------------------------------------------------------------

.gameLoopWaitIntLower

	jsr waitIntLower			; wait for timer1 interrupt and read analogue joystick (if enabled)
	
	jsr processSound			; process sound effects and music

	jsr drawBonusItemCenter			; draw the vegetable/diamond in the center bug box (if enabled)

	jsr redrawSprites			; erase and redraw sprites (above the center line split lines 0-155)
	
	jsr ladybugDeathAnimation		; draw ladybug death movement animation (if enabled)
	bcs gameLevelContinue			; continue current level if ladbug death animation has just completed

	jsr drawVegetableScore			; draw the vegetable bonus score in center (if enabled)

	jsr drawObjectScore			; draw object score (if enabled)

	jsr updateAnimationFrame		; update the sprite animation frame number

	jsr drawScore				; draw a single score digit, move to next score digit ready for next time around

	jsr inputScan				; read keyboard input and joystick (if enabled)

	jsr checkEsc				; if we need to quit the game (esc held)
	bcc gameLoopCheckPause
	jmp gameIntroScreen			; then jump back to the game intro screen

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; if game is paused (return was pressed) then skip enemy timer, enemy release, enemy/ladybug pause timers
	;---------------------------------------------------------------------------------------------------------------------------------------------

.gameLoopCheckPause

	jsr checkPauseGame			; if game is paused then skip enemy timer, enemy release, enemy/ladybug pause timers
	bcs gameLoopWaitIntUpper

	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr updateEnemyTimer			; update the enemy timer and draw the new timer tile when needed

	jsr enemyRelease			; release an enemy (if enabled)

	jsr updatePauseTimers			; update ladybug and enemy pause timers (also handles erasure of object score and bonus item score)

	jmp gameLoopWaitIntUpper		; loop back to game main loop



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; checkEsc					if esc key not pressed then reset esc counter and return false
;						if esc key pressed then decrement esc counter
;						if counter = 0 then return true else return false
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;			C			set if esc key held long enough for timer to reach 0
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.checkEsc

	lda playerInput				; if esc key not pressed
	cmp #keyBitEsc
	beq checkEscPressed
	
	lda #escTime				; then reset escCounter
	sta escCounter
	
.checkEscReturnFalse

	clc					; return false
	rts
	
.checkEscPressed

	lda demoMode				; if demo mode is enabled then skip waiting for escCounter to timeout
	bne checkEscReturnTrue

	dec escCounter				; else decrement escCounter
	bne checkEscReturnFalse			; if escCounter != 0 then return false
	
.checkEscReturnTrue

	jsr playSoundSilence			; esc time has passed so terminate any current sound effects or music

	lda #sfxSkull				; play skull sound
	jsr playSound

	sec					; return true
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; gameOver					if the game was a demo then just jmp back to the game intro
;						for a regular game clear the screen and display game over message
;						then check if score needs to be registered in the high score table
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.gameOver

	lda demoMode				; if game was a demo then just jump to game intro (dont show game over screen or check high score)
	bne gameOverExit

	lda #gameOverTime			; set display time
	sta pauseCounter

	jsr initMiddle				; initialize and draw empty playfield with timer, initialize all sprites as blanked and erased

	jsr drawString				; draw game over message in red
	equw screenAddr + 2 + 7 * chrColumn + 12 * chrRow
	equs colorRed, "GAME OVER", &ff
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; update bonus colors while waiting for the pauseCounter to timeout
	;---------------------------------------------------------------------------------------------------------------------------------------------

.gameOverLoop					; repeat

	jsr waitIntUpper			; wait for vsync interrupt and read analogue joystick (if enabled)
	jsr waitIntLower			; wait for timer1 interrupt and read analogue joystick (if enabled)

	jsr updateBonusColor			; update the bonus letters palette colors

	lda pauseCounter			; until pause time expires
	bne gameOverLoop

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check if highscore needs to be entered
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr checkHighScore			; check if highScore was beaten (handles score position and the high score entry)

.gameOverExit

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

.checkHighScoreLoop				; repeat

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
	
	lda #14 - 1				; else move to the next score in the table (14 bytes to next highscore but carry is set so use 13 instead)
	adc highScorePtr			; (cannot cross page boundry so no need to adjust high byte address)
	sta highScorePtr
	
	dex					; until end of table
	bpl checkHighScoreLoop

	rts					; score didnt qualify for the high score table so just return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; shift high score table down if needed to make room for entry
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.checkHighScoreEntry

	cpx #0					; if the entry is last in the table then no need to shift scores, go directly to registration
	beq checkHighScoreRegister

	ldy #lo(highScoreTableEnd - 15)		; else we need to shift high score data down to make room for high score entry

.checkHighScoreShift

	lda hi(highScoreTable) * 256, y		; shift memory
	sta hi(highScoreTable) * 256 + 14, y
	
	dey
	cpy highScorePtr
	bne checkHighScoreShift

	lda hi(highScoreTable) * 256, y		; shift last byte
	sta hi(highScoreTable) * 256 + 14, y

	; continue down to registration



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; enter name into table
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.checkHighScoreRegister

	jsr nameReg				; get name registration from player

	jsr generateValidation			; update the validation code

	lda #sfxMusicLetters			; play high score music
	jsr playSound
	
	jsr updateHighScoreFirstPlace		; update lower playfield with first place high score

	jmp drawScoreTable			; draw the high score page and return
	
	
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; initMiddle					initialize all sprites as blanked and erased
;						clear tilemap
;						fill tilemap edges with with timer tiles
;						draw tilemap
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.initMiddle

	jsr initSprites				; initialize all sprites as blanked and erased

	jsr clearTileMap			; fill tilemap with blank tile
	
	jsr initTimerTiles			; fill edges with timer tiles
	
	jmp drawPlayfieldMiddle			; draw the middle playfield and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; ladybugEntryAnimation				if ladybug entry animation is disabled then exit else
;						if ladybug is at the entry position
;						{
;						play the entry music
;						pause enemy timer
;						}
;						start ladybug entry animation (if not already started)
;						if ladybug has reached starting position then disable ladybug entry animation
;	
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			destroyed (call to animateLadybug)
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

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; checkLevelEnd					check levelEnd flag and levelEdibles, trigger an end if needed (set the carry flag)
;						handle the level advance and if shield > 0 then reduce by 1
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.checkLevelEnd

	lda levelEndActive			; if level has ended then advance level and exit with true status
	bne checkLevelEndTrue

	lda levelEdibles			; if there are still edible objects then return with false status
	bne checkLevelEndFalse

	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #endLevelTime			; pause ladybug and enemies
	sta pauseLadybug
	sta pauseEnemy

	lda soundTimers + 0			; if sound effect not playing on channel 0 (music)
	bne checkLevelEndFalse

	lda soundTimers + 3			; and if sound effect not playing on channel 3 (object, skull)
	bne checkLevelEndFalse

	lda soundTimers + 5			; and if sound effect not playing on channel 5 (munch)
	bne checkLevelEndFalse

	lda #&ff				; flag level as ended
	sta levelEndActive

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

	lda shield				; if shield != 0
	beq checkLevelEndExit

	sed					; then bcd mode

	sec					; reduce shield by 1
	sbc #1
	sta shield

	cld					; binary mode

.checkLevelEndExit

	sec					; flag end level as true and return
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; moveSprites					update coordinates of all sprites, enemy ai and ladybug collision with enemy
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		moveSpritesIndex	index to current sprite
;			moveSpritesPathCounter	count the number of paths from sprite location to check for valid junction
;
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; x and y delta for the four possible directions
	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesDirX

	equb 0, 0, -1 , 1			; X up down left right

.moveSpritesDirY

	equb -1, 1, 0, 0			; Y up down left right
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; move ladybug and enemies 1 pixel
	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSprites

	ldx #0					; start at sprite index 0 (ladybug)
	jsr moveSpritesPixel			; move ladybug and enemies (if they're enabled to move)
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; move enemies an extra pixel if required (enemy speed)
	;---------------------------------------------------------------------------------------------------------------------------------------------

	clc					; enemy speed fraction counter see if we need to move the enemies another pixel
	lda enemySpeedCounter
	adc enemySpeed
	sta enemySpeedCounter
	bcs moveSpritesEnemy			; if carry generated (extra move required for enemies)
	jmp moveSpritesExit
	
.moveSpritesEnemy

	ldx #1					; then start at sprite index 1 (enemies) and move them again (if they're enabled to move)

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; move sprites
	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesPixel	

	stx moveSpritesIndex			; set the starting sprite index

	inc enemyMoveCounter			; increment pixel count used for enemy release delay

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

	cpx #0					; if this is sprite 0 (ladybug) then skip collision checks and move onto next sprite
	bne moveSpritesCollision
	jmp moveSpritesNext

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check for enemy collision with ladybug which is true when
	; abs(ladybugx - enemyx) < collisionRange
	; and
	; abs(ladybugy - enemyy) < collisionRange
	; and
	; abs(ladybugx - enemyx) + abs(ladybugy - enemyy) < collisionRange
	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesCollision

	lda spritesDir + 0			; if ladybug is active (not blanked) then
	and #spriteBlanking
	bne moveSpritesCheckAlignmentX

	lda spritesX, x				; calculate abs(enemyX - ladybugX)
	sec
	sbc spritesX + 0
	bcs moveSpritesCollisionX
	eor #&ff
	adc #1					; (carry is clear here so no need for clc)

.moveSpritesCollisionX

	cmp #collisionRange			; if x distance < collisionRange
	bcs moveSpritesCheckAlignmentX

	sta moveSpritesDistanceSave		; save x distance

	lda spritesY, x				; calculate abs(enemyY - ladybugY)
	sec
	sbc spritesY + 0
	bcs moveSpritesCollisionY
	eor #&ff
	adc #1					; (carry is clear here so no need for clc)

.moveSpritesCollisionY

	cmp #collisionRange			; if y distance < collisionRange
	bcs moveSpritesCheckAlignmentX

	adc moveSpritesDistanceSave		; add together both x and y distances (carry is clear so no need for clc)

	cmp #collisionRange			; if combined distance is less than collisionRange
	bcs moveSpritesCheckAlignmentX

	jsr ladybugKill				; then ladybug and enemy have collided so kill ladybug

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check if enemy is exactly aligned with grid
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

	jsr spriteToAddr			; enemy sprite is grid aligned so convert sprite XY to tileMapAddr

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check if enemy hit a skull
	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesCheckSkull

	ldy #24					; if tile under enemy = skull
	lda (tileMapAddr), y
	cmp #mapTileSkull
	bne moveSpritesEnemyAi

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; enemy hit a skull so remove skull from map, remove skull from screen, kill enemy, spawn new enemy in center box (if its empty)
	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesKillEnemy

	lda #mapTileBlank			; erase skull from map
	sta (tileMapAddr), y

	jsr offsetDrawMapTileAddr		; adjust mapTileAddress to be location underneath sprite center

	lda #mapTileBlankObj			; and erase skull from screen
	jsr drawMapTile

	lda #sfxSkull				; play skull sound
	jsr playSound

	dec enemiesActive			; reduce number of active enemies
	
	lda #spriteBlanking			; disable the current enemy
	sta spritesDir, x

	jsr enemySpawn				; spawn new enemy in center box (if its empty)

	jmp moveSpritesNext			; skip to next sprite

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; enemy ai
	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesEnemyAi

	ldx #3					; remove unavailable paths that are blocked by solid walls/turnstiles
	
.moveSpritesEnemyAiRemovePaths

	ldy mapDir, x
	lda (tileMapAddr), y
	sta moveSpritesAvailablePaths, x

	dex
	bpl moveSpritesEnemyAiRemovePaths

	ldx moveSpritesIndex			; get enemy sprite index

	lda spritesDir, x			; get current enemy direction
	eor #1					; flip to 180 direction
	tay					; and remove that direction from available paths
	lda #wallSolid
	sta moveSpritesAvailablePaths, y

	jsr random				; if (random and 15) + enemyAttack < 9
	and #15
	clc
	adc enemyAttack
	cmp #9
	bcc moveSpritesRandomAvailableDirection	; then pick a random direction

.moveSpritesEnemyAiCheckUp			; else check the 4 directions

	ldy #moveUp

	lda spritesY + 0
	cmp spritesY, x
	ror a
	ora moveSpritesAvailablePaths, y
	sta moveSpritesTargetPaths, y
	
.moveSpritesEnemyAiCheckDown

	iny

	lda spritesY, x
	cmp spritesY + 0
	ror a
	ora moveSpritesAvailablePaths, y
	sta moveSpritesTargetPaths, y

.moveSpritesEnemyAiCheckLeft

	iny

	lda spritesX + 0
	cmp spritesX, x
	ror a
	ora moveSpritesAvailablePaths, y
	sta moveSpritesTargetPaths, y

.moveSpritesEnemyAiCheckRight

	iny

	lda spritesX, x
	cmp spritesX + 0
	ror a
	ora moveSpritesAvailablePaths, y
	sta moveSpritesTargetPaths, y

	lda moveSpritesTargetPaths + moveUp	; if a target path is not available
	and moveSpritesTargetPaths + moveDown
	and moveSpritesTargetPaths + moveLeft
	and moveSpritesTargetPaths + moveRight
	bmi moveSpritesRandomAvailableDirection	; then choose a random available path

.moveSpritesChooseTargetPath

	jsr random				; else choose a random available target path
	and #3

	tay
	lda moveSpritesTargetPaths, y
	bpl moveSpritesSetEnemyDirection
	bmi moveSpritesChooseTargetPath


.moveSpritesRandomAvailableDirection

	lda moveSpritesAvailablePaths + moveUp	; if all paths are blocked
	and moveSpritesAvailablePaths + moveDown
	and moveSpritesAvailablePaths + moveLeft
	and moveSpritesAvailablePaths + moveRight
	bpl moveSpritesRandomAvailableDirectionLoop

	lda spritesDir, x			; then get current direction
	eor #1					; flip direction 180
	tay
	lda #0					; and unblock the path
	sta moveSpritesAvailablePaths, y

.moveSpritesRandomAvailableDirectionLoop

	jsr random				; choose a random path
	and #3

	tay					; if path not available then choose another
	lda moveSpritesAvailablePaths, y
	bmi moveSpritesRandomAvailableDirectionLoop

.moveSpritesSetEnemyDirection

	sty spritesDir, x			; set enemy direction to chosen path

.moveSpritesNext

	inc moveSpritesIndex			; until all enemy sprites have been processed
	lda moveSpritesIndex
	cmp #spritesTotal
	beq moveSpritesExit
	jmp moveSpritesLoop

.moveSpritesExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawPlayfieldUpper				draws the top red, yellow and cyan boxes with text "special" "extra" "x2 x3 x5"
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
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
	
.drawPlayfieldUpperGetData			; repeat

	lda drawPlayfieldUpperBoxData + 0, y	; get screen address
	sta drawMapTileAddr + 0
	lda drawPlayfieldUpperBoxData + 1, y
	sta drawMapTileAddr + 1

	ldx drawPlayfieldUpperBoxData + 2, y	; get tile count
	
	lda drawPlayfieldUpperBoxData + 3, y	; get tile

.drawPlayfieldUpperLoop				; repeat

	jsr drawExtraTile			; draw the tile
	
	dex					; until tilecount done
	bne drawPlayfieldUpperLoop
	
	iny					; move to next entry in list
	iny
	iny
	iny

	cpy #9 * 4				; until all done
	bne drawPlayfieldUpperGetData

	jsr drawString				; draw the 3 'x' multipliers in cyan
	equw screenAddr + 2 + 16 * chrColumn
	equs colorCyan, chrMultiplierX, ' ', chrMultiplierX, ' ', chrMultiplierX, &ff

	; contine to drawPlayfieldUpperBonus
	


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawPlayfieldUpperBonus			draws the bonus letters and multipliers in the correct colors from bonusBits flags
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		bonusBitsCopy		copy of bonus bits while processing letter colors
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawPlayfieldUpperBonus

	stx drawPlayfieldUpperBonusSaveX	; preserve register

	jsr drawString				; setup screen address to the bonus first letter position 'S'
	equw screenAddr + 2 + 16
	equb &ff
	
	lda bonusBits + 0			; copy bonus bits (shifting everything left one bit to drop the unused bit 15)
	asl a
	sta bonusBitsCopy + 0
	lda bonusBits + 1
	rol a
	sta bonusBitsCopy + 1

	ldx #0					; index for bonus tables

.drawPlayfieldUpperText				; repeat

	lda #pixelsWhite			; if bit = 1 use white
	bit bonusBitsCopy + 1
	bmi drawPlayfieldUpperTextChr
	lda upperBonusColor, x			; else bit = 0 so use color from table

.drawPlayfieldUpperTextChr

	sta drawChrColor			; store color

	lda upperBonusText, x			; get chr from table and print it
	jsr drawChr
	
	lda upperBonusOffset, x			; add address offset from table to screen position
	clc
	adc drawChrAddr
	sta drawChrAddr
	bcc drawPlayfieldUpperTextShiftBits
	inc drawChrAddr + 1
	
.drawPlayfieldUpperTextShiftBits

	asl bonusBitsCopy + 0			; shift to next bonus bit
	rol bonusBitsCopy + 1

	inx					; until all bonus bits processed
	cpx #15
	bne drawPlayfieldUpperText

	ldx drawPlayfieldUpperBonusSaveX	; restore register

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
	equb 24,24				; *2 *3 offsets only
						; (*5 offset is read out of bounds from below but no chrs are printed so the incorrect offset doesn't matter)

	;---------------------------------------------------------------------------------------------------------------------------------------------

.upperBonusColor				; colors for special, extra, 235

	equb pixelsSpecial0, pixelsSpecial1, pixelsSpecial0, pixelsSpecial1, pixelsSpecial0, pixelsSpecial1, pixelsSpecial0
	equb pixelsExtra1, pixelsExtra0, pixelsExtra1, pixelsExtra0, pixelsExtra1
	equb pixelsCyan, pixelsCyan, pixelsCyan

	;---------------------------------------------------------------------------------------------------------------------------------------------

bonusBitsSpecial	= %01111111		; bit mask for special bits on bonusBits + 1
bonusBitsExtra		= %11111000		; bit mask for extra bits on bonusBits + 0
bonusBitsMultiplier	= %00000111		; bit mask for x2x3x5 multiplier bits on bonusBits + 0



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawPlayfieldLower				draws the bottom info panel showing
;						ladybug, lives, diamond available, vegetable, vegetable score, level, score, highScore, highScore name
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawPlayfieldLower

	lda spriteBaseImg + 0			; ladybug sprite image
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

	jsr swrDrawPlayfieldLowerDiamond	; draw a diamond if enabled else draw a space (see loader.asm)

	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #34					; draw vegetable image to the right of ladybug
	sta drawSpriteX
	lda #23 * 8 + 6
	sta drawSpriteY

	jsr drawBonusItemVegetable

	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawString				; set screen address for vegetable score and color to green
	equw screenAddr + 2 + 16 + 5 * chrColumn + 24 * chrRow
	equb colorGreen, &ff

	lda vegetableScore			; draw 2 digit vegetable score in green
	jsr drawBcd

	lda #&00				; draw 2 digit 00 in green
	jsr drawBcd

	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawString				; draw "L" in blue then set color to magenta
	equw screenAddr + 2 + 8 + 10 * chrColumn + 24 * chrRow
	equs colorBlue, "L", colorMagenta, &ff

	lda level				; draw 2 digits level number in magenta
	jsr drawBcd

	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawString				; set screen position and draw "1P" in yellow
	equw screenAddr + 2 + 16 + 13 * chrColumn + 24 * chrRow
	equs colorYellow, "1P", &ff
	
						; set screen position to last digit of score and draw a "0" tile
	lda #lo(screenAddr + 22 * chrColumn + 24 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 22 * chrColumn + 24 * chrRow)
	sta drawMapTileAddr + 1

	lda #0
	jsr drawExtraTile

	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawHighScore			; draw high score points

	; continue down to draw demo mode or highscore name



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawDemoModeOrName				draw demo mode or high score name
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawDemoModeOrName

	lda demoMode				; if demo mode then draw " GAME OVER"
	beq drawHighScoreName

	jsr drawString
	equw screenAddr + 2 + 16 + 5 * chrColumn + 25 * chrRow
	equs colorMultiplier0, " GAME OVER", &ff

	rts

.drawHighScoreName

	jsr drawString				; else draw high score name
	equw screenAddr + 2 + 16 + 5 * chrColumn + 25 * chrRow
	equb colorRed, &ff
	
	lda #lo(highScoreTable + 3)
	sta drawTextAddr + 0
	lda #Hi(highScoreTable + 3)
	sta drawTextAddr + 1
	
	jmp drawText



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawPlayfieldLowerLives			; draw lives value
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawPlayfieldLowerLives

	jsr drawString				; set screen position to right of ladybug, set color to yellow
	equw screenAddr + 2 + 16 + 1 * chrColumn + 24 * chrRow
	equb colorYellow, &ff

	lda lives				; draw 2 digit lives value and return
	jmp drawBcd



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawPlayfieldMiddle				draw middle playfield tiles
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
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
; drawBonusItemCenter				draw vegetable sprite in center bug box (if enabled)
;						or draw diamond sprite (if enabled and correct level)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
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
	
	lda #centerDiamond			; then draw a diamond and return
	bne drawSprite10x10			; (bne used as branch always)

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

	lda #sprite10x10Height			; set height for the 10x10 pixel sprite
	sta drawSpriteColumnVtest + 1
	sta drawSpriteColumnTileHeight + 1
	
	lda #opcodeINX				; drawing normal so use INX instruction
	sta drawSpriteNextLineInstruction

	lda #sprite10x10Bytes			; store number of bytes for sprite in counter
	sta drawByteCount

	bne drawSpriteGetAddr			; draw the 10x10 pixel sprite (bne used as branch always)



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawSprite					draw sprite on screen
; drawSpriteFlipped				draw sprite on screen vertically flipped
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	drawSpriteImg		sprite to be drawn
;			drawSpriteX		x position of sprite
;			drawSpriteY		y position of sprite
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		drawSpriteScreenAddr	calculated screen position
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; additional info:	3 will be automatically added to spriteImg when the x position requires a pixel shifted image
;			to select the alternate 1 pixel shifted version of the sprite
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

	bne drawSpriteGetAddr			; contine to drawSprite code

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup drawing vertically mirrored sprite (flipped)
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
	; convert sprite x and y coordinates to screen address
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawSpriteGetAddr

	jsr spriteToScreen			; convert sprite XY to screen address

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup address of sprite data from table
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda drawSpriteImg			; get sprite data address from spriteImg address table

	tay
	lda spriteImgAddrLo, y
	sta drawSpriteRead + 1
	lda spriteImgAddrHi, y
	sta drawSpriteRead + 2

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup screen address and offset for drawing
	;---------------------------------------------------------------------------------------------------------------------------------------------

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
	; data value of following instruction modified by drawSprite/drawSpriteVflip/drawBonusItemCenter etc
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #dummy8				; get initial vertical value

.drawSpriteRead

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; sprite source address value of following intruction is setup by previous code
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda dummy16, x				; read byte from sprite (address previously setup)

.drawSpriteWrite	

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; screen destination address value of following intruction is setup by previous code
	;---------------------------------------------------------------------------------------------------------------------------------------------

	sta dummy16, y				; write byte to screen (address previously setup)

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
	beq drawSpriteExit			; exit if all bytes drawn

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; the following instruction is replaced with INX or DEX by drawSprite/drawSpriteVflip/drawBonusItemCenter for normal or mirrored drawing mode
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

	adc #dummy8				; adjust sprite data address for next column
	sta drawSpriteRead + 1
	bcc drawSpriteColumnTileHeightNext
	inc drawSpriteRead + 2

.drawSpriteColumnTileHeightNext

	clc					; adjust screen address for next column and loop back to do next column
	lda drawSpriteScreenAddr
	adc #8
	sta drawSpriteScreenAddr
	bcc drawSpriteColumn
	inc drawSpriteScreenAddr + 1
	bpl drawSpriteColumn

.drawSpriteExit

	ldx drawSpriteSaveX			; restore registers
	ldy drawSpriteSaveY

	rts					; return


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; addScoreMultiply				add A to score in bcd mode with multiplier
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	A			score value
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
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
	
.addScoreMultiplyLoop				; repeat

	lda addScoreMultiplySaveA		; add score to player score
	jsr addScore
	
	dex					; until loop count done
	bne addScoreMultiplyLoop
	
	ldx addScoreMultiplySaveX		; restore register

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; addScore					add A to score in bcd mode
;						score is stored as 6 digits in 3 bcd bytes, last digit (units) not stored and is always 0 on screen
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	A			LSB
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.addScore

	sed					; bcd mode

	clc					; add the tens/hundreds
	adc score
	sta score

	lda #0

.addScoreMiddle

	adc score + 1				; add the thousands/ten thousands (with carry)
	sta score + 1

	lda #0

.addScoreTop

	adc score + 2				; add the hundred thousands/millions (with carry)
	sta score + 2

	bcc addScoreExit			; if no score overflow ( > 999999 ) then return
	
	lda #&99				; else set score to 999999
	sta score
	sta score + 1
	sta score + 2

.addScoreExit

	cld					; binary mode

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; addScoreVegetable				multiply vegetable score by 10(bcd) and add to score
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.addScoreVegetable

	lda vegetableScore			; shift vegetable tens to hundreds
	asl a
	asl a
	asl a
	asl a
	jsr addScore				; and add to score
	
	lda vegetableScore			; shift vegetable hundreds to thousands
	lsr a
	lsr a
	lsr a
	lsr a
	
	sed					; bcd mode

	clc					; and add to score
	bcc addScoreMiddle



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; addScoreDiamond				add diamond bonus to score
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.addScoreDiamond

	sed					; bcd mode

	clc					; add the diamond bonus to score
	lda #bonusDiamondScore
	bcc addScoreTop



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; addScoreSpecial				add special bonus to score
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.addScoreSpecial

	bit highScoreChallenge			; if high score challenge mode
	bmi addScoreExit			; then exit without adding special bonus score

	sed					; bcd mode

	clc					; add the special bonus score to score
	lda #bonusSpecialScore and 15
	bcc addScoreTop				; (bcc used as branch always)



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; generateValidation				generate a validation code for the high score table and game settings
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.generateValidation

	ldx #0					; validationCode = 0
	stx validationCode
	
.generateValidationLoop				; repeat

	lda configData, x			; validationCode += configData[x] eor #magicNumber
	eor #magicNumber
	clc
	adc validationCode
	sta validationCode
	
	inx
	cpx #validationCode - configData
	bne generateValidationLoop		; until end of configData
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawBcdMini					draw 2 digits of BCD mini chr tiles
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	A			value to display
;			drawChrMiniAddr		screen address to draw
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			y			preserved
;			drawChrMiniAddr		points to next mini chr tile position on screen
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

	; continue on into drawChrMini for the low nybble



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawChrMini					draw mini chr to screen, move to next chr position
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	A			chr to be drawn 0-9
;			drawChrMiniAddr		screen address for chr
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;			drawChrMiniAddr		points to next chr position
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
	adc #lo(miniFontBin)			; (carry is clear here so no need for clc)
	sta drawChrMiniLoop + 1
	lda #0
	adc #hi(miniFontBin)
	sta drawChrMiniLoop + 2

	ldy #miniFontBytes - 1			; number of bytes to transfer
	
.drawChrMiniLoop				; repeat

	lda dummy16, y				; get byte from minifont (address previously setup)
	
.drawChrMiniWrite

	sta dummy16, y				; store it on screen (address previously setup)
	
	dey
	bpl drawChrMiniLoop			; until all bytes transferred
	
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
; alias to draw address
;-----------------------------------------------------------------------------------------------------------------------------------------------------

drawChrMiniAddr = drawChrMiniWrite + 1




;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawMapTileAddrAdvance			advance the drawMapTileAddr by value in accumilator
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	A			value to add to drawMapTileAddr
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;			drawMapTileAddr		advanced by value in A
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
;						if vegetableScore < &9500 then vegetableScore = vegetableScore + &0500
;						vegetableImage = vegetableImage + 1
;						if vegetableImage >= horseradish + 1 then vegetableImage = cucumber
;						mazeMap = mazeMap + 1
;						if MazeMap >= 6 then mazeMap = 0
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.levelAdvance

	sed					; bcd mode

	lda level				; if level < &99
	cmp #&99
	bcs levelAdvanceVegetableScore

	adc #&01				; add &01 to level (carry already clear from previous cmp result)
	sta level
	
.levelAdvanceVegetableScore	

	lda vegetableScore			; if vegetableScore < &9500
	cmp #&95
	bcs levelAdvanceVegetableImage
	adc #&05				; then add &0500 to vegetableScore
	sta vegetableScore
	
.levelAdvanceVegetableImage

	cld					; binary mode

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
;                                               unblank ladybug (if required)
;						erase object score (if required)
;						erase vegetable score (if required)
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
	
						; position to 1 tile left of center box

	lda #lo(screenAddr + vegetableScoreX * chrColumn + vegetableScoreY * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + vegetableScoreX * chrColumn + vegetableScoreY * chrRow)
	sta drawMapTileAddr + 1
	
	lda tileMap + centerBoxLeft		; redraw the 3 tiles that were over written by vegetable bonus score
	jsr drawMapTile
	
	lda tileMap + centerBoxCenter
	jsr drawMapTile
	
	lda tileMap + centerBoxRight
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
	
	lda #spritesAnimationTime		; then reset animation timer
	sta spritesImgFrameCounter
	
	dec spritesImgFrame			; animation frame -= 1
	bpl updateAnimationFrameExit		; if animation frame < 0

	lda #3					; then animation frame = 3
	sta spritesImgFrame

.updateAnimationFrameExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; instructions					display game instructions and wait for start, esc to be pressed or idle timeout
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.instructionsLetters

	equb mapTileS, mapTileP, mapTileE, mapTileC, mapTileI, mapTileA, mapTileL
	equb -1, -1, -1
	equb mapTileYellowE, mapTileYellowX, mapTileYellowT, mapTileYellowR, mapTileYellowA

.instructionsLettersEnd

	;---------------------------------------------------------------------------------------------------------------------------------------------

.instructions

	jsr playSoundSilence			; kill any current sounds

	lda demoMode				; if demo mode
	beq instructionsInit

	sec					; then return with carry set (skip instructions, start demo game)
	rts

.instructionsInit

	jsr initMiddle				; initialize and draw empty playfield with timer, initialize all sprites as blanked and erased

	lda #palObject + palRed			; set object color to red so that "SPECIAL" tiles are red
	sta ulaPalette

	lda #2					; draw two random flowers at screen row 2
	jsr drawFlowers

	jsr drawString				; draw "instructions" in red
	equw screenAddr + 2 + 8 + 5 * chrColumn + 4 * chrRow
	equs colorRed, "INSTRUCTIONS", &ff
	
						; position ready for the 3 cyan hearts objects

	lda #lo(screenAddr + 8 + 2 * chrColumn + 7 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 8 + 2 * chrColumn + 7 * chrRow)
	sta drawMapTileAddr + 1

	ldx #3					; draw 3 cyan hearts

.instructionsHeartsLoop

	lda #mapTileCyanHeart			; draw cyan heart
	jsr drawMapTile

	lda #column				; advance 1 column
	jsr drawMapTileAddrAdvance

	dex
	bne instructionsHeartsLoop

	jsr drawString				; draw "multiply score" in green
	equw screenAddr + 2 + 16 + 6 * chrColumn + 7 * chrRow
	equs colorGreen, "MULTIPLY SCORE", &ff

						; position ready for drawing special and extra letter object tiles

	lda #lo(screenAddr + 8 + 3 * chrColumn + 10 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 8 + 3 * chrColumn + 10 * chrRow)
	sta drawMapTileAddr + 1

	ldx #0					; set index to special/extra object tile table

.instructionsLettersLoop			; repeat

	lda instructionsLetters, x		; get object tile from table

	bmi instructionsLettersAddrAdjust	; if its an object tile then draw it, if its a -1 then do nothing

	jsr drawMapTile

.instructionsLettersAddrAdjust

	lda #column				; advance 1 column
	jsr drawMapTileAddrAdvance
	
	inx					; move to next entry in table

	cpx #instructionsLettersEnd - instructionsLetters
	bne instructionsLettersLoop		; until done

	jsr drawString				; draw "collect for bonus" in green
	equw screenAddr + 2 + 3 * chrColumn + 11 * chrRow
	equs colorGreen, "COLLECT FOR BONUS", &ff

	jsr drawString				; draw "garden prizes" in green
	equw screenAddr + 2 + 5 * chrColumn + 12 * chrRow
	equs "GARDEN PRIZES", &ff

	jsr drawString				; draw "return pauses" in magenta
	equw screenAddr + 2 + 5 * chrColumn + 15 * chrRow
	equs colorMagenta, "RETURN PAUSES", &ff

	jsr drawString				; draw "move to unpause" in yellow
	equw screenAddr + 2 + 4 * chrColumn + 16 * chrRow
	equs colorYellow, "MOVE TO UNPAUSE", &ff

	jsr drawString				; draw "hold esc to quit!" in red
	equw screenAddr + 2 + 3 * chrColumn + 17 * chrRow
	equs colorRed, "HOLD ESC TO QUIT!", &ff

	jsr drawString				; draw "> START GAME <" in flashing red/magenta, skull color, flashing red/magenta
	equw screenAddr + 2 + 8 + 4 * chrColumn + 20 * chrRow
	equs colorSpecial0, chrRight, colorSkull, " START GAME ", colorSpecial0, chrLeft, &ff
	
	lda #sfxObject				; play object sound effect
	jsr playSound

	lda #idleTime				; reset the idle timeout
	sta idleCounter

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; process functions and wait keyboard/joystick released
	;---------------------------------------------------------------------------------------------------------------------------------------------

.instructionsRelease				; repeat

	lda idleCounter				; if idle counter timed out then return to main menu
	beq instructionsReturnFalse

	jsr instructionsFunctions		; update colors and get input bits (keyboard/joystick)

	bne instructionsRelease			; until no input bits are active (nothing pressed)
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; process functions and wait keyboard/joystick pressed
	;---------------------------------------------------------------------------------------------------------------------------------------------

.instructionsPress				; repeat

	lda idleCounter				; if idle counter timed out then return to main menu
	beq instructionsReturnFalse

	jsr instructionsFunctions		; update colors and get input bits (keyboard/joystick)

	beq instructionsPress			; until an input bit is active

	cmp #keyBitStart			; if start key was pressed then return with true status (start game)
	beq instructionsReturnTrue

	cmp #keyBitEsc				; if esc key was pressed then return with false status (return to menu)
	beq instructionsReturnFalse
	
	bne instructionsRelease			; else loop back and wait for release again

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

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; wait for timer1 interrupt and process functions
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr waitIntLower			; wait for timer1 interrupt and read analogue joystick (if enabled)

	jsr instructionsLadybugAnimation	; do the ladybug walking animation

	jsr animateLadybug			; update ladybug direction and frame counter
	
	jsr redrawSprites			; draw ladybug and enemy sprites

	jsr moveSprites				; move ladybug sprite

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; wait for vsync interrupt and process functions
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr waitIntUpper			; wait for vsync interrupt and read analogue joystick (if enabled)

	jsr processSound			; process sound effects and music

	jsr redrawSprites			; draw ladybug and enemy sprites

	jsr updateAnimationFrame		; update the animtion frame number

	jsr updateSkullColor			; update the skull palette color

	jsr updateBonusColor			; update the bonus letters palette colors (also used for lower diamond)

	jsr drawScore				; draw score (1 digit per loop)

	jsr random				; call random so that when game is started objects are randomized

	jsr inputScan				; read keyboard input and joystick (if enabled)

	lda playerInput				; return with input bits
	rts



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; initialize ladybug walking animation
	;---------------------------------------------------------------------------------------------------------------------------------------------

.instructionsLadybugAnimation

	lda animateLadybugActive		; if ladybug animation not active
	bne instructionsLadybugAnimationExit
	
	lda #animateLadybugInstructions		; then start the animation
	jsr animateLadybugInitialize

.instructionsLadybugAnimationExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; ladybugDeathAnimation
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.ladybugDeathAnimationTable

	equb -2,  0				; floating angel x, y delta
	equb -2, -1
	equb  0, -1
	equb  2,  0
	equb  2, -1
	equb  0, -1

	;---------------------------------------------------------------------------------------------------------------------------------------------

angelMin	= 8 * 1				; angel sprite minimum x/y value (keep within playfield)
angelMax	= 8 * 21			; angel sprite maximum x value (keep within playfield)

	;---------------------------------------------------------------------------------------------------------------------------------------------

.ladybugDeathAnimation

	lda ladybugDeathEnable			; if ladybug death animation enabled
	beq ladybugDeathAnimationReturnFalse

	lda pauseLadybug			; if ladybug pause timer >= flash time
	cmp #256 - ladybugDeathFlashTime
	bcc ladybugDeathAnimationDrawAngel

	and #2					; then flash ladybug
	bne ladybugDeathAnimationBlank
	
	sta ladybugDeathAnimationIndex		; initialize death animation index = 0 for 2nd part

	lda spritesDir + 0
	and #spriteBlanking eor 255
	sta spritesDir + 0

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
	rts

.ladybugDeathAnimationReturnFalse

	clc					; return false (level restart not yet required)
	rts

	;---------------------------------------------------------------------------------------------------------------------------------------------

.ladybugDeathAnimationDrawAngel

	lda spritesY + 0			; check if we're in the correct screen half to draw sprite
	cmp #upperLowerThreshold
	ror a					; I very rarely write strange code but this bit is a little strange
	eor screenHalf
	bpl ladybugDeathAnimationCheckMusic

	lda spritesX + 0			; copy ladybug xy position for address conversion and angel sprite drawing
	sta spriteToAddrX
	sta drawSpriteX
	lda spritesY + 0
	sta spriteToAddrY
	sta drawSpriteY

	jsr spriteToAddr			; convert xy to tile map address and screen tile address
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldy #0					; redraw 3x3 tiles around ladybug
	ldx #3

.ladybugDeathAnimationDrawTiles

	lda (tileMapAddr), y
	jsr drawMapTile
	iny
	lda (tileMapAddr), y
	jsr drawMapTile
	iny
	lda (tileMapAddr), y
	jsr drawMapTile
	tya
	clc
	adc #23 - 2
	tay

	lda #lo(chrRow - 3 * chrColumn)		; move to next row
	clc
	adc drawMapTileAddr
	sta drawMapTileAddr
	lda #hi(chrRow - 3 * chrColumn)
	adc drawMapTileAddr + 1
	sta drawMapTileAddr + 1
	
	dex
	bne ladybugDeathAnimationDrawTiles

	;---------------------------------------------------------------------------------------------------------------------------------------------

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

	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda pauseLadybug			; if its time to move
	cmp #256 - ladybugDeathFlashTime - ladybugDeathWaitTime
	bcs ladybugDeathAnimationDrawAngelExit

	ldx ladybugDeathAnimationIndex		; get index into animation table

	lda spritesX + 0			; get signed x direction and add to sprite x
	clc
	adc ladybugDeathAnimationTable, x	; adjust angel x postion using direction table

	cmp #angelMin				; clip x to stay within the playfield area
	bcs ladybugDeathAnimationLimitXhi
	lda #angelMin

.ladybugDeathAnimationLimitXhi

	cmp #angelMax
	bcc ladybugDeathAnimationLimitXstore
	lda #angelMax

.ladybugDeathAnimationLimitXstore

	sta spritesX + 0			; save new sprite x position

	lda spritesY + 0			; get signed y direction and add to sprite y
	clc
	adc ladybugDeathAnimationTable + 1, x	; adjust angel y position using direction table

	cmp #angelMin				; clip y to stay within the playfield area
	bcs ladybugDeathAnimationLimitYstore
	lda #angelMin

.ladybugDeathAnimationLimitYstore

	sta spritesY + 0			; save new sprite y position

	lda vsyncCounter			; after 8 vsync frames have counted
	and #7
	bne ladybugDeathAnimationDrawAngelExit
	
	inx					; index now points to next pair of directions
	inx

	cpx #12					; if index at end of table
	bne ladybugDeathAnimationFrame

	ldx #0					; then set it back to start
	
.ladybugDeathAnimationFrame

	stx ladybugDeathAnimationIndex		; save updated index for next time around

.ladybugDeathAnimationDrawAngelExit

	jmp ladybugDeathAnimationCheckMusic	; go check if death animation music is complete (end of animation)



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; initLevelSettings				setup letters, skulls and enemy settings etc, everything for the current level
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.enemySpeedTable				; enemies always move 1 step per frame plus this additional fraction

						; enemy speed option 0
	equb 0.00 * 256				; 1.00 level 1-6
	equb 0.02 * 256				; 1.02 level 7-11
	equb 0.04 * 256				; 1.04 level 12-17
	equb 0.06 * 256				; 1.06 level 18-99
	
						; enemy speed option 1
	equb 0.00 * 256				; 1.00 level 1-6
	equb 0.05 * 256				; 1.05 level 7-11
	equb 0.10 * 256				; 1.10 level 12-17
	equb 0.15 * 256				; 1.15 level 18-99
	
						; enemy speed option 2
	equb 0.00 * 256				; 1.00 level 1-6
	equb 0.07 * 256				; 1.07 level 7-11
	equb 0.14 * 256				; 1.14 level 12-17
	equb 0.21 * 256				; 1.21 level 18-99
	
						; enemy speed option 3
	equb 0.05 * 256				; 1.05 level 1-6
	equb 0.13 * 256				; 1.13 level 7-11
	equb 0.21 * 256				; 1.21 level 12-17
	equb 0.29 * 256				; 1.29 level 18-99

						; enemy speed option 4
	equb 0.10 * 256				; 1.10 level 1-6
	equb 0.20 * 256				; 1.20 level 7-11
	equb 0.30 * 256				; 1.30 level 12-17
	equb 0.40 * 256				; 1.40 level 18-99
	
						; enemy speed option 5
	equb 0.15 * 256				; 1.15 level 1-6
	equb 0.30 * 256				; 1.30 level 7-11
	equb 0.45 * 256				; 1.45 level 12-17
	equb 0.59 * 256				; 1.59 level 18-99		was 1.60 but caused too many roll-overs with new release code
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.initLevelSettings

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; set number of skulls
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda level				; get current level (bcd)

	ldx #&02				; 2 skulls
	cmp #&01 + 1				; level 1
	bcc initLevelSettingsSkulls
	
	inx					; 3 skulls
	cmp #&04 + 1				; level 2-4
	bcc initLevelSettingsSkulls
	
	inx					; 4 skulls
	cmp #&09 + 1				; level 5-9
	bcc initLevelSettingsSkulls

	inx					; 5 skulls
	cmp #&17 + 1				; level 10-17
	bcc initLevelSettingsSkulls
	
	inx					; 6 skulls
						; level 18-99

.initLevelSettingsSkulls

	stx levelSkulls				; set the number of skulls

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; set enemy speed			use user set speed option for regular game or default speed for demo
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx optionEnemySpeed			; x = user selected speed for regular game
	lda demoMode
	beq initLevelSettingsSpeed
	ldx #defaultEnemySpeed			; x = default speed for demo game

.initLevelSettingsSpeed

	txa					; multiply x * 4
	asl a
	asl a
	tax

	lda level				; get current level (bcd)

	cmp #&07				; if level >= 7 then add 1 to x
	bcc initLevelSettingsSetSpeed
	inx

	cmp #&12				; if level >= 12 the add 1 to x
	bcc initLevelSettingsSetSpeed
	inx

	cmp #&18				; if level >= 18 then add 1 to x
	bcc initLevelSettingsSetSpeed
	inx

.initLevelSettingsSetSpeed

	lda enemySpeedTable, x			; set the enemy speed from the enemySpeedTable, x
	sta enemySpeed

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; set enemy attack			use user set attack for regular game or default attack for demo game
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx optionEnemyAttack			; for regular game use attack option
	lda demoMode
	beq initLevelSettingsAttack
	ldx #defaultEnemyAttack

.initLevelSettingsAttack

	stx enemyAttack

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; set enemy timer speed (number of vsync frames per timer tick)
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda level				; get current level (bcd)

	ldx #enemyTimerSlow			; if level 1 use slow timer speed
	cmp #&01 + 1
	bcc initLevelSettingsTimer
	
	ldx #enemyTimerMedium			; if level 2 to 4 use medium timer speed
	cmp #&04 + 1
	bcc initLevelSettingsTimer
	
	ldx #enemyTimerFast			; if level >= 5 use fast timer speed

.initLevelSettingsTimer

	stx enemyTimerSpeed			; set enemy timer speed
	stx enemyTimerSpeedCounter
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; zero timer position, clear timer zero, enemy release, diamond/special/extra active, level ended, game paused flags
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #0					; zero enemy timer position
	sta enemyTimer
	sta enemyTimerZero			; clear timer zero and enemy release flags
	sta enemyReleaseEnable

	sta scoreMultiplier			; start with no multiplier

	sta bonusDiamondActive			; disable diamond bonus active
	sta bonusSpecialActive			; disable special bonus active
	sta bonusExtraActive			; disable extra bonus active

	sta levelEndActive			; disable level end (level still active)

	sta pauseGame				; unpause game

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; clear the x2 x3 x5 flags
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda bonusBits + 0			; clear the x2 x3 x5 bits of bonusBits
	ora #bonusBitsMultiplier
	sta bonusBits + 0

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

	equb  1,  0,  0,  0,  0			; minimum value for ladybug lives, enemy speed, enemy attack, timer volume, sound
	
.optionsMax

	equb  10, 6, 10,  4,  3			; maximum value + 1 for ladybug lives, enemy speed, enemy attack, timer volume, sound

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenu

	lda #0					; disable demo mode
	sta demoMode

	jsr drawHighScoreName			; redraw the high score name

	jsr mainMenuDraw			; draw the main menu screen (and reset idle time)

	lda #0					; make sure the "START GAME" text is flashing (skull color) by setting shield to 0
	sta shield

	sta pauseLadybug			; unpause ladybug so that it will animate

	sta mainMenuCursor			; set cursor to first menu entry (start game)
	sta mainMenuCursorOld			; make old cursor same

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; update cursor and validation code
	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuUpdateCursor				; repeat

	jsr mainMenuDrawCursor			; draw updated cursor

	jsr generateValidation			; update the validation code

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; process functions and wait key release
	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuWaitRelease				; repeat

	lda idleCounter				; if idle counter timed out then return and start a demo game
	beq mainMenuIdleTimeout

	jsr mainMenuFunctions			; update animation, sprites, sound and scan keyboard

	bne mainMenuWaitRelease			; until no player input
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; process functions and wait key press
	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuWaitPress				; repeat

	lda idleCounter				; if idle counter timed out then return and start a demo game
	beq mainMenuIdleTimeout

	jsr mainMenuFunctions			; update animation, sprites, sound and scan keyboard

	beq mainMenuWaitPress			; until player input

	lda #idleTime				; reset the idle timeout
	sta idleCounter

	jsr mainMenuProcess			; process the key/joystick pressed option

	bcc mainMenuUpdateCursor		; until start pressed

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; return to start a new game or demo game
	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuExit

	jmp generateValidation			; update the validation code and return

	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuIdleTimeout

	lda #&ff				; enable demo mode and return to start demo game
	sta demoMode

	bne mainMenuExit


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

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; mainMenuFunctions				wait for syncs and do sprite redraw, color update, keyboard scan etc
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuFunctions

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; wait for timer1 interrupt and process functions
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr waitIntLower			; wait for timer1 interrupt and read analogue joystick (if enabled)

	jsr mainMenuLadybugAnimation		; check and initialise ladybug animation if needed

	jsr animateLadybug			; update ladybug direction and frame counter
	
	jsr redrawSprites			; draw sprites

	jsr moveSprites				; move sprites

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; wait for vsync interrupt and process functions
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr waitIntUpper			; wait for vsync interrupt and read analogue joystick (if enabled)

	jsr processSound			; process sound effects and music

	jsr redrawSprites			; draw sprites

	jsr updateAnimationFrame		; update the animtion frame number

	jsr updateSkullColor			; update the skull palette color

	jsr updateBonusColor			; update the bonus letters palette colors

	jsr drawScore				; draw score (1 digit per loop)

	jsr random				; call to random so that when the player starts a new game it'll have a random letter choice

	jsr inputScan				; read keyboard input and joystick (if enabled)

	lda mainMenuCursor			; if the cursor is on the timer volume selection
	cmp #6
	bne mainMenuFunctionsExit
	
	lda vsyncCounter			; and if vsyncCounter & 7 == 0
	and #7
	bne mainMenuFunctionsExit

	jsr playSoundTimer			; play timer sound at the selected volume

.mainMenuFunctionsExit

	lda playerInput				; return with input bits (esc key bit removed)
	and #keyBitEsc eor &ff

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; mainMenuProcess				process keyboard input and run required function
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcess

	lda playerInput				; get player input

	cmp #keyBitUp				; if up pressed then run up function
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
	beq mainMenuProcessUp			; (skip over blank line between "HIGH SCORES" and "START GAME"")

	bit highScoreChallenge			; if high score challenge mode
	bpl mainMenuProcessUpWrapAround

	sec					; if mainMenuCursor >=3 and mainMenuCursor <=5
	sbc #3
	cmp #6-3
	bcc mainMenuProcessUp			; then decrement again (skip over lives, enemy speed and enemy attack)

.mainMenuProcessUpWrapAround

	lda mainMenuCursor			; if mainMenuCursor < 0
	bpl mainMenuProcessUpExit

	lda #8					; then mainMenuCursor = 8 (wrap around)
	
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
	beq mainMenuProcessDown			; (skip over the blank line between "START GAME" and "HIGH SCORES")

	bit highScoreChallenge			; if high score challenge mode
	bpl mainMenuProcessDownWrapAround

	sec					; if mainMenuCursor >=3 and mainMenuCursor <=5
	sbc #3
	cmp #6-3
	bcc mainMenuProcessDown			; the increment again  (skip over lives, enemy speed and enemy attack)

.mainMenuProcessDownWrapAround

	lda mainMenuCursor

	cmp #9					; if mainMenuCursor >= 9
	bcc mainMenuProcessDownExit
	
	lda #0					; then mainMenuCursor == 0 (wrap around)

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

	ldx mainMenuCursor			; get mainMenuCursor position

	beq mainMenuProcessReturnTrue		; if mainMenuCursor = 0 ("START GAME") then return true
	
	cpx #2					; if mainMenuCursor = 2 ("HIGH SCORES") then display high score table
	beq mainMenuHighScores

	cpx #8					; if mainMenuCursor == 8 ("CONTROLS") then redefine the keyboard
	beq mainMenuProcessKeyboard

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; we got here so its none of the above, we are at one of the adjustable game settings so
	; add 1 to the current game setting pointed to by cursor index (x)
	; if its higher than allowed then reset it to the minimum value
	; note: x is offset by 3 so compensate by subtracting 3 from the game settings table address
	;---------------------------------------------------------------------------------------------------------------------------------------------

						; x index is offset by 3
						; first entrys in menu are 0:"START GAME", 1:"blank line", 2:"HIGH SCORES"
						; so subtract 3 from table addresses to compensate for the offset

	inc gameSettings - 3, x			; gameSettings[x - 3] += 1

	lda gameSettings - 3, x			; if gameSettings[x - 3] >= optionsMax[x - 3] (maximum value + 1)
	cmp optionsMax - 3, x
	bcc mainMenuProcessStartExit
	
	lda optionsMin - 3, x			; then gameSettings[x - 3] = optionsMin[x - 3] (minimum value)
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

	lda optionLadybugLives			; lives = optionLadybugLives (update the game lives value from the new optionLives value)
	sta lives

	jsr drawPlayfieldLowerLives		; draw the updated lives value in the lower playfield

	clc					; return false
	rts

	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessStartEnemySpeed

	cmp #4					; if cursor == 4 (enemySpeed) then
	bne mainMenuProcessStartEnemyAttack
	
	jsr mainMenuDrawEnemies			; update the 4 enemies on screen with new random enemies
	
	clc					; return false
	rts

	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessStartEnemyAttack

	cmp #5					; if cursor == 5 (enemyAttack) then
	bne mainMenuProcessReturnFalse
	
	jsr mainMenuDrawEnemies			; update the 4 enemies on screen with new random enemies
	
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

	jsr playSoundSilence			; kill any sounds currently playing

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	jsr drawScoreTable			; draw the high scores page and wait for start / esc to be pressed or idle timeout
	
	jsr mainMenuDraw			; draw the main menu screen (and reset idle time)

	clc					; return false
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; .mainMenuProcessKeyboard			redefine the keys
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessKeyboard

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawString				; draw text
	equw screenAddr + 2 + 4 * chrColumn + 20 * chrRow
	equs colorSpecial1, "PRESS ", colorMultiplier1,"UP       ", &ff

	jsr mainMenuProcessKeyboardKey		; process graphics and sound functions and return with get key index
	
	tay					; get key scan code
	lda keyScanCodes, y

	sta optionKeys + 3			; store up key scan code

	jsr drawString				; position for printing and set color to white
	equw screenAddr + 2 + 15 * chrColumn + 20 * chrRow
	equb colorWhite, &ff

	lda keyScanAscii, y			; get ascii chr of key

	sta optionKeysAscii + 3			; store in list

	jsr drawChr				; draw it on screen

	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	jsr drawString				; draw text
	equw screenAddr + 2 + 10 * chrColumn + 20 * chrRow
	equs colorMultiplier1, "DOWN", &ff
	
.mainMenuProcessKeyboardDown

	jsr mainMenuProcessKeyboardKey		; process graphics and sound functions and return with get key index
	
	tay					; get key scan code
	lda keyScanCodes, y

	cmp optionKeys + 3			; if key is same as up then try again
	beq mainMenuProcessKeyboardDown

	sta optionKeys + 2			; store down key scan code

	jsr drawString				; position for printing
	equw screenAddr + 2 + 16 * chrColumn + 20 * chrRow
	equb colorWhite, &ff

	lda keyScanAscii, y			; get ascii chr of key

	sta optionKeysAscii + 2			; store in list

	jsr drawChr				; draw it on screen

	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	jsr drawString				; draw text
	equw screenAddr + 2 + 10 * chrColumn + 20 * chrRow
	equs colorMultiplier1, "LEFT", &ff
	
.mainMenuProcessKeyboardLeft

	jsr mainMenuProcessKeyboardKey		; process graphics and sound functions and return with get key index
	
	tay					; get key scan code
	lda keyScanCodes, y

	cmp optionKeys + 3			; if key is same as up then try again
	beq mainMenuProcessKeyboardLeft
	cmp optionKeys + 2			; if key is same as down then try again
	beq mainMenuProcessKeyboardLeft

	sta optionKeys + 1			; store left key scan code

	jsr drawString				; position for printing
	equw screenAddr + 2 + 17 * chrColumn + 20 * chrRow
	equb colorWhite, &ff

	lda keyScanAscii, y			; get ascii chr of key

	sta optionKeysAscii + 1			; store in list

	jsr drawChr				; draw it on screen

	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	jsr drawString				; draw text
	equw screenAddr + 2 + 10 * chrColumn + 20 * chrRow
	equs colorMultiplier1, "RIGHT", &ff
	
.mainMenuProcessKeyboardRight

	jsr mainMenuProcessKeyboardKey		; process graphics and sound functions and return with get key index
	
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
	equw screenAddr + 2 + 18 * chrColumn + 20 * chrRow
	equb colorWhite, &ff

	lda keyScanAscii, y			; get ascii chr of key

	sta optionKeysAscii + 0			; store in list

	jsr drawChr				; draw it on screen

	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	jsr drawString				; restore original text
	equw screenAddr + 2 + 4 * chrColumn + 20 * chrRow
	equs colorYellow, "CONTROLS   ", &ff

	lda #idleTime				; reset the idle timeout
	sta idleCounter

	clc					; return
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; colors, animation, wait for key release
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessKeyboardKey

	jsr mainMenuFunctions			; update colors animation movement etc

	jsr swrKeyboardScan			; wait for key release
	bcs mainMenuProcessKeyboardKey
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; colors, animation, wait for key press and return with key index in A
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessKeyboardKeyWaitPress

	jsr mainMenuFunctions			; update colors animation movement etc

	jsr swrKeyboardScan			; wait for key press
	bcc mainMenuProcessKeyboardKeyWaitPress

	rts					; return with key press index in A



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; draw the ladybug logo of 15x3 tiles
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuDrawLogoData
						; value offset added to #extraTileLogo constant
						; or -1 to draw a blank tile

	equb -1,  0,  1, -1, -1,  2, -1, -1,  3,  4,  5, -1, -1, -1, -1
	equb  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
	equb 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35

	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuDrawLogo

	lda #lo(mainMenuDrawLogoData)		; set start address of logo tile data
	sta mainMenuDrawLogoX + 1
	lda #hi(mainMenuDrawLogoData)
	sta mainMenuDrawLogoX + 2

	ldy #0					; initialise row counter
	
.mainMenuDrawLogoY				; repeat

	clc					; calculate screen address for row
	lda #lo(4 * chrColumn)
	adc screenRowLo + 2, y
	sta drawMapTileAddr
	lda #hi(4 * chrColumn)
	adc screenRowHi + 2, y
	sta drawMapTileAddr + 1

	ldx #0					; initialise column counter
	
.mainMenuDrawLogoX				; repeat

	lda dummy16				; get byte from logo tile data (address previously setup)

	bpl mainMenuDrawLogoTile		; if tile = -1
	
	lda #extraTileBlank			; then draw a blank tile
	bne mainMenuDrawLogoTileNow

.mainMenuDrawLogoTile

	clc					; else draw logo tile
	adc #extraTileLogo

.mainMenuDrawLogoTileNow

	jsr drawExtraTile
	
	inc mainMenuDrawLogoX + 1		; inc logo tile address
	bne mainMenuDrawLogoNext
	inc mainMenuDrawLogoX + 2

.mainMenuDrawLogoNext

	inx					; until current row is complete
	cpx #15
	bne mainMenuDrawLogoX
	
	iny					; until all rows are completed
	cpy #3
	bne mainMenuDrawLogoY

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; draw the menu text and user defined control keys
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuDrawText

	jsr drawString
	equw screenAddr + 2 + 8 + 4 * chrColumn + 7 * chrRow
	equs colorRed, "UNIVERSAL", colorMagenta, chrCopyright, colorRed, "1981", &ff

	jsr drawString
	equw screenAddr + 2 + 5 * chrColumn + 9 * chrRow
	equs colorYellow, "LOVEBYTE", colorRed, chrCopyright, colorYellow, "2021", &ff

	jsr drawString
	equw screenAddr + 2 + 6 * chrColumn + 10 * chrRow
	equs colorGreen
	incbin "output/projectBuild"
	clear * - 1, *				; remove the 0a terminator byte
	org * - 1				; at the end of the projectBuild string
	equb &ff

	jsr drawString
	equw screenAddr + 2 + 8 + 6 * chrColumn + 12 * chrRow
	equs colorSkull, "START GAME", &ff

.mainMenuDrawTextHighScores

	jsr drawString
	equw screenAddr + 2 + 4 * chrColumn + 14 * chrRow
	equs colorExtra1, "HIGH SCORES", &ff

	jsr drawString
	equw screenAddr + 2 + 4 * chrColumn + 15 * chrRow
	equs colorGreen, "LADY BUGS", &ff
	
	jsr drawString
	equw screenAddr + 2 + 4 * chrColumn + 16 * chrRow
	equs colorYellow, "ENEMY SPEED", &ff
	
	jsr drawString
	equw screenAddr + 2 + 4 * chrColumn + 17 * chrRow
	equs colorGreen, "ENEMY ATTACK", &ff
	
	jsr drawString
	equw screenAddr + 2 + 4 * chrColumn + 18 * chrRow
	equs colorYellow, "TIMER VOLUME", &ff

	jsr drawString
	equw screenAddr + 2 + 4 * chrColumn + 19 * chrRow
	equs colorGreen, "SOUND", &ff
	
	jsr drawString
	equw screenAddr + 2 + 4 * chrColumn + 20 * chrRow
	equs colorYellow, "CONTROLS   ", colorWhite, &ff
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; display the current user defined control keys
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #3					; start with up key
	
.mainMenuDrawTextKeys				; repeat

	lda optionKeysAscii, x			; get ascii version of key and display it
	jsr drawChr
	
	dex					; until all 4 printed
	bpl mainMenuDrawTextKeys

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; draw the option settings
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuDrawSettings

	jsr drawString				; draw ladybug lives
	equw screenAddr + 2 + 18 * chrColumn + 15 * chrRow
	equb colorWhite, &ff
	
	lda optionLadybugLives
	ora #'0'
	jsr drawChr
	
	jsr drawString				; draw enemy speed
	equw screenAddr + 2 + 18 * chrColumn + 16 * chrRow
	equb &ff
	
	lda optionEnemySpeed
	ora #'0'
	jsr drawChr
	
	jsr drawString				; draw enemy attack
	equw screenAddr + 2 + 18 * chrColumn + 17 * chrRow
	equb &ff
	
	lda optionEnemyAttack
	ora #'0'
	jsr drawChr
	
	jsr drawString				; draw timer volume
	equw screenAddr + 2 + 18 * chrColumn + 18 * chrRow
	equb &ff
	
	lda optionTimerVolume
	ora #'0'
	jsr drawChr

	lda optionSound				; read sound option

.mainMenuDrawSettingsOff

	bne mainMenuDrawSettingsDemoOff		; if sound = 0 (off)

	jsr drawString				; draw " OFF"
	equw screenAddr + 2 + 15 * chrColumn + 19 * chrRow
	equs " OFF", &ff

	jmp playSoundSilence			; mute the sound and return

.mainMenuDrawSettingsDemoOff

	cmp #1					; if sound = 1 (on)
	bne mainMenuDrawSettingsDemoOn

	jsr drawString				; draw "  ON" and return
	equw screenAddr + 2 + 15 * chrColumn + 19 * chrRow
	equs "  ON", &ff

	rts
	
.mainMenuDrawSettingsDemoOn			; sound = 2 (demo on)

	jsr drawString				; draw "DEMO" and return
	equw screenAddr + 2 + 15 * chrColumn + 19 * chrRow
	equs "DEMO", &ff

	rts
	



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

	lda randomSeed + 0			; backup random seed as it gets modified by this draw loop
	pha					; and there is a possibility that the seed could contain 0
	lda randomSeed + 1			; after drawing if the seed contained &fcfc at entry to this function
	pha					; and this would break the random number generator

	ldx #3					; 4 enemies to draw
	
.mainMenuDrawEnemiesLoop			; repeat

	lda mainMenuEnemiesX, x			; set X and Y position for enemy from table
	sta spritesX + 1, x
	lda mainMenuEnemiesY, x
	sta spritesY + 1, x
	
	lda randomSeed + 0			; set enemy sprite image
	and #7
	tay
	lda spriteBaseImg + 1, y
	sta spritesImg + 1, x
	
	lda randomSeed + 1			; set sprite direction (not moving)
	and #3
	ora #moveStop
	sta spritesDir + 1, x

	inc randomSeed + 0			; increment sprite image for next sprite

	inc randomSeed + 1			; increment direction for next sprite

	dex					; until all enemies placed
	bpl mainMenuDrawEnemiesLoop
	
	pla					; restore the original random seed
	sta randomSeed + 1
	pla
	sta randomSeed + 0

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawFlowers					draw two flowers at supplied row
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	A			vertical row for flowers
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
; .drawRandomFlower				draw a single random flower at the current drawMapTileAddr
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawRandomFlower

	jsr random				; pick random number 0,4,8,12
	and #%00001100

	tay					; use as index for flower tile table

	lda drawRandomFlowerTile + 0, y		; draw top left
	jsr drawExtraTile

	lda drawRandomFlowerTile + 1, y		; draw top right
	jsr drawExtraTile

	clc					; move to next row
	lda drawMapTileAddr
	adc #lo(chrRow - 2 * chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrRow - 2 * chrColumn)
	sta drawMapTileAddr + 1

	lda drawRandomFlowerTile + 2, y		; draw bottom left
	jsr drawExtraTile
	
	lda drawRandomFlowerTile + 3, y		; draw bottom right
	jsr drawExtraTile

	clc					; move to next row
	lda drawMapTileAddr
	adc #lo(chrRow - 2 * chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrRow - 2 * chrColumn)
	sta drawMapTileAddr + 1

	lda #extraTileLeafL			; draw leaves and return
	jsr drawExtraTile

	lda #extraTileLeafR
	jmp drawExtraTile

	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawRandomFlowerTile				; top left, top right, bottom left, bottom right tiles for the 4 different flower types

	equb extraTileFlower0TL, extraTileFlower0TR, extraTileFlower0BL, extraTileFlower0BR
	equb extraTileFlower1TL, extraTileFlower1TR, extraTileFlower1BL, extraTileFlower1BR
	equb extraTileFlower2TL, extraTileFlower2TR, extraTileFlower2BL, extraTileFlower2BR
	equb extraTileFlower3TL, extraTileFlower3TR, extraTileFlower3BL, extraTileFlower3BR



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; mainMenuDraw					draw everything on main menu screen
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuDraw

	lda #idleTime				; reset the idle timeout
	sta idleCounter

	jsr initMiddle				; initialize and draw empty playfield with timer, initialize all sprites as blanked and erased

	jsr mainMenuDrawLogo			; draw the ladybug logo

	lda #2					; draw two random flowers at screen row 2
	jsr drawFlowers

	jsr mainMenuDrawText			; draw the menu text

	jsr mainMenuDrawSettings		; draw the current settings

	jmp mainMenuDrawEnemies			; place 4 enemies on screen and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawScoreTable				draw the high score table page and wait for start or esc to be pressed or idle timer timeout
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawScoreTable

	lda #idleTime				; reset the idle timeout
	sta idleCounter

	jsr initMiddle				; initialize and draw empty playfield with timer, initialize all sprites as blanked and erased

	jsr drawString				; draw 3 red hearts, "BEST PLAYERS" in skull color, 3 red hearts
	equw screenAddr + 2 + 8 + 1 * chrColumn + 3 * chrRow
	equs colorRed, chrHeart,chrHeart,chrHeart, colorSkull, " BEST PLAYERS ", colorRed, chrHeart,chrHeart,chrHeart, &ff
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawString				; position cursor for start of high score table
	equw screenAddr + 2 + 8 + 2 * chrColumn + 5 * chrRow
	equb &ff

	ldx #8					; 8 high scores and names to display

	lda #lo(highScoreTable)			; start at 1st entry
	sta highScorePtr
	lda #hi(highScoreTable)
	sta highScorePtr + 1
	
.drawScoreTableLoop				; repeat

	jsr drawScoreTableBlanking		; draw score with leading zero blanking

	lda #pixelsBlue				; set draw color to blue
	sta drawChrColor

	ldy #3					; if bit 7 of 1st character of name is 1 then draw a heart instead of a space
	lda (highScorePtr), y
	bpl drawScoreTableArcadeMode
	
.drawScoreTableChallengeMode

	lda #chrHeart				; heart for high score challenge mode
	bpl drawScoreTableSpacer

.drawScoreTableArcadeMode

	lda #' '				; space for arcade mode

.drawScoreTableSpacer

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

	lda highScorePtr			; advance 14 bytes to next high score entry (3 bytes score, 10 bytes name, 1 byte terminator)
	clc
	adc #14
	sta highScorePtr
	
	dex					; until all 8 entrys processed
	bne drawScoreTableLoop

	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawString				; draw "> MENU <"
	equw screenAddr + 2 + 8 + 7 * chrColumn + 21 * chrRow
	equs colorSpecial0, chrRight, colorSkull, " MENU ", colorSpecial0, chrLeft, &ff
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; do functions and wait for key release
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawScoreTableRelease

	lda idleCounter				; if idle counter timed out then return
	beq drawScoreTableExit

	jsr drawScoreTableFunctions		; update colors and scan keyboard

	bne drawScoreTableRelease		; if key pressed then wait for key release
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; do functions and wait for start/esc pressed
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawScoreTablePress

	jsr drawScoreTableFunctions		; update colors and scan keyboard

	cmp #keyBitStart			; if start or esc pressed then return
	beq drawScoreTableExit

	cmp #keyBitEsc
	beq drawScoreTableExit

	lda idleCounter				; else if idle counter timed out then return
	bne drawScoreTablePress			; else loop back and wait for key press

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

.drawScoreTableBlankingPrint			; repeat

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
	
	dey					; until all pairs displayed
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
; drawScoreTableFunctions			wait for interrupts, process sound, update colors, scan keyboard/joystick
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawScoreTableFunctions

	jsr waitIntLower			; wait for timer1 interrupt and read analogue joystick (if enabled)

	jsr waitIntUpper			; wait for vsync interrupt and read analogue joystick (if enabled)

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

	lda demoMode				; if game is in demo mode then return false
	bne checkPauseGameReturnFalse

	lda pauseGame				; if game is currently paused then handle unpausing if needed
	bne checkPauseGameTrue

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; we got here so game isnt paused and no entry or death movement animation is active
	; so check if start is pressed and if so pause game and display paused message
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda playerInput				; if start not pressed
	cmp #keyBitStart
	beq checkPauseGameActivate

.checkPauseGameReturnFalse

	clc					; then return false
	rts
	
.checkPauseGameActivate

	lda #&ff				; else pause game
	sta pauseGame
	
	jsr drawString				; and replace high score name with "  PAUSED  "
	equw screenAddr + 2 + 16 + 5 * chrColumn + 25 * chrRow
	equs colorMultiplier0, "  PAUSED  ", &ff
	
.checkPauseGameReturnTrue

	sec					; return true
	rts

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; we got here because game is currently paused so check if start is not pressed and if any movement keys are pressed then unpause game
	;---------------------------------------------------------------------------------------------------------------------------------------------

.checkPauseGameTrue

	lda playerInput				; if start is not pressed
	and #keyBitStart
	bne checkPauseGameReturnTrue

	lda playerInput				; and if any of up down left right are pressed
	and #keyBitUp + keyBitDown + keyBitLeft + keyBitRight
	beq checkPauseGameReturnTrue
	
	lda #0					; disable game pause
	sta pauseGame
	
	jsr drawDemoModeOrName			; redraw the demo mode or high score name text to replace the paused text

	clc					; return false
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
;
; nameReg					draw the name registration screen and get high score name
;
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.nameRegText
						; available characters for name registration
	equs "ABCDEFGHIJKLMNOPQRSTUVWXYZ", chrHeart, "!. "

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameReg

	jsr initMiddle				; initialize and draw empty playfield with timer, initialize all sprites as blanked and erased

	lda #nameRegTimer			; set enemy timer speed (1 tick per second)
	sta enemyTimerSpeed

	lda #0					; unpause enemies so that the timer will tick
	sta pauseEnemy

	sta pauseLadybug			; unpause ladybug so that it will animate

	sta shield				; make sure the "Enter Name" character is flashing (skull color) by setting shield to 0

	sta enemiesActive			; set active enemies to 0 so that the enemy release will sound at timer top left (time running out warning)

	lda #4					; draw two random flowers at screen row 4
	jsr drawFlowers

	ldy #3					; draw congratulations on row 3
	jsr drawCongratulations

	jsr drawString
	equw screenAddr + 2 + 6 * chrRow + 5 * chrColumn
	equs colorMagenta, "REGISTER YOUR", &ff

	jsr drawString
	equw screenAddr + 2 + 8 * chrRow + 4 * chrColumn
	equs colorYellow, "HIGH SCORE NAME", &ff

	jsr drawString
	equw screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn
	equs colorBlue, "----------", &ff

	jsr drawString
	equw screenAddr + 2 + 14 * chrRow + 4 * chrColumn
	equs colorGreen, "A B C D E F G H", &ff
	
	jsr drawString
	equw screenAddr + 2 + 16 * chrRow + 4 * chrColumn
	equs "I J K L M N O P", &ff

	jsr drawString
	equw screenAddr + 2 + 18 * chrRow + 4 * chrColumn
	equs "Q R S T U V W X", &ff

	jsr drawString
	equw screenAddr + 2 + 20 * chrRow + 4 * chrColumn
	equs "Y Z ", chrHeart, " ! .   ", colorRed, chrLeft, " ", colorSkull, chrDown, &ff

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

	ldy #9					; fill the high score name with 10 spaces
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

	bcs nameRegExit				; if enemy timer timed out then return

	bne nameRegWaitRelease			; if key is pressed then loop back and wait for release
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; do functions, wait for key press and process key press
	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegWaitPress

	jsr nameRegFunctions			; update timer, colors and scan keyboard

	bcs nameRegExit				; if enemy timer timed out then return

	beq nameRegWaitPress			; if key not pressed then loop back and wait for key press

	jsr nameRegProcess			; process the key pressed functions

	bcc nameRegWaitRelease			; if end not selected then loop back and wait for key release

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegExit

	ldy #0					; copy bit 7 of highScoreChallenge to bit 7 of 1st character in high score name
	lda highScoreChallenge
	and #%10000000
	ora (highScorePtr), y
	sta (highScorePtr), y

	jmp playSoundSilence			; kill any sounds playing and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; nameRegFunctions				wait for sync, update sound and graphics, scan keyboard etc etc
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.nameRegFunctions

	jsr waitIntLower			; wait for timer1 interrupt and read analogue joystick (if enabled)

	jsr animateLadybug			; update ladybug direction and frame counter
	
	jsr moveSprites				; move ladybug

	jsr redrawSprites			; draw ladybug

	jsr waitIntUpper			; wait for vsync interrupt and read analogue joystick (if enabled)

	jsr processSound			; process sound effects and music

	jsr redrawSprites			; draw ladybug

	jsr updateAnimationFrame		; update the animation frame number

	jsr updateEnemyTimer			; update the enemy timer and draw tile when needed

	jsr updateSkullColor			; update the skull palette color

	jsr updateBonusColor			; update the bonus letters palette colors

	jsr inputScan				; read keyboard input and joystick (if enabled)

	lda enemyTimer				; if timer is at position 1
	cmp #1
	bne nameRegTimerActive
	lda enemyReleaseEnable			; and if timer has passed top left
	bne nameRegTimerTimeout			; then return with timer timeout status

.nameRegTimerActive

	lda playerInput				; read keyboard/joystick status and return with timer active status
	clc
	rts

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegTimerTimeout

	lda playerInput				; read keyboard/joystick status and return with timer timeout status
	sec
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; nameRegProcess and support functions		process key presses (insert/delete characters in name, cursor update etc)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessAdd32

	clc					; add 32 to cursor box position
	lda nameRegCursor
	adc #32
	sta nameRegCursor
	bpl nameRegProcessCursor		; branch always to processCursor

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessSub32

	sec					; sub 32 from cursor box position
	lda nameRegCursor
	sbc #32
	sta nameRegCursor
	bpl nameRegProcessCursor		; branch always to processCursor

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcess

	lsr playerInput				; if start pressed
	bcc nameRegProcessLeft

	lda nameRegCursor			; if cursor box = 31 (enter name)
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

	sec					; move cursor box back 1 position
	lda nameRegCursor
	sbc #1
	sta nameRegCursor

	bpl nameRegProcessCursor		; wrap around if required
	bmi nameRegProcessAdd32

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessDown

	lsr playerInput				; if down pressed
	bcc nameRegProcessUp
	
	lda #sfxMunch				; play sound effect
	jsr playSound

	clc					; move cursor box forward 8 positions
	lda nameRegCursor
	adc #8
	sta nameRegCursor

	cmp #32					; wrap around if required
	bcs nameRegProcessSub32
	bcc nameRegProcessCursor

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessUp

	lsr playerInput				; if up pressed
	bcc nameRegProcessRight
	
	lda #sfxMunch				; play sound effect
	jsr playSound

	sec					; move cursor box back 8 positions
	lda nameRegCursor
	sbc #8
	sta nameRegCursor

	bpl nameRegProcessCursor		; wrap around if required
	bmi nameRegProcessAdd32

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessRight

	lsr playerInput				; if right pressed
	bcc nameRegProcessCursor
	
	lda #sfxMunch				; play sound effect
	jsr playSound

	clc					; move cursor box forward 1 position
	lda nameRegCursor
	adc #1
	sta nameRegCursor

	cmp #32					; wrap around if required
	bcc nameRegProcessCursor
	bcs nameRegProcessSub32

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessCursor

	jsr nameRegCursorUpdate			; update new cursor box position

	clc
	rts


	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegCursorUpdate

	jsr nameRegCursorAddr			; erase old and draw new cursor box
	jsr nameRegCursorErase
	jsr nameRegCursorAddr
	jmp nameRegCursorDraw

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessDelete

	cmp #30					; if cursor box = 30 (delete)
	bne nameRegProcessChr
	
	ldy nameRegCursorText			; if text cursor != 0 (first character position in name)
	beq nameRegProcessCursor

	dey					; move text cursor back 1 position

	sty nameRegCursorText			; update text cursor

	lda #' '				; replace chr in string with space
	sta (highScorePtr), y

	clc					; print blue '-'
	lda screenColumnLo, y
	adc #lo(screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn)
	sta drawChrAddr
	lda screenColumnHi, y
	adc #Hi(screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn)
	sta drawChrAddr + 1
	lda #pixelsBlue
	sta drawChrColor
	lda #'-'
	jsr drawChr

	lda #sfxObject				; play sound
	jsr playSound

	jmp nameRegProcessCursor
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessChr

	ldy nameRegCursorText			; if text cursor != 10 (10 characters max in name)
	cpy #10
	beq nameRegProcessCursor
	
	ldx nameRegCursor			; get selected character
	lda nameRegText, x

	sta (highScorePtr), y			; store character in string

	clc					; calculate screen position from text cursor position
	lda screenColumnLo, y
	adc #lo(screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn)
	sta drawChrAddr
	lda screenColumnHi, y
	adc #Hi(screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn)
	sta drawChrAddr + 1

	lda #pixelsWhite			; draw character in white
	sta drawChrColor
	lda (highScorePtr), y
	jsr drawChr

	inc nameRegCursorText			; move text cursor forward 1

	lda #sfxTurnstile			; play sound
	jsr playSound

	jmp nameRegProcessCursor		; exit



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; nameRegCursorErase				replace cursor box with blank tiles (erase)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

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



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; nameRegCursorDraw				draw the cursor box
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.nameRegCursorDraw

	lda #registrationTL			; draw the box
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
	adc #12					; (carry is clear here, no need for clc)
	tay
	lda nameRegCursorOld
	and #&07
	asl a
	adc #3					; (carry is clear here, no need for clc)
	tax
	lda screenRowLo, y
	adc screenColumnLo, x			; (carry is clear here, no need for clc)
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
;						carry clear if no bonus was given
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.checkBonus

	lda ladybugEntryEnable			; if ladybug entry animation is enabled then return
	bne checkBonusExit



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check if diamond bonus is required
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda bonusDiamondActive
	bne checkBonusDiamondActive



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check if special bonus is required
	;---------------------------------------------------------------------------------------------------------------------------------------------

.checkBonusSpecial

	lda bonusSpecialActive			; if special bonus not active
	bne checkBonusSpecialActive
	
	lda bonusBits + 1			; check if all special letters are active
	and #bonusBitsSpecial
	bne checkBonusExtra

	lda #letterBonusTime			; pause ladybug and enemies
	sta pauseLadybug
	sta pauseEnemy

	lda soundTimers + 0			; if sound effect playing on channel 0 (music) then return
	bne checkBonusExit

	lda soundTimers + 3			; if sound effects playing on channel 3 (object) then return
	bne checkBonusExit

	lda #&ff				; flag special bonus as active
	sta bonusSpecialActive

	lda #sfxMusicLetters			; play bonus letters music
	jsr playSound

	jmp checkBonusExit			; return to game



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check if extra bonus is required
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

	lda soundTimers + 0			; if sound effect playing on channel 0 (music) then return
	bne checkBonusExit

	lda soundTimers + 3			; if sound effects playing on channel 3 (object) then return
	bne checkBonusExit

	lda #&ff				; flag special bonus as active
	sta bonusExtraActive

	lda #sfxMusicLetters			; play bonus letters music
	jsr playSound



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; return to game (no bonus)
	;---------------------------------------------------------------------------------------------------------------------------------------------

.checkBonusExit

	clc					; return to game (clear carry, no bonus)
	rts



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; diamond bonus
	;---------------------------------------------------------------------------------------------------------------------------------------------

.checkBonusDiamondActive

	lda soundTimers + 0			; if sound effect not playing on channel 0 (music)
	bne checkBonusExit

	lda soundTimers + 3			; and if sound effects not playing on channel 3 (object)
	bne checkBonusExit

	lda pauseLadybug			; if pause is over (ladybug and enemy unpaused)
	ora pauseEnemy
	bne checkBonusExit

	jsr playSoundSilence			; silence all effects and music

	jsr drawBonusScreen			; draw the diamond bonus screen

	jsr levelAdvance			; advance game to next level

	jsr addScoreDiamond			; add the diamond bonus score (bcd)

	lda #0					; disable the possibiliy of getting a diamond after this
	sta bonusDiamondEnable
	
	lda #sfxTwinkle				; play the twinkle sound
	jsr playSound

	sec					; bonus given so signal end the current level (set carry) and return
	rts



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; special bonus
	;---------------------------------------------------------------------------------------------------------------------------------------------

.checkBonusSpecialActive

	lda pauseLadybug			; if pause is over (ladybug and enemy unpaused)
	ora pauseEnemy
	bne checkBonusExit

	jsr drawBonusScreen			; draw the special bonus screen

	jsr addScoreSpecial			; add the special bonus score (bcd)

	sed					; bcd mode

	clc					; add the shield bonus to shield
	lda #bonusSpecialShield and 15
	adc shield

	bcc checkBonusSpecialShieldUpdate	; if shield >= 99
	lda #&99				; then shield = 99

.checkBonusSpecialShieldUpdate
	
	sta shield				; update shield value

	cld					; binary mode
	
	lda #bonusBitsSpecial			; clear the special letters
	ora bonusBits + 1
	sta bonusBits + 1

	lda #sfxTwinkle				; play the twinkle sound
	jsr playSound

	jsr levelAdvance			; advance game to next level

	sec					; bonus given so signal end the current level (set carry) and return
	rts



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; extra bonus
	;---------------------------------------------------------------------------------------------------------------------------------------------

.checkBonusExtraActive

	lda pauseLadybug			; if pause is over (ladybug and enemy unpaused)
	ora pauseEnemy
	bne checkBonusExit

	jsr drawBonusScreen			; draw the extra bonus screen

	sed					; bcd mode

	clc					; add bonus lives
	lda #bonusExtraLives and 15
	adc lives

	bcc checkBonusExtraLives		; if lives >= 99
	lda #&99				; then lives = 99

.checkBonusExtraLives

	sta lives

	cld					; binary mode

	lda #bonusBitsExtra			; clear the extra letters
	ora bonusBits + 0
	sta bonusBits + 0

	lda #sfxTwinkle				; play the twinkle sound
	jsr playSound

	jsr levelAdvance			; advance game to next level

	sec					; bonus given so signal end the current level (set carry) and return
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; changeTimerTile				change timer tile in tileMap (invert bit 0) and redraw tile on screen
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.changeTimerTile

	lda enemyTimer				; get current timer position * 2
	asl a

	tay					; get tileMap x and y position
	
	ldx timerXYTable + 0, y
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
	
	lda #%00000001				; invert bit 0 of tile id in tileMap timer position (switches tile green<->blue)

.changeTimerTileFlip

	eor dummy16				; (tilemap address previously setup)
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

	lda #dummy8				; (tile number previously setup)

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
;						adjust sprite image number for 1 pixel shifted alternate image if x = odd
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	drawSpriteX		sprite x coordinate
;			drawSpriteY		sprite y coordinate
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
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
	
	ldy spriteToScreenSaveY			; restore registers
	ldx spriteToScreenSaveX

	rts					; and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; updateSkullColor				update the skull palette color
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
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

	ldx #0					; if shield is active use index 0 from color table (red) for skull color
	lda shield
	bne updateSkullColorPalette

	lda vsyncCounter			; else use index = (vsyncCounter / 4) & 15 for the flashing color sequence from table
	lsr a
	lsr a
	and #15
	tax

.updateSkullColorPalette

	lda updateSkullColorTable, x		; set skull palette to color from table using index
	sta ulaPalette

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; enemyRelease					if release is enabled (enemyReleaseEnable and enemyTimerZero both set) then
;						if frame delay is correct
;						release an enemy from the center box
;						increase active enemy count
;						clear enemyReleaseEnable and enemyTimerZero
;						if enemies < maximum then spawn a new enemy into center box else set bonusItemActive to enable the center item
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.enemyRelease

	lda enemyReleaseEnable			; if enemy release is enabled
	beq enemyReleaseExit

	lda enemyTimerZero			; and if timer has hit zero position top center
	beq enemyReleaseExit

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; search enemy list to find which enemy is waiting to be released
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #spritesTotal - 1			; start with last enemy in list

.enemyReleaseLoop

	lda spritesDir, x			; if pending enemy not found
	and #spriteBlanking + moveStop
	cmp #moveStop
	beq enemyReleaseTime

	dex					; then try next enemy
	bne enemyReleaseLoop			; until all checked

	; the above branch is a branch always as an enemy will be found before x = 0

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; pending enemy found so calculate delay for this enemy to be released
	;---------------------------------------------------------------------------------------------------------------------------------------------

.enemyReleaseTime

	txa					; enemyReleaseDelay = (enemy & 3) * 4
	and #3
	asl a
	asl a
	sta enemyReleaseDelay

	lda enemyMoveCounter			; if (enemyMoveCounter & 15) != enemyReleaseDelay then return (delay the release)
	and #15
	cmp enemyReleaseDelay
	bne enemyReleaseExit

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; now its time to release the enemy so make sure there is a path to exit
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda tileMap + centerBoxUp		; if all exits blocked then enemy is trapped in a sealed box
	and tileMap + centerBoxDown 		; so don't release it
	and tileMap + centerBoxLeft
	and tileMap + centerBoxRight
	bmi enemyReleaseSpawn

	txa					; we got here so there is at least 1 exit available so save enemy sprite index
	tay

.enemyReleaseChooseDirection

	jsr random				; choose a random value 0,2,4,6 as index into direction table
	and #%00000110
	tax

	lda (enemyReleaseDir, x)		; if chosen direction is blocked then choose another
	bmi enemyReleaseChooseDirection

	txa					; convert the chosen direction index (0,2,4,6) to a direction (0,1,2,3) 
	lsr a

	sta spritesDir, y			; set enemy to chosen direction so it can be released from the center box

.enemyReleaseSpawn

	inc enemiesActive			; increase enemies active count

	lda #0					; disable enemy release and timer zero flags
	sta enemyReleaseEnable
	sta enemyTimerZero

	lda enemiesActive			; if maximum number of enemys not yet released then spawn another enemy into the center box
	cmp #spritesTotal - 1
	bne enemySpawn
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.enemyReleaseBonusItem

	lda #&ff				; else activate center bonus item (vegetable/diamond)
	sta bonusItemActive
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.enemyReleaseExit

	rts					; return




;-----------------------------------------------------------------------------------------------------------------------------------------------------
; enemySpawn					firstly check that there is currently no enemy waiting in the box
;						then find a blanked (non active) enemy and spawn it into the bix
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.enemySpawn

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; firstly check box is empty
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #spritesTotal - 1			; start at last enemy in list

.enemySpawnCheckEmpty

	lda spritesDir, x			; get enemy blanking and stopped bits
	and #spriteBlanking + moveStop

	cmp #moveStop				; if enemy is visible and has the stop flag set
	beq enemySpawnExit			; then its waiting in the box so exit this function

	dex					; else check next
	bne enemySpawnCheckEmpty		; until all checked

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; next find inactive enemy
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #spritesTotal - 1			; start at last enemy in list

.enemySpawnCheckAvailable

	lda spritesDir, x			; get enemy blanking
	and #spriteBlanking

	bne enemySpawnType			; if enemy is not visible then its available to be spawned into center box 

	dex					; else check next
	bne enemySpawnCheckAvailable		; until all checked (bne used as branch always, enemy will be found before x = 0)

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; then spawn enemy into the center box with the correct enemy type for current level
	;---------------------------------------------------------------------------------------------------------------------------------------------

.enemySpawnType

	lda level				; if level >= 9
	cmp #&09
	bcc enemySpawnLevelImage

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; level is 9 or higher so pick a random enemy type that isnt already active to make sure that all 4 enemys on screen will be different
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #&ff				; set enemy image as invalid
	sta spritesImg, x

	stx enemySpawnSaveX			; save enemy index

.enemySpawnRandom

	jsr random				; choose random enemy type 1 to 8
	and #7
	tay
	iny

	lda spriteBaseImg, y			; get enemy image id for chosen type
	
	ldx #spritesTotal - 1			; check all enemy sprites make sure that random enemy isnt already in list
	
.enemySpawnCheckImage				; repeat

	cmp spritesImg, x			; if chosen enemy type image id is found in list
	beq enemySpawnRandom			; then pick another random enemy type

	dex					; until done
	bne enemySpawnCheckImage

	ldx enemySpawnSaveX			; get enemy number
	bne enemySpawnImage			; and setup enemy image and position (bne used as branch always as enemy number > 0)

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

	lda #moveUp + moveStop			; set enemy direction up + stopped (enemy sits in box facing upwards)
	sta spritesDir, x

	lda #0					; disable center bonus item vegetable/diamond (enemy now in box so remove bonus item if there was one)
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
	; if entry animation is disabled then spawn ladybug at regular start position
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
; redrawSprites					process sprite list for upper or lower area, for each sprite in the list
;						erase sprite
;						redraw background tile at tail end of sprite
;						redraw sprite
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

						; lines 0-91 upper, lines 92-183 lower (visible display area for screen middle section)
						; actual raster lines upper = 64-155, lower = 156-247

						; sprite vertical switching point for upper/lower half
upperLowerThreshold	= 92 - (spriteTileHeight / 2)

spritesPerFrame		= 3			; maximum number of sprites in each half of the screen that can be safely
						; erased and drawn without tearing
						; if there are more than this to be redrawn then they will be processed next frame (auto frame-skip)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup maximum number of sprites and choose upper or lower half
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSprites

	lda #spritesTotal - 1			; set length of sprites list to parse (start with enemies)
	sta redrawSpritesCount
	
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

	lda #redrawSpritesIndexLower		; use the lower index pointer
	sta redrawSpritesErase + 1
	sta redrawSpritesUpdateIndex + 1

	bne redrawSpritesLadybug		; (bne used as branch always)
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; set threshold and index for upper half
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesUpper
	
	lda #opcodeBCS				; BCS instruction for upper threshold test
	sta redrawSpritesEraseThreshold
	sta redrawSpritesDrawThreshold

	lda #redrawSpritesIndexUpper		; use the upper index pointer
	sta redrawSpritesErase + 1
	sta redrawSpritesUpdateIndex + 1

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; process ladybug first so that the sprite is always drawn at 50Hz without frame skip
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesLadybug

	ldx #0					; set index for sprite 0 (ladybug)
	beq redrawSpritesEraseLoop

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; process enemy sprites (auto frame skip active) 
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesErase

	; --------------------------------------------------------------------------------------------------------------------------------------------
	; the following address is modified to point to either the upper or lower sprite index
	; --------------------------------------------------------------------------------------------------------------------------------------------

	ldx dummy8				; get sprite index so we can continue (address previously setup)

.redrawSpritesEraseLoop				; repeat

	lda spritesEraseY, x			; compare sprite y coordinate with upper/lower threshold
	cmp #upperLowerThreshold

.redrawSpritesEraseThreshold

	; --------------------------------------------------------------------------------------------------------------------------------------------
	; the following BNE instruction is overwritten by redrawSpritesLower/redrawSpritesUpper (see above) and replaced with
	; BCC for threshold test (sprite y < upperlower threshold)
	; or
	; BCS for threshold test (sprite y >= upperlower threshold)
	; --------------------------------------------------------------------------------------------------------------------------------------------

	bne redrawSpritesDraw			; if sprite is within the current half
						; (bne instruction replaced by bcc or bcs from above code)

	lda spritesErased, x			; and if sprite needs to be erased
	bne redrawSpritesDraw
	
.redrawSpritesEraseSprite

	jsr eraseSprite				; erase sprite and redraw background tile at tail end of sprite

	lda #&ff				; mark sprite as erased
	sta spritesErased, x

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw a sprite
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesDraw

	lda spritesDir, x			; if sprite is blanked then skip drawing it and go on to the next sprite
	and #spriteBlanking
	bne redrawSpritesNext

	lda spritesY, x				; compare sprite y coordinate with upper/lower threshold
	cmp #upperLowerThreshold		

.redrawSpritesDrawThreshold

	; --------------------------------------------------------------------------------------------------------------------------------------------
	; the following BNE instruction is overwritten by redrawSpritesLower/redrawSpritesUpper (see above) and replaced with
	; BCC for threshold test (sprite y < upperlower threshold)
	; or
	; BCS for threshold test (sprite y >= upperlower threshold)
	; --------------------------------------------------------------------------------------------------------------------------------------------

	bne redrawSpritesNext			; if sprite is within the current half
						; (bne instruction replaced by bcc or bcs from above code)
	
	lda spritesErased, x			; if theres still a pending erase caused by the sprite crossing upper/lower boundary
	beq redrawSpritesEraseSprite		; then we must ignore boundary and do an emergency erase otherwise part of
						; the sprite will be left on screen just below the upper/lower boundary crossing

	lda spritesX, x				; store sprite x for drawing (now) and erasing
	sta drawSpriteX
	sta spritesEraseX, x
	
	lda spritesY, x				; store sprite y for drawing (now) and erasing
	sta drawSpriteY
	sta spritesEraseY, x

	lda #0					; enable erasure for later
	sta spritesErased, x

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; calculate animation img offset
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda spritesImgFrame			; get current animation frame number, add to sprite img value and draw
	cmp #3					; unless its animation frame 3, use frame 1 instead. gives the sequence 0 1 2 1
	bne redrawSpritesAnimation
	lda #1

.redrawSpritesAnimation

	clc					; add animation offset to spriteImg
	adc spritesImg, x
	sta drawSpriteImg			; store sprite image number

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; calculate direction img offset
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesDirection

	lda spritesDir, x			; get sprite direction
	
	sta spritesEraseDir, x			; save direction for later erasure

						; use just the direction bits only
	and #moveUp or moveDown or moveLeft or moveRight

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

	jmp redrawSpritesDrawn

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; direction = up/left/right so draw sprite normal (not flipped)
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesNormal

	jsr drawSprite				; draw the sprite

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; sprite has been drawn so reduce counter by 1
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesDrawn

	dec redrawSpritesMax			; reduce the drawn sprites counter by 1

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; if we just processed ladybug then jump back and start processing enemies else continue processing enemies
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesNext

	cpx #0					; if we just processed ladybug
	beq redrawSpritesErase			; then loop back and start processing enemies

	dex					; else point index to next enemy sprite

	bne redrawSpritesCheckDrawn		; if index has past the end of the enemy sprite list
	ldx #spritesTotal - 1			; then index = spritesTotal - 1 (wrap around the sprite buffer)

.redrawSpritesCheckDrawn

	lda redrawSpritesMax			; if max sprites drawn then return saving current index so that the remaining sprites
	beq redrawSpritesUpdateIndex		; will be processed next frame (frameskip)

.redrawSpritesCheckDone	

	dec redrawSpritesCount			; until all sprites processed
	bne redrawSpritesEraseLoop
	
.redrawSpritesUpdateIndex

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

	jsr initMiddle				; initialize and draw empty playfield with timer, initialize all sprites as blanked and erased

	lda #palObject + palCyan		; set letters and hearts to cyan
	sta ulaPalette

	jsr updateSkullColor			; update the skull palette color (make sure skull is show as red when shield is enabled)

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw the level text and number
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawString				; draw level text
	equw screenAddr + 2 + 8 + 7 * chrColumn + 4 * chrRow
	equs colorGreen, "LEVEL ", colorMagenta, &ff
	
	lda level				; draw 2 digit level number
	jsr drawBcd
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw the vegetable image and value
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #72					; draw vegetable sprite
	sta drawSpriteX
	lda #5 * 8 + 2
	sta drawSpriteY
	jsr drawBonusItemVegetable

	jsr drawString				; draw vegetable score value in white
	equw screenAddr + 2 + 8 + 10 * chrColumn + 6 * chrRow
	equb colorWhite, &ff

	lda vegetableScore			; draw top 2 digits vegetable score
	jsr drawBcd

	lda #&00				; draw 00
	jsr drawBcd

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw vegetable name centered
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #pixelsYellow			; set text color to yellow
	sta drawChrColor

	lda vegetableImg			; index = vegetableImg * 4
	asl a
	asl a
	tax

	lda vegetableAddr, x			; get vegetable name screen draw address
	sta drawChrAddr
	lda vegetableAddr + 1, x
	sta drawChrAddr + 1

	lda vegetableAddr + 2, x		; get vegetable name string address
	sta drawTextAddr
	lda vegetableAddr + 3, x
	sta drawTextAddr + 1

	jsr drawText				; draw the vegetable name string

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; calculate the address to center the skulls
	;---------------------------------------------------------------------------------------------------------------------------------------------

						; start with center position

	lda #lo(screenAddr + 12 * chrColumn + 11 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 12 * chrColumn + 11 * chrRow)
	sta drawMapTileAddr + 1
	
	ldx levelSkulls				; get number of skulls

.drawLevelIntroSkullAddr			; repeat

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

.drawLevelIntroSkullImg				; repeat

	lda #mapTileSkull			; draw the skull and leave a gap
	jsr drawMapTileSpace

	dex					; until all skulls done
	bne drawLevelIntroSkullImg

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw the letters
	;---------------------------------------------------------------------------------------------------------------------------------------------

						; set screen position

	lda #lo(screenAddr + 9 * chrColumn + 14 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 9 * chrColumn + 14 * chrRow)
	sta drawMapTileAddr + 1
	
	ldx #2					; start at last letter

.drawLevelIntroLettersImg			; repeat

	lda levelLetters, x			; draw a letter from the list and leave a space
	jsr drawMapTileSpace

	dex					; until all letters done
	bpl drawLevelIntroLettersImg

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw the hearts
	;---------------------------------------------------------------------------------------------------------------------------------------------

						; set screen position

	lda #lo(screenAddr + 9 * chrColumn + 17 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 9 * chrColumn + 17 * chrRow)
	sta drawMapTileAddr + 1
	
	ldx #3					; 3 hearts to draw

.drawLevelIntroHeartsImg			; repeat

	lda #mapTileHeart			; draw a heart and leave a space
	jsr drawMapTileSpace

	dex					; until all hearts done
	bne drawLevelIntroHeartsImg

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; if shield != 0 then draw "SHIELD " in red and number of shield rounds remaining in white
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda shield				; if shields available then
	beq drawLevelIntroGoodLuck

	jsr drawString				; draw "SHIELD " in red and number of shields in white
	equw screenAddr + 2 + 7 * chrColumn + 20 * chrRow
	equs colorRed, "SHIELD ", colorWhite, &ff
	
	lda shield
	jsr drawBcd

	jmp drawLevelIntroWait

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; else draw draw "GOOD LUCK" in red
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawLevelIntroGoodLuck

	jsr drawString				; else draw "GOOD LUCK" in red
	equw screenAddr + 2 + 7 * chrColumn + 20 * chrRow
	equs colorRed, "GOOD LUCK", &ff
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; process sound and colors until level intro time expires
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawLevelIntroWait				; repeat

	jsr waitIntUpper			; wait for vsync interrupt and read analogue joystick (if enabled)
	jsr waitIntLower			; wait for timer1 interrupt and read analogue joystick (if enabled)

	jsr processSound			; process sound effects and music

	jsr updateSkullColor			; update the skull palette color

	jsr updateBonusColor			; update the bonus letters palette colors

	jsr drawScore				; draw score (1 digit per loop)

	lda pauseCounter			; until pause time expires
	bne drawLevelIntroWait

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawBonusGraphics				draw the bonus screen flowers from the list and also 3 skulls if its a special bonus
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusGraphicsList				; screen positions for 16 flowers

	equw screenAddr +  5 * chrRow +  1 * chrColumn
	equw screenAddr +  4 * chrRow +  3 * chrColumn
	equw screenAddr +  3 * chrRow +  7 * chrColumn
	equw screenAddr +  3 * chrRow +  9 * chrColumn
	equw screenAddr +  3 * chrRow + 12 * chrColumn
	equw screenAddr +  3 * chrRow + 14 * chrColumn
	equw screenAddr +  4 * chrRow + 18 * chrColumn
	equw screenAddr +  5 * chrRow + 20 * chrColumn
	equw screenAddr + 10 * chrRow +  1 * chrColumn
	equw screenAddr + 11 * chrRow +  3 * chrColumn
	equw screenAddr + 12 * chrRow +  7 * chrColumn
	equw screenAddr + 12 * chrRow +  9 * chrColumn
	equw screenAddr + 12 * chrRow + 12 * chrColumn
	equw screenAddr + 12 * chrRow + 14 * chrColumn
	equw screenAddr + 11 * chrRow + 18 * chrColumn
	equw screenAddr + 10 * chrRow + 20 * chrColumn

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw 3 skulls if its a special bonus screen
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusGraphics

	lda bonusSpecialActive			; if special bonus is active then set skull palette to red and draw some skulls in ladybugs path
	beq drawBonusGraphicsFlowers

	lda #palSkull + palRed			; set skull palette to red
	sta ulaPalette

	lda #lo(screenAddr + 9 * chrRow + 9 * chrColumn)
	sta drawMapTileAddr
	lda #hi(screenAddr + 9 * chrRow + 9 * chrColumn)
	sta drawMapTileAddr + 1

	ldx #3
	
.drawBonusGraphicsSkulls

	lda #mapTileSkull			; draw a skull and leave a space
	jsr drawMapTileSpace

	dex
	bne drawBonusGraphicsSkulls

	lda #mapTileSkull			; and place them in the tile map so ladybug can pass over and redraw them
	sta tileMap + 8 * 23 + 9
	sta tileMap + 8 * 23 + 11
	sta tileMap + 8 * 23 + 13

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw the flowers (same for all bonus screens)
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusGraphicsFlowers

	ldx #0					; index into flower positions
		
.drawBonusGraphicsLoop				; repeat

	lda drawBonusGraphicsList, x		; get low byte address from table
	sta drawMapTileAddr + 0
	inx

	lda drawBonusGraphicsList, x		; get high byte address from table
	sta drawMapTileAddr + 1
	inx

	jsr drawRandomFlower			; draw a single random flower

	cpx #16 * 2				; until all flowers drawn
	bne drawBonusGraphicsLoop

.drawBonusGraphicsExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawBonusScreen
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusScreen

	lda #bonusTime				; set display time
	sta pauseCounter

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; initialize playfieldMiddle
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr initMiddle				; initialize and draw empty playfield with timer, initialize all sprites as blanked and erased

	jsr drawBonusGraphics			; draw the bonus screen graphics

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; special bonus text, skulls, play special bonus music
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda bonusSpecialActive			; if special bonus active then draw special bonus text
	bne drawBonusScreenSpecialActive
	jmp drawBonusScreenExtra

.drawBonusScreenSpecialActive

	ldy #15					; draw congratulations on row 15
	jsr drawCongratulations

	bit highScoreChallenge			; if arcade mode
	bmi drawBonusScreenSpecialActiveChallenge

.drawBonusScreenSpecialActiveArcade

	jsr drawString
	equw screenAddr + 2 + 8 + 2 * chrColumn + 18 * chrRow
	equs colorMagenta, "YOU WIN ", colorWhite, &ff

	lda #(bonusSpecialScore and 15) + '0'
	jsr drawChr
	lda #&00
	jsr drawBcd
	lda #&00
	jsr drawBcd
	lda #'0'
	jsr drawChr

	jsr drawString
	equw screenAddr + 2 + 8 + 17 * chrColumn + 18 * chrRow
	equs colorMagenta, "PTS", &ff

	jsr drawString
	equw screenAddr + 2 + 8 + 4 * chrColumn + 20 * chrRow
	equs colorYellow, "AND", &ff

	jmp drawBonusScreenSpecialActiveShields

.drawBonusScreenSpecialActiveChallenge

	jsr drawString
	equw screenAddr + 2 + 8 * chrColumn + 18 * chrRow
	equs colorMagenta, "YOU WIN ", &ff

.drawBonusScreenSpecialActiveShields

	bit highScoreChallenge			; if challenge mode then draw alternate shield position
	bpl drawBonusScreenSpecialActiveShieldsArcade

	jsr swrDrawBonusScreenSpecialActiveShieldsChallenge
	jmp drawBonusScreenSpecialActiveExit

.drawBonusScreenSpecialActiveShieldsArcade

	if (bonusSpecialShield and 15) = 1
	{

	jsr drawString
	equw screenAddr + 2 + 8 + 8 * chrColumn + 20 * chrRow
	equb colorWhite, &ff

	lda #(bonusSpecialShield and 15) + '0'
	jsr drawChr
	
	jsr drawString
	equw screenAddr + 2 + 8 + 10 * chrColumn + 20 * chrRow
	equs colorYellow, "SHIELD", &ff

	lda #lo(screenAddr + 8 + 17 * chrColumn + 20 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 8 + 17 * chrColumn + 20 * chrRow)
	sta drawMapTileAddr + 1

	lda #mapTileSkull
	jsr drawMapTile

	}
	else
	{	

	jsr drawString
	equw screenAddr + 2 + 8 * chrColumn + 20 * chrRow
	equb colorWhite, &ff

	lda #(bonusSpecialShield and 15) + '0'
	jsr drawChr
	
	jsr drawString
	equw screenAddr + 2 + 10 * chrColumn + 20 * chrRow
	equs colorYellow, "SHIELDS", &ff

	lda #lo(screenAddr + 18 * chrColumn + 20 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 18 * chrColumn + 20 * chrRow)
	sta drawMapTileAddr + 1

	lda #mapTileSkull
	jsr drawMapTile

	}
	endif

.drawBonusScreenSpecialActiveExit

	lda #sfxMusicSpecial			; play special bonus music
	jsr playSound

	lda #animateLadybugBonusSpecial		; setup ladybug animation
	jmp drawBonusScreenAnimationSetup

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; extra bonus text, play extra bonus music
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusScreenExtra

	lda bonusExtraActive			; if extra bonus active
	beq drawBonusScreenDiamond

	ldy #15					; draw congratulations on row 15
	jsr drawCongratulations

	jsr drawString
	equw screenAddr + 2 + 7 * chrColumn + 18 * chrRow
	equs colorMagenta, "YOU WIN ", colorWhite, &ff

	lda #(bonusExtraLives and 15) + '0'
	jsr drawChr

	if (bonusExtraLives and 15) = 1
	{	

	jsr drawString
	equw screenAddr + 2 + 8 + 4 * chrColumn + 20 * chrRow
	equs colorYellow, "EXTRA LADY BUG", &ff

	}
	else
	{

	jsr drawString
	equw screenAddr + 2 + 4 * chrColumn + 20 * chrRow
	equs colorYellow, "EXTRA LADY BUGS", &ff

	}
	endif

	lda #sfxMusicExtra			; play extra bonus music
	jsr playSound

	lda #animateLadybugBonusExtra		; setup ladybug animation
	jmp drawBonusScreenAnimationSetup

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
	equw screenAddr + 2 + 8 + 2 * chrColumn + 16 * chrRow
	equs colorRed, "YOU DISCOVERED THE", &ff

	jsr drawString
	equw screenAddr + 2 + 8 + 4 * chrColumn + 18 * chrRow
	equs colorYellow, "DIAMOND GARDEN", &ff

	jsr drawString
	equw screenAddr + 2 + 2 * chrColumn + 20 * chrRow
	equs colorMagenta, "AND WIN ", colorWhite, &ff

	lda #bonusDiamondScore
	jsr drawBcd
	lda #&00
	jsr drawBcd
	lda #&00
	jsr drawBcd
	lda #'0'
	jsr drawChr

	jsr drawString
	equw screenAddr + 2 + 18 * chrColumn + 20 * chrRow
	equs colorMagenta, "PTS", &ff

	lda #sfxMusicExtra			; play extra bonus music
	jsr playSound

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup diamond animation for ladybug
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #animateLadybugBonusDiamond		; initialize a ladybug animation

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup diamond/special/extra animation for ladybug
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusScreenAnimationSetup

	jsr animateLadybugInitialize

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; wait for vsync interrupt and process
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusScreenWaitVsync			; repeat

	jsr waitIntUpper			; wait for vsync interrupt and read analogue joystick (if enabled)

	jsr animateLadybug			; update ladybug direction and frame counter
	
	jsr redrawSprites			; draw ladybug

	jsr moveSprites				; move ladybug

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; wait for timer1 interrupt and process
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr waitIntLower			; wait for timer1 interrupt and read analogue joystick (if enabled)

	jsr processSound			; process sound effects and music

	jsr redrawSprites			; draw ladybug

	jsr updateAnimationFrame		; update the animtion frame number

	jsr updateBonusColor			; update the bonus letters palette colors

	jsr drawScore				; draw score (1 digit per loop)

	lda pauseCounter			; until pause time expires
	bne drawBonusScreenWaitVsync

	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusScreenExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; draw congratulations text on specified row
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawCongratulations

	clc					; setup screen row / column to print text
	lda #lo(3 * chrColumn + 2 + 16)
	adc screenRowLo, y
	sta drawCongratulationsText + 0
	lda #hi(3 * chrColumn + 2 + 16)
	adc screenRowHi, y
	sta drawCongratulationsText + 1

	jsr drawString				; draw the congratulations

.drawCongratulationsText

	equw dummy16
	equs colorRed, "CONGRATULATIONS!", &ff

	rts



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

.animateLadybugBonusSpecialTable

ladybugBonusX		= 168			; starting position
ladybugBonusY		= 11

	equb 34, moveLeft
	equb 53, moveDown
	equb 87, moveLeft
	equb 87, moveRight
	equb 87, moveLeft
	equb 1, moveLeft + moveStop + spriteBlanking
	equb 0

	;---------------------------------------------------------------------------------------------------------------------------------------------

.animateLadybugBonusExtraTable

;ladybugBonusX		= 168			; starting position (already defined above)
;ladybugBonusY		= 11

	equb 34, moveLeft
	equb 53, moveDown
	equb 87, moveLeft
	equb 43, moveDown
	equb 36, moveLeft
	equb 56, moveDown
	equb 1, moveDown + moveStop + spriteBlanking
	equb 0

	;---------------------------------------------------------------------------------------------------------------------------------------------

.animateLadybugBonusDiamondTable

;ladybugBonusX		= 168			; starting position (already defined above)
;ladybugBonusY		= 11

	equb 34, moveLeft
	equb 53, moveDown
	equb 16, moveLeft
	equb 75, moveLeft + moveStop
	equb 16, moveRight
	equb 20, moveUp
	equb 87, moveLeft
	equb 33, moveUp
	equb 1, moveUp + moveStop + spriteBlanking
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
animateLadybugBonusSpecial	= 1		; special bonus animation index
animateLadybugBonusExtra	= 2		; extra bonus animation index
animateLadybugBonusDiamond	= 3		; diamond bonus animation index
animateLadybugNameReg		= 4		; high score animation index
animateLadybugMainMenu		= 5		; main menu animation index
animateLadybugInstructions	= 6		; instructions animation index

	;---------------------------------------------------------------------------------------------------------------------------------------------

.animateLadybugInitTable

	equw animateLadybugEntryTable
	equb ladybugEntryX
	equb ladybugEntryY
	
	equw animateLadybugBonusSpecialTable
	equb ladybugBonusX
	equb ladybugBonusY
	
	equw animateLadybugBonusExtraTable
	equb ladybugBonusX
	equb ladybugBonusY
	
	equw animateLadybugBonusDiamondTable
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
	lda (animateLadybugAddr), y		; get next byte and store in ladybug direction
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

	sta animateLadybugActive		; disable animation

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; updateBonusColor				update the special/extra/multiplier color palette
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateBonusColor

	lda vsyncCounter			; if vsyncCounter & 0x08 == 0 then use color set 0 else use color set 1
	and #%00001000
	bne updateBonusColorSet1
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; color set 0 = red/magenta, yellow/green, cyan/blue
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateBonusColorSet0

	lda #palSpecial0 + palRed		; set special colors to red/magenta
	sta ulaPalette
	lda #palSpecial1 + palMagenta
	sta ulaPalette

	lda #palExtra0 + palYellow		; set extra colors to yellow/green
	sta ulaPalette
	lda #palExtra1 + palGreen
	sta ulaPalette

	lda #palMultiplier0 + palCyan		; set multiplier colors to cyan/blue
	sta ulaPalette
	lda #palMultiplier1 + palBlue
	sta ulaPalette
	
	rts					; return

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; color set 1 = magenta/red, green/yellow, blue/cyan
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateBonusColorSet1

	lda #palSpecial0 + palMagenta		; set special colors to magenta/red
	sta ulaPalette
	lda #palSpecial1 + palRed
	sta ulaPalette

	lda #palExtra0 + palGreen		; set extra colors to green/yellow
	sta ulaPalette
	lda #palExtra1 + palYellow
	sta ulaPalette
	
	lda #palMultiplier0 + palBlue		; set multiplier colors to blue/cyan
	sta ulaPalette
	lda #palMultiplier1 + palCyan
	sta ulaPalette
	
	rts					; return
	



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; clearTileMap					fill tileMap with blank tile (23 * 23 tiles = &211 bytes)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.clearTileMap

	ldx #0					; start with index = 0
	
	lda #mapTileBlank			; use blank tile

.clearTileMapLoop

	sta tileMap + &000, x			; tileMap + &000 to tileMap + &0ff
	sta tileMap + &100, x			; tileMap + &100 to tileMap + &1ff
	sta tileMap + &111, x			; tileMap + &111 to tileMap + &210

	inx					; bump index and loop back until fill completed
	bne clearTileMapLoop
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawString					draw text string terminanated by byte with bit 7 set
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	addr, data		addr is read from memory following the jsr
;						format is
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

	ldy #0					; get screen address
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

	jsr drawText				; draw the text until &ff byte string terminator
	
	ldy drawStringSaveY			; restore register

	lda drawTextAddr + 1			; push string terminator address (return address - 1 as the 6502 adds 1 to the popped address)
	pha
	lda drawTextAddr
	pha
	
	rts					; return terminator address + 1 to continue



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawText					draw text string terminanated by byte with bit 7 set (not drawn)
;						if text contains value 0-15 then select draw color
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	drawTextAddr		points to first character of string
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			drawChrAddr		points to next chr position on screen
;			A			destroyed
;			X			preserved
;			y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawText

	stx drawTextSaveX			; preserve register
	sty drawTextSaveY			; preserve register

	ldy #0					; start at first text chr

.drawTextLoop

	lda (drawTextAddr), y			; get chr from text string

	cmp #&ff				; if string terminator found
	beq drawTextAdjustAddr			; then exit

	and #%01111111				; remove bit 7
	
	cmp #16					; if chr value is 0-15 then select chr color
	bcs drawTextChr

.drawTextColor

	tax
	lda drawTextChrColor, x
	sta drawChrColor
	jmp drawTextNext

.drawTextChr

	jsr drawChr				; else draw chr

.drawTextNext

	iny					; point to next chr and repeat
	bne drawTextLoop

.drawTextAdjustAddr

	tya					; add text length to addr
	clc
	adc drawTextAddr
	sta drawTextAddr
	bcc drawTextExit
	inc drawTextAddr + 1

.drawTextExit

	ldx drawTextSaveX			; restore register
	ldy drawTextSaveY			; restore register

	rts					; return
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; pixel mask values for chr colors
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawTextChrColor

	equb pixelsBlack			; black
	equb pixelsRed				; red
	equb pixelsGreen			; green
	equb pixelsYellow			; yellow
	equb pixelsBlue				; blue
	equb pixelsMagenta			; magenta
	equb pixelsCyan				; cyan
	equb pixelsWhite			; white
	equb pixelsMultiplier0			; multiplier0
	equb pixelsMultiplier1			; multiplier1
	equb pixelsSpecial0			; special0
	equb pixelsSpecial1			; special1
	equb pixelsExtra0			; extra0
	equb pixelsExtra1			; extra1
	equb pixelsSkull			; skull
	equb pixelsObject			; object



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; processSound					process sound effect/music tables and send data to psg
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
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

.processSoundLoop				; repeat

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

	lda optionSound				; if sound mode = 0 (off) then skip write to psg
	beq processSoundNextByte

	cmp #2					; if sound mode = 2 (demo on) then write to psg
	beq processSoundWritePsg

	lda demoMode				; sound mode = 1 (demo off) if demo mode then skip write to psg
	bne processSoundNextByte	

.processSoundWritePsg

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

	bpl processSoundLoop			; until all channels processed

.processSoundExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; playSoundSilence				shut down all sound effect types and psg channels
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.playSoundSilence

	lda #0					; shut down all sound effect types
	sta soundTimers + 0
	sta soundTimers + 1
	sta soundTimers + 2
	sta soundTimers + 3
	sta soundTimers + 4
	sta soundTimers + 5

	lda #&9f				; start with psg first channel zero volume (silence)
	
.playSoundSilenceLoop				; repeat

	pha					; save current channel
	
	jsr psgWrite				; silence channel
	
	pla					; get current channel
	
	clc					; bump value to next channel
	adc #&20
	
	bmi playSoundSilenceLoop		; until all channels done
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; .playSoundTimer				play the enemy timer sound at the selected volume
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------


.playSoundTimer

	lda optionTimerVolume			; if optionTimerVolume = 0
	bne playSoundTimerVolume

	rts					; then return
	
.playSoundTimerVolume

	clc					; else add optionTimerVolume to #sfxTimerLow - 1 to select the required volume Low/Medium/High
	adc #sfxTimerLow - 1

	; contine down to play sound		; play timer sound effect and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; playSound					send sound data to soundEffect to trigger sound
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
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
	lda sfxAddrTable + 1, y
	sta playSoundAddr + 1

	ldy #0					; get sound effect type from table
	lda (playSoundAddr), y

	tay					; y = effect type
	
	asl a					; x = effect type index
	tax

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; type 0 high priority (music)
	;---------------------------------------------------------------------------------------------------------------------------------------------

	cpy #0					; if type = 0 (music)
	bne playSoundLowPriority
	
	jsr playSoundSilence			; kill any current sounds

	jmp playSoundNow			; play the music

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; type 1 to 5 low priorty (sound effects)
	;---------------------------------------------------------------------------------------------------------------------------------------------

.playSoundLowPriority

	lda soundTimers + 0			; if type 0 (music) is currently active
	beq playSoundCheckType3

	lda playSoundSaveA			; then only allow sfxMunch
	cmp #sfxMunch
	beq playSoundNow
	bne playSoundExit

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check if type 3 is active (sfxObject/sfxSkull) and if so then skip playing turnstile or timer sounds if requested
	;---------------------------------------------------------------------------------------------------------------------------------------------

.playSoundCheckType3

	lda soundTimers + 3			; if sound type 3 active (sfxObject/sfxSkull)
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
	
	cmp #sfxObject				; if sfxObject then shut down types 1 and 4 (sfxTimer, sfxTurnstile)
	beq playSoundDisable14
	
	cmp #sfxSkull				; if sfxSkull then shut down types 1 and 4 (sfxTimer, sfxTurnstile)
	beq playSoundDisable14

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; copy sound data address and set timer to 1 to instantly trigger start of sound
	;---------------------------------------------------------------------------------------------------------------------------------------------

.playSoundNow

	clc					; copy sound data address to channel soundPtr
	lda playSoundAddr
	adc #1
	sta soundAddrPtr, x
	lda playSoundAddr + 1
	adc #0
	sta soundAddrPtr + 1, x

	lda #1					; set sound type soundTimer to 1 to trigger sound on the next game loop
	sta soundTimers, y

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; restore registers and exit
	;---------------------------------------------------------------------------------------------------------------------------------------------

.playSoundExit

	ldy playSoundSaveY			; restore registers
	ldx playSoundSaveX

	rts					; return

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; disable sound types 1 and 4 (sfxTimer, sfxTurnstile)
	;---------------------------------------------------------------------------------------------------------------------------------------------

.playSoundDisable14

	lda #0

	sta soundTimers + 1			; disable type 1 (sfxTimer)
	sta soundTimers + 4			; disable type 4 (sfxTurnstile)

	beq playSoundNow			; play sound and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; checkForObject				check if ladybug is over the bonus vegetable/diamond
;						also read tile under ladybug and handle any objects found (dots, hearts, letters, skulls)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.bonusBitsMultiplierFlags			; multiplier bit flags

	equb %11111011, %11111101, %11111110	; x2 x3 x5

.objectScore 

	equb &10, &80, &30			; object multiplier score cyan 100, red 800, yellow 300

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObject

	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; check if vegetable/diamond is active and that ladybug is in the center box
	;-------------------------------------------------------------------------------------------------------------------------------------------------

	lda bonusItemActive			; if bonus item is active
	beq checkForObjectTile

	lda spritesX + 0			; and ladybug is in the center box
	cmp #centerBoxX
	bne checkForObjectTile
	lda spritesY + 0
	cmp #centerBoxY
	bne checkForObjectTile
	
	lda #0					; disable the bonus item
	sta bonusItemActive
	
	lda bonusDiamondEnable			; if diamond is enabled then
	beq checkForObjectVegetableScore

	lda level				; if level >= bonusDiamondLevel
	cmp #bonusDiamondLevel
	bcc checkForObjectVegetableScore

	lda #&ff				; enable diamond bonus
	sta bonusDiamondActive
	
	lda #letterBonusTime			; pause ladybug and enemies
	sta pauseLadybug
	sta pauseEnemy

	lda #sfxMusicLetters			; play bonus letters music and return
	jmp playSound



	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; vegetable score
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectVegetableScore			; we got here so diamond is not active, its a vegetable so we

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
	
.checkForObjectTile				; we got here so ladybug is not in the center box so we

	jsr offsetDrawMapTileAddr		; adjust drawMapTileAddress to be location underneath ladybug center

	ldy #24					; read tile at ladybug location
	lda (tileMapAddr), y

	cmp #mapTileDot				; if tile = dot then use dot function
	beq checkForObjectDot
	
	cmp #mapTileSkull			; if tile = skull then use skull function
	beq checkForObjectSkull

	cmp #mapTileHeart			; if tile = heart then use heart function
	beq checkForObjectHeart

	cmp #mapTileS				; if tile >= mapTileS and < mapTileR + 1 (letter tiles S P E C I A L X T R)
	bcc checkForObjectTileExit
	
	cmp #mapTileR + 1
	bcc checkForObjectLetter		; then use letter function

.checkForObjectTileExit

	rts					; return



	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; dot tile
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectDot

	lda #mapTileBlank			; replace dot with blank tile (no need to erase dot from screen as ladybug erase will do that)
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
	bne checkForObjectTileExit

	lda #mapTileBlank			; replace skull with blank tile
	sta (tileMapAddr), y
	
	lda #mapTileBlankObj			; erase skull from screen
	jsr drawMapTile

	jmp ladybugKill				; kill ladybug and exit



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
	and bonusBits + 0
	sta bonusBits + 0

	jsr drawPlayfieldUpperBonus		; update playfield upper to show the multiplier changes

	inc scoreMultiplier			; inc the score multiplier

	; continue down to add heart value to score



	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; object score
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectScore

	ldx objectMode				; add score value for object * multiplier (cyan = 100, red = 800, yellow = 300)
	lda objectScore, x
	jsr addScoreMultiply

	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; if object was not cyan then disable the the possibility of getting a diamond bonus
	;-------------------------------------------------------------------------------------------------------------------------------------------------

	cpx #objectModeCyan			; if the object was not cyan
	beq checkForObjectScoreDisplay

	lda bonusDiamondEnable			; and if the diamond bonus was enabled
	beq checkForObjectScoreDisplay

	lda #0					; then disable the diamond bonus
	sta bonusDiamondEnable

	jsr swrDrawPlayfieldLowerDiamond	; and remove it from the lower playfield

	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; display the object score at the object location
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectScoreDisplay

	jsr displayObjectScore			; setup object score display

	lda spritesDir + 0			; blank ladybug sprite
	ora #spriteBlanking
	sta spritesDir + 0

	lda #objectTime				; pause ladybug
	sta pauseLadybug

	cmp pauseEnemy				; only set enemy pause time if its currently less than object pause time
	bcc checkForObjectExit			; to prevent a vegetable pause time being overwriiten with the shorter object pause time
	sta pauseEnemy

.checkForObjectExit

	lda #sfxObject				; play object sound
	jsr playSound

	ldx checkForObjectSaveX			; restore X register

	rts					; return



	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; letter tile				replace letter in map with blank tile
	;					levelEdibles--
	;					update bonus bits
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
	jmp checkForObjectLetterUpdate

	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; yellow letter mode
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectLetterYellow

	cpx #objectModeYellow			; if objectMode = objectModeYellow
	bne checkForObjectScore

	tax					; get yellow letter bit value and highlight it
	lda objectLetterBitsYellow, x
	and bonusBits + 0
	sta bonusBits + 0
	; continue down to update letter bit

	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; update upper letter display
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectLetterUpdate

	jsr drawPlayfieldUpperBonus		; update upper bonus display

	jmp checkForObjectScore			; add the object score and return



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
	equb %11111111				; "L" no effect in yellow mode
	equb %10111111				; "X" clear bit 6 of bonusBits + 0
	equb %11011111				; "T" clear bit 5 of bonusBits + 0
	equb %11101111				; "R" clear bit 4 of bonusBits + 0



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; displayObjectScore				setup the img and position to display the object score
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.displayObjectScore

	lda spritesX + 0			; get ladybug position to the nearest 16 pixels in x and y, offset it by 8, 10 for correct positioning
	and #&f0
	ora #8
	sta objectScoreX
	lda spritesY + 0
	and #&f0
	ora #10
	sta objectScoreY

	lda objectMode				; calculate which object score image to display based on object mode and multiplier
	asl a					; objectScoreImg = objectMode * 4 + scoreMultiplier + ImgPoints
	asl a
	adc scoreMultiplier			; (carry is clear here, no need for clc)
	adc #ImgPoints
	sta objectScoreImg

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; ladybugKill					erase all enemies
;						disable center bonus item
;						remove the possibility of getting a diamond bonus
;						remove diamond from lower playfield
;						erase center box item
;						enable ladybug death animation
;						reduce lives by 1
;						pause ladybug and enemies
;						play death music
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.ladybugKill

	ldx #spritesTotal - 1			; erase all enemies
	lda #spriteBlanking

.ladybugKillLoop

	sta spritesDir + 0, x
	dex
	bne ladybugKillLoop

	lda #0					; disable bonus item
	sta bonusItemActive

	sta bonusDiamondEnable			; remove the possibility of getting a diamond bonus

	jsr swrDrawPlayfieldLowerDiamond	; remove diamond from lower playfield

	lda #centerBoxX				; erase center box (remove vegetable or diamond)
	sta drawSpriteX
	lda #centerBoxY
	sta drawSpriteY
	jsr eraseBlock

	lda #&ff				; enable death animation
	sta ladybugDeathEnable

	sed					; bcd mode

	sec					; reduce lives by 1
	lda lives
	sbc #1
	sta lives

	cld					; binary mode

	lda #ladybugDeathTime			; pause ladybug and enemies
	sta pauseLadybug
	sta pauseEnemy

	lda #sfxMusicDeath			; play ladybug death music and return
	jmp playSound



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; updateLadybug					set direction of ladybug if requested direction is valid
;						handle objects under ladybug
;						handle turnstile movement (rewrite background map tile id's only, drawing is handled by mainloop)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; return if ladybug is paused or entry animation is enabled
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybug

	lda pauseLadybug			; if ladybug movement is paused then return
	bne updateLadybugReturn

	lda ladybugEntryEnable			; if ladybug entry movement is enabled then return
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

	and #moveStop eor &ff			; save current direction and blanking but with movement enabled
	sta updateLadybugOldDir			; ( used later to slide ladybug when turned early for junction or turnstile )

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
	
	lda spritesY + 0			; save ladybug y for map and screen address conversion
	sta spriteToAddrY

	and #15					; create ladybug grid y flag, on grid = 0, off grid != 0
	eor #8
	sta updateLadybugGridY
	
	jsr spriteToAddr			; convert ladybug xy to tileMapAddr and drawTileAddr

	jsr checkForObject			; check for object under ladybug (dot/heart/letter/skull) and do required action (multiplier/highlight/points/death)

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; check inputs and store selected directions
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugInput

	jsr swrDemo				; replace player inputs with demo inputs (if enabled)

	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugInputStart

	lsr playerInput				; discard start/fire input

	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugInputLeft

	lsr playerInput				; get left input

	bcc updateLadybugInputDown		; if left was requested then

	ldx #moveLeft				; set direction left

	lda updateLadybugGridY			; if ladybug y is on exact grid (left direction allowed) then
	bne updateLadybugInputLeftAlign

	stx updateLadybugNewDirX		; set new direction to left
	beq updateLadybugInputDown		; check for down
	
.updateLadybugInputLeftAlign

	lda updateLadybugOldDir			; else use old direction to align ladybug with grid
	sta updateLadybugNewDirY

	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugInputDown

	lsr playerInput				; get down input

	bcc updateLadybugInputUp		; if down was requested then

	ldx #moveDown				; set direction down

	lda updateLadybugGridX			; if ladybug x is on exact grid (down direction allowed) then
	bne updateLadybugInputDownAlign

	stx updateLadybugNewDirY		; set new direction to down
	beq updateLadybugInputUp		; check for up

.updateLadybugInputDownAlign

	lda updateLadybugOldDir			; else use old direction to align ladybug with grid
	sta updateLadybugNewDirX

	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugInputUp

	lsr playerInput				; get up input

	bcc updateLadybugInputRight		; if up was requested then

	ldx #moveUp				; set direction up

	lda updateLadybugGridX			; if ladybug y is on exact grid (up direction allowed) then
	bne updateLadybugInputUpAlign

	stx updateLadybugNewDirY		; set new direction to up
	beq updateLadybugInputRight		; check for right

.updateLadybugInputUpAlign

	lda updateLadybugOldDir			; else use old direction to align ladybug with grid
	sta updateLadybugNewDirX
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugInputRight

	lsr playerInput				; get right input

	bcc updateLadybugCheckGrid		; if right was requested

	ldx #moveRight				; set direction right

	lda updateLadybugGridY			; if ladybug y is on exact grid (right direction allowed) then
	bne updateLadybugInputRightAlign

	stx updateLadybugNewDirX		; set new x direction to right
	beq updateLadybugCheckGrid		; go check for grid alignment

.updateLadybugInputRightAlign

	lda updateLadybugOldDir			; else use old direction to align ladybug with grid
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

	ora spritesDir + 0			; and combine with blanking bit
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

	lda updateLadybugSave			; if saved tile is not a turnstile then return
	and #wallSolid
	cmp #wallTurnstile
	bne updateLadybugExit

	lda updateLadybugSave			; get tile

	ldy #0

	cmp #mapTileTurnstileU + wallTurnstile	; if up tile found then y = 0 (up)
	beq updateLadybugTurnstilePush

	iny
	cmp #mapTileTurnstileD + wallTurnstile	; if down tile found then y = 1 (down)
	beq updateLadybugTurnstilePush

	iny
	cmp #mapTileTurnstileL + wallTurnstile	; if left tile found then y = 2 (left)
	beq updateLadybugTurnstilePush

	iny					; remaining tile must be right, y = 3 (right)

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

	beq updateLadybugTurnstileExit		; (beq used as branch always)

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

	ldy mapDir, x				; get tile in front of ladybug from supplied direction in x
	lda (tileMapAddr), y
	tay

	and #wallSolid				; return with 0 if solid wall or not 0 if path/turnstile
	eor #wallSolid

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawTurnstile					draw a vertical or horizontal turnstile if required
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	drawTurnstileAddr	screen address for top left corner of 3x3 turnstile block or high byte >= &80 for no draw
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

	clc					; tile offset 1,0
	lda drawMapTileAddr
	adc #lo(chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrColumn)
	sta drawMapTileAddr + 1

	lda #mapTileTurnstileU			; draw up tile
	jsr drawMapTile
	
	clc					; tile offset 0,1
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
	
	clc					; tile offset 1,2
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

	clc					; tile offset 1,0
	lda drawMapTileAddr
	adc #lo(chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrColumn)
	sta drawMapTileAddr + 1

	lda #mapTileBlank			; draw blank
	jsr drawMapTile
	
	clc					; tile offset 0,1
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
	
	clc					; tile offset 1,2
	lda drawMapTileAddr
	adc #lo(chrRow - 2 * chrColumn)
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	adc #hi(chrRow - 2 * chrColumn)
	sta drawMapTileAddr + 1

	lda #mapTileBlank			; draw blank tile
	jmp drawMapTile				; and exit



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; offsetDrawMapTileAddr				offset drawMapTileAddr to 1 character and 1 row to be under center of sprite
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
; entry parameters	spriteToAddrX		sprite x position
;			spriteToAddrY		sprite y position
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			tileMapAddr		tile map address for top left corner of sprite
;			drawTileAddr		screen tile address for top left corner of sprite
;			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

spriteToAddrOffset	= 4			; correction factor for center of tile
						; ( tiles are internally 8x8 pixels so offset of 4 puts it at the center)

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
; readKey					place key onto slow bus port a and read key status into player input bits
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	A			key matrix scan code
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;			playerInput		shifted left and key status placed into bit 0
;-----------------------------------------------------------------------------------------------------------------------------------------------------


.readKey

	sta via1PortA				; select key

	lda via1PortA				; read key status (bit 7)

	asl a					; shift bit 7 into carry
	rol playerInput				; and shift carry into playerInput bits
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; joystickAnalogue				read analogue joystick values, convert to player input bits
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
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

	lda dummy16				; read value from adc and save (addr setup by relocate.asm)
	sta joystickAnalogueSave
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.joystickAnalogueControlRead

	lda dummy16				; read current channel and flip bit to select other channel (addr setup by relocate.asm)
	and #%00000001
	eor #%00000001

.joystickAnalogueControlWrite

	sta dummy16				; start new adc conversion (addr setup by relocate.asm)

	lsr a					; put channel (0 or 1) into carry (note inverted channel from previous eor instruction)

	rol joystickAnalogueSave		; rotate analogue value so that
	rol joystickAnalogueSave		; bit 2 contains the channel (inverted)
	rol joystickAnalogueSave		; bits 1,0 contain bits 7,6 of the analogue value

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

.vegetableEnd

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; vegetable name screen address (centered) and name string address
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.vegetableAddr

	equw screenAddr + 2 + 8 * chrRow + ((((24 - (vegetableEggplant - vegetableCucumber)) * chrColumn) / 2) and &fff8)
	equw vegetableCucumber

	equw screenAddr + 2 + 8 * chrRow + ((((24 - (vegetableCarrot - vegetableEggplant)) * chrColumn) / 2) and &fff8)
	equw vegetableEggplant

	equw screenAddr + 2 + 8 * chrRow + ((((24 - (vegetableRadish - vegetableCarrot)) * chrColumn) / 2) and &fff8)
	equw vegetableCarrot

	equw screenAddr + 2 + 8 * chrRow + ((((24 - (vegetableParsley - vegetableRadish)) * chrColumn) / 2) and &fff8)
	equw vegetableRadish

	equw screenAddr + 2 + 8 * chrRow + ((((24 - (vegetableTomato - vegetableParsley)) * chrColumn) / 2) and &fff8)
	equw vegetableParsley

	equw screenAddr + 2 + 8 * chrRow + ((((24 - (vegetablePumpkin - vegetableTomato)) * chrColumn) / 2) and &fff8)
	equw vegetableTomato

	equw screenAddr + 2 + 8 * chrRow + ((((24 - (vegetableBambooShoot - vegetablePumpkin)) * chrColumn) / 2) and &fff8)
	equw vegetablePumpkin

	equw screenAddr + 2 + 8 * chrRow + ((((24 - (vegetableJapaneseRadish - vegetableBambooShoot)) * chrColumn) / 2) and &fff8)
	equw vegetableBambooShoot

	equw screenAddr + 2 + 8 * chrRow + ((((24 - (vegetableMushroom - vegetableJapaneseRadish)) * chrColumn) / 2) and &fff8)
	equw vegetableJapaneseRadish

	equw screenAddr + 2 + 8 * chrRow + ((((24 - (vegetablePotato - vegetableMushroom)) * chrColumn) / 2) and &fff8)
	equw vegetableMushroom

	equw screenAddr + 2 + 8 * chrRow + ((((24 - (vegetableOnion - vegetablePotato)) * chrColumn) / 2) and &fff8)
	equw vegetablePotato

	equw screenAddr + 2 + 8 * chrRow + ((((24 - (vegetableChineseCabbage - vegetableOnion)) * chrColumn) / 2) and &fff8)
	equw vegetableOnion

	equw screenAddr + 2 + 8 * chrRow + ((((24 - (vegetableTurnip - vegetableChineseCabbage)) * chrColumn) / 2) and &fff8)
	equw vegetableChineseCabbage

	equw screenAddr + 2 + 8 * chrRow + ((((24 - (vegetableGreenChilli - vegetableTurnip)) * chrColumn) / 2) and &fff8)
	equw vegetableTurnip

	equw screenAddr + 2 + 8 * chrRow + ((((24 - (vegetableCelery - vegetableGreenChilli)) * chrColumn) / 2) and &fff8)
	equw vegetableGreenChilli

	equw screenAddr + 2 + 8 * chrRow + ((((24 - (vegetableSweetPotato - vegetableCelery)) * chrColumn) / 2) and &fff8)
	equw vegetableCelery

	equw screenAddr + 2 + 8 * chrRow + ((((24 - (vegetableHorseradish - vegetableSweetPotato)) * chrColumn) / 2) and &fff8)
	equw vegetableSweetPotato

	equw screenAddr + 2 + 8 * chrRow + ((((24 - (vegetableEnd - vegetableHorseradish)) * chrColumn) / 2) and &fff8)
	equw vegetableHorseradish



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; font ascii
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.fontBin
	skip 0					; show this address in listing

	incbin "bin/font.bin"			; main font 6x6 pixels (1 bit per pixel)

.fontBinEnd
	skip 0					; show this address in listing



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; small font used for vegetable bonus
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.miniFontBin
	skip 0					; show this address in listing
	
	incbin "bin/fontBonus.bin"		; 4x8 pixels (4 bits per pixel)

.miniFontBinEnd
	skip 0					; show this address in listing



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; mapTile data					maze tiles used in map
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mapTileBin
	skip 0					; show this address in listing

	incbin "bin/tiles.bin"			; maze map tiles 6x8 pixels (4 bits per pixel)

.mapTileBinEnd
	skip 0					; show this address in listing



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; objectTile data				object tiles used in map
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.objectTileBin
	skip 0					; show this address in listing

	incbin "bin/objects.bin"		; maze object tiles hearts, letters, skulls 8x8 pixels (4 bits per pixel)

.objectTileBinEnd
	skip 0					; show this address in listing



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; extra tile data				extra tiles used on screen only (logo, flowers etc, not stored in background map)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.extraTileBin
	skip 0					; show this address in listing

	incbin "bin/extra.bin"			; extra tiles 6x8 pixels (4 bits per pixel)

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

	for n, 0, 22				; generate column offset high byte lookup table
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
; sound effects and music tables
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	include "asm/soundTables.asm"



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; sprite image base and address tables
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.spriteBaseImg

	for n, 0, 9				; img index for base sprite of ladybug, the 8 enemies and the angel
	equb n * 15 + ImgLadybugEnemiesAngel
	next

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; generate lo address table for the 10x10 pixel bonus vegetable/diamond/points sprites
	;---------------------------------------------------------------------------------------------------------------------------------------------

.spriteImgAddrLo

	for n, sprite10x10Bin, sprite10x10BinEnd - 1, sprite10x10Bytes
	equb lo(n)
	next

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; generate lo address table for the 10x14 pixel ladybug, enemies and angel sprites
	;---------------------------------------------------------------------------------------------------------------------------------------------

	for n, ladybugBin, spriteBinEnd - 1, spriteTileBytes
	equb lo(n)
	next

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; generate lo address table for the 10x7 diamond tiles
	;---------------------------------------------------------------------------------------------------------------------------------------------

	equb lo(diamondBin)
	equb lo(diamondBin + diamondTileBytes)

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; generate hi address table for bonus vegetable/diamond/point tiles
	;---------------------------------------------------------------------------------------------------------------------------------------------

.spriteImgAddrHi

	for n,  sprite10x10Bin, sprite10x10BinEnd - 1, sprite10x10Bytes
	equb hi(n)
	next

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; generate hi address table for ladybug, enemies and angel sprites
	;---------------------------------------------------------------------------------------------------------------------------------------------

	for n, ladybugBin, spriteBinEnd - 1, spriteTileBytes
	equb hi(n)
	next

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; generate hi address table for the 10x7 diamond tiles
	;---------------------------------------------------------------------------------------------------------------------------------------------

	equb hi(diamondBin)
	equb hi(diamondBin + diamondTileBytes)



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; map data files loaded here by loader.asm
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mapData					; 21 rows of 11 bytes (left half of maze map only, right half generated by mirror flip)

.map1	skip 21*11
.map2	skip 21*11
.map3	skip 21*11



;-----------------------------------------------------------------------------------------------------------------------------------------------------
;
.programEnd					; end of main program
	skip 0					; show this address in listing
;
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print


