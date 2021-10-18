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
; thanks to everyone @ stardot forums for their kind words and support
;-----------------------------------------------------------------------------------------------------------------------------------------------------

; note after cmp/cpx/cpy			; why i cannot remember this :P
; use bcc for register < compare value
; use bcs for register >= compare value



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; show memory usage
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	include "memoryusage.asm"



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; game constants
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	include "constants.asm"



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; generate !boot file
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	include "!boot.asm"



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; game loader
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	include "loader.asm"



	include "config.asm"



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; ladybug main program
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " ladybug.asm"
	print "----------------------------------------------------"
	print



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; debugging stuff
;-----------------------------------------------------------------------------------------------------------------------------------------------------

;debugRaster		= true			; enable raster timing bars
debugRaster		= false

;debugCoordinates	= true			; display ladybug coordinates
debugCoordinates	= false

;debugSpriteTest		= true			; display sprite animations
debugSpriteTest		= false

;-----------------------------------------------------------------------------------------------------------------------------------------------------

;debugLevel		= &18			; start on this level
debugLevel		= 0

;debugInvulnerable	= true			; ladybug is invulnerable to harm
debugInvulnerable	= false

;debugZeroHighScore	= true			; set highScore to 0 at start of new game (any score will register as a high score)
debugZeroHighScore	= false

;debugInstantHighScore	= true			; end the game instantly with a high score to quicky test name registration
debugInstantHighScore	= false

;debugDiamondOverride	= 1			; override level of diamond bonus or 0 to use the default value
debugDiamondOverride	= 0

;debugDiamondBonus	= true			; enable instant diamond bonus garden for quick testing
debugDiamondBonus	= false

;debugBonus		= true			; enable all special and extra letters for quick testing of gardens
debugBonus		= false

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; page0000 variables
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org page0000

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.vsyncCounter		skip 1			; 50Hz vsync counter (counts up and wraps over from 255 to 0)
.pauseCounter		skip 1			; 25Hz pause counter (counts down every 2 vsyncs)
.screenHalf		skip 1			; &00 = upper scanlines 0 to 155, &ff = lower scanlines 156 to 311
						; for game playfield sprite y, upper y=0 to 91, lower y=92 to 183

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; game constants, variables and flags
;-----------------------------------------------------------------------------------------------------------------------------------------------------

ladybugEnemyRange	= 6			; range allowed between enemy and ladybug to detect as a hit

frame			= 50			; 1 second = 50 * 50Hz vsync frames
pause			= 25			; 1 second pause = 25 * 25Hz

escTime			= frame * 2.00		; hold esc for 2.00 seconds to quit game

levelIntroTime		= pause * 3.00		; 3.00 seconds level intro screen time

ladybugEntryTime	= pause * 2.40		; 2.40 seconds timer pause while ladybug enters the game

ladybugDeathTime	= 255			; maximum pause time, ladybug is unpaused at end of death sequence/intro
ladybugDeathFlashTime	= frame * 0.50		; ladybug flashes for 0.50 seconds during death
ladybugDeathWaitTime	= frame * 0.80		; ladybug waits as an angel 0.80 seconds before floating

endLevelTime		= pause * 1.00		; 1.00 seconds pause during end of level sound

vegetableLadybugTime	= pause * 0.60		; 0.60 second ladybug pause when ladybug collects vegetable
vegetableEnemyTime	= pause * 6.50		; 6.50 second enemy pause when ladybug collects vegetable

objectTime		= pause * 0.64		; 0.64 second ladybug and enemy pause while collecting letter and heart object

gameOverTime		= pause * 3.00		; 3.00 seconds game over screen

nameRegTimer		= frame * 1.00		; 1.00 second timer tick speed during name entry

letterBonusTime		= pause * 1.50		; 1.50 seconds pause during letter bonus sound

bonusTime		= pause * 7.00		; 7.00 seconds special/extra/diamond bonus screen time

specialBonusScore	= &02			; special bonus 200,000 points
specialBonusShield	= &06			; special bonus skull shield for 6 levels

extraBonusLives		= &02			; extra bonus 2 more lives

diamondBonusScore	= &10			; diamond bonus score 1,000,000 points

objectModeCyanTime	= pause * 7.00		; 7.00 second cyan objects
objectModeRedTime	= pause * 0.60		; 0.60 second red objects
objectModeYellowTime	= pause * 2.50		; 2.50 second yellow objects

objectModeCyan		= 0			; object modes
objectModeRed		= 1
objectModeYellow	= 2

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.escCounter		equb 0			; escape key counter (times how long esc is pressed)

.score			skip 3			; player score (BCD) last digit always 0 and not stored
.highScore		skip 3			; high score (BCD) last digit always 0 and not stored

.lives			equb defaultLives	; number of lives

.ladybugEntryEnable	skip 1			; enable ladybug entry animation
.ladybugDeathEnable	skip 1			; enable ladybug death animation
.ladybugDeathAnimationIndex
			skip 1			; index into ladybug death animation table

.bonusBits		skip 2			; special, extra, x2 x3 x5 (1 bit each)
.bonusBitsTemp		skip 2			; storage for working on bonus bits

.bonusSpecialActive	skip 1			; special bonus is active if != 0
.bonusExtraActive	skip 1			; extra bonus is active if != 0
.bonusDiamondActive	skip 1			; diamond bonus is active if != 0
.bonusDiamondEnable	skip 1			; diamond bonus is enabled if != 0
bonusDiamondImg		= 18			; diamond image number

if debugDiamondOverride == false
bonusDiamondLevel	= 6			; level for releasing the diamond (if diamond bonus is enabled)
else
bonusDiamondLevel 	= debugDiamondOverride	; use debugDiamondOverride level if enabled
endif

.scoreMultiplier	skip 1			; multiplier 0=x1, 1=x2, 2=x3, 3=x5

.vegetableImg		skip 1			; vegetable image number for current level
.vegetableActive	skip 1			; vegetable bonus active if != 0
.vegetableScore		equb &10		; vegetable score value (bcd)
.vegetableScoreActive	skip 1			; vegetable score displayed if != 0

.objectScoreImg		skip 1			; object score img active if != 0
.objectScoreX		skip 1			; object score x position
.objectScoreY		skip 1			; object score y position

.level			equb 1			; game level (BCD)
.levelEdibles		skip 1			; number of edible objects (dots, letters, hearts) remaining in current level
.levelSkulls		skip 1			; number of skulls in current level
.levelLetters		skip 3			; 3 random letters for current level
.levelEnd		skip 1			; level is over if != 0
.shield			skip 1			; number of rounds remaining that ladybug is protected against skulls

.objectModeTimer	skip 1			; timer for object mode change
.objectMode		skip 1			; current object mode 0=cyan, 1=red, 2=yellow

.pauseGame		skip 1			; pause the whole game if != 0
.pauseLadybug		skip 1			; number of frames to pause ladybug movement
.pauseEnemy		skip 1			; number of frames to pause enemy movement

.enemySpeed		skip 1			; enemy speed fraction
.enemySpeedCounter	skip 1			; enemy speed fraction counter

.enemyReleaseEnable	skip 1			; enables release of enemy when != 0
.enemyReleaseFrame	skip 1			; frame number to release enemy

.enemysActive		skip 1			; number of enemys currectly active
.enemyTimer		skip 1			; enemy release timer counter 0-91, enemy released when = 0 and enemy release enable != 0
.enemyTimerSpeed	skip 1			; enemy release timer speed (frames) level 1=8, level 2-4=5, level 5-99=3
.enemyTimerSpeedCounter skip 1			; enemy timer tick counter

.enemySpawnSaveX	skip 1			; preserve current enemy index

.playerInput		skip 1			; player input flags, bit 0=up 1=down 2=left 3=right 4=start 5=esc
			
.randomSeed		skip 2			; random number generator seed

.addScoreMultiplySaveA	skip 1			; preserve register
.addScoreMultiplySaveX	skip 1			; preserve register

.checkForObjectsSaveX	skip 1			; preserve register

.drawPlayfieldUpperBonusSaveX
			skip 1			; preserve register

.drawStringSaveY	skip 1			; preserve register

.drawTextSaveY		skip 1			; preserve register
.drawTextAddr		skip 2			; pointer to text

.drawChrFontData	skip 1			; chr bits from font table
.drawChrColor		skip 1			; chr color bit mask
.drawChrSaveX		skip 1			; preserve register
.drawChrSaveY		skip 1			; preserve register

.drawScoreBlanking	equb 0			; leading zero blanking for score/high score display
.drawScoreIndex		equb 0			; current digit being printed
.drawScoreAddr		skip 2			; screen address for current digit

.drawByteCount		skip 1			; byte transfer counter used in sprite and tile functions

.drawTurnstileAddr	skip 2			; drawTurnstile screen address
.drawTurnstileDir	skip 1			; drawTurnstile direction (vertical or horizontal)

.initTimerTilesAddr	skip 2			; tileMap address while initializing timer tiles

.spriteToAddrSaveX	skip 1			; preserve register
.spriteToAddrSaveY	skip 1			; preserve register
.spriteToAddrX		skip 1			; spriteToAddr sprite x position
.spriteToAddrY		skip 1			; spritetoAddr sprite y position

.tileMapAddr		skip 2			; tileMap address used by various routines

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.moveDirMap		equb 1, 47, 23, 25, 24	; tileMap offset from top left corner to up, down, left, right, center
	
.moveSpritesPathCounter	skip 1			; counter for number of paths for valid junction

.moveSpritesSaveX	skip 1			; preserve register

.moveSpritesSaveDirection
			skip 1			; save original sprite direction while calculating new path direction in enemy aim logic

.moveSpritesIndex	skip 1			; index of current sprite

;-----------------------------------------------------------------------------------------------------------------------------------------------------

spritesTotal		= 5			; total number of sprites in game (1 for ladybug and 4 for enemys)

.spritesImg		skip spritesTotal	; sprite image, position and direction for drawing
.spritesX		skip spritesTotal
.spritesY		skip spritesTotal
.spritesDir		skip spritesTotal

.spritesErased		skip spritesTotal	; sprite flag, position and direction for erasing
.spritesEraseX		skip spritesTotal
.spritesEraseY		skip spritesTotal
.spritesEraseDir	skip spritesTotal

spritesAnimationSpeed	= 8			; number of vsyncs per animation frame (6.25Hz)
.spritesImgFrameCounter	equb 1			; timer for animation speed
.spritesImgFrame	equb 0			; current animation frame 0-3

.drawSpriteImg		skip 1			; drawSprite img
.drawSpriteX		skip 1			; drawSprite X position
.drawSpriteY		skip 1			; drawSprite Y position

.drawSpriteSaveX	skip 1			; preserve register
.drawSpriteSaveY	skip 1			; preserve register
.drawSpriteScreenAddr	skip 2			; calculated screen address from sprite x and y position

.spriteToScreenSaveX	skip 1			; preserve register
.spriteToScreenSaveY	skip 1			; preserve register

.eraseSpriteSaveX	skip 1			; preserve register
.eraseSpriteSaveY	skip 1			; preserve register

.eraseBlockBytes	skip 1			; byte count for full block erase

.redrawSpritesCount	skip 1			; counter for sprite list length

.redrawSpritesMax	skip 1			; counter for maximum number of sprites processed in 1 frame

.redrawSpritesIndexUpper			; index to current sprite in list for upper sprites
			equb 0

.redrawSpritesIndexLower			; index to current sprite in list for lower half
			equb 0

.updateLadybugSaveDir	skip 1			; preserve durrent ladybug direction while calculating new direction

.updateObjectTimerSaveX	skip 1			; preserve register

.drawMapTileSaveY	skip 1			; preserve register

soundChannels		= 6			; number of software defined sound channels
.soundAddrPtrs		skip soundChannels * 2	; pointers to sound effect data
.soundTimers		skip soundChannels	; timers for sound effect data (filled with 0 by skip)

.playSoundAddr		skip 2			; storage for sound table address
.playSoundSaveA		skip 1			; preserve register
.playSoundSaveX		skip 1			; preserve register
.playSoundSaveY		skip 1			; preserve register

.animateLadybugActive	skip 1			; ladybug animation activate when != 0
.animateLadybugAddr	skip 2			; address pointer to ladybug animation tables
.animateLadybugCounter	skip 1			; frame counter for ladybug animation

.nameRegCursor		skip 1			; high score name registration cursor position of selected letter
.nameRegCursorOld	skip 1			; high score name registration cursor position of previous selected letter
.nameRegCursorText	skip 1			; high score name registration cursor position in player name text

.mainMenuCursor		skip 1			; main menu cursor position
.mainMenuCursorOld	skip 1			; main menu previous cursor position

.keyboardScanFullSaveX	skip 1			; preserve register



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; end of page0000
;-----------------------------------------------------------------------------------------------------------------------------------------------------
.page0000End
;-----------------------------------------------------------------------------------------------------------------------------------------------------



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; page0100 functions
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org page0100



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; configData					game options, high score, high score name
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.configData					; start of config data
	skip 0

defaultEnemySpeed	= 1			; enemy speed 0-3, 0=slower 1=normal 2=faster 3=fastest
defaultEnemyAttack	= 4			; enemy attack 0-9, 0=more random 4=normal 9=more attack
defaultLives		= 3			; number of starting lives 1-9
defaultSound		= 1			; sound enable 0-1, 0=mute, 1=sound
defaultTimerVolume	= 1			; enemy timer tick volume 0-3, 0=off 1=low 2=medium 3=high

.optionEnemySpeed	equb defaultEnemySpeed
.optionEnemyAttack	equb defaultEnemyAttack
.optionLadybugLives	equb defaultLives
.optionSound		equb defaultSound
.optionTimerVolume	equb defaultTimerVolume

.optionKeys
			equb keyX		; default key for right
			equb keyZ		; default key for left
			equb keySlash		; default key for down
			equb keyColon		; default key for up

.optionKeysAscii

			equs "XZ/:"		; default ascii text for keys used in main menu display

.highScoreBackup

	skip 3					; copy of high score goes here

.highScoreName

	skip 11					; storage for high score name

.configDataEnd					; end of config data

	skip 0

.validationCode					; validation code of config data

	skip 1



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; cleanReset (break key)			force a power on reset to put the bbc computer into a clean state
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org page0130

.cleanReset

	sei					; disable irq interrupts
	
.cleanResetBank

	lda #&ff				; page in ram (value previously set by loader)
	sta bankSelect
	
.cleanResetMachine

	ldx #&ff				; get machine type (value previously set by loader)

	jmp swrCleanReset			; continue with reset code



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; irq interrupt					handle vsync and timer1 interrupts, setting screenHalf upper/lower flag and bump counters
;-----------------------------------------------------------------------------------------------------------------------------------------------------

rasterTimer		= (312 / 2) * 64	; vsync interupt sets timer interrupt to line 156 (312/2 vertical center)

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.irqInterrupt

	lda viaIfr				; if interrupt flag = vsync
	and #2
	bne irqVsync				; then go do the upper vsync interrupt
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.irqTimer					; else its a timer interrupt so do the lower interrupt

	lda #&40				; clear timer interrupt flag
	sta viaIfr

	lda #&ff				; screenHalf = lower
	sta screenHalf

	lda irqAcc				; restore A

	rti					; return to main program

	;---------------------------------------------------------------------------------------------------------------------------------------------

.irqVsync					; upper interrupt

	sta viaIfr				; clear vsync interrupt flag

	lda #lo(rasterTimer)			; set timer 1 for lower interrupt
	sta viaT1CounterLo
	lda #hi(rasterTimer)
	sta viaT1CounterHi

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
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.psgWrite

	sta viaPortA				; place psg data on port a
	
	lda #0 + 0				; psg -we low
	sta viaPortB
	
	pha					; 5uS delay
	pla
	nop
	nop
	
	lda #0 + 8				; psg -we high
	sta viaPortB

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; waitVsyncUpper				wait for next vsync to upper area
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.waitVsyncUpper

	if debugRaster				; if debug then set background color

	lda #palBlack
	sta ulaPalette

	endif

.waitVsyncUpperTest

	bit screenHalf				; wait until upper arena
	bpl waitVsyncUpperTest
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; waitVsyncLower				wait for next vsync to lower area
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.waitVsyncLower

	if debugRaster				; if debug then set background color

	lda #palBlack
	sta ulaPalette

	endif

.waitVsyncLowerTest

	bit screenHalf				; wait until lower area
	bmi waitVsyncLowerTest
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; updateObjectTimer				update object timer (25Hz), change object mode and palette color
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateObjectTimerPalette			; palette colors for letters and hearts

	equb &f0 + palCyan, &f0 + palRed, &f0 + palYellow

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateObjectTimerFrames			; duration of the colors letters and hearts

	equb objectModeCyanTime, objectModeRedTime, objectModeYellowTime

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateObjectTimer

	lda vsyncCounter			; if vsync counter and 1 = 0 (25Hz)
	and #1
	beq updateObjectTimerBump
	rts
	
.updateObjectTimerBump

	dec objectModeTimer			; then bump object timer
	beq updateObjectTimerMode		; if objectModeTimer = 0
	rts

.updateObjectTimerMode
	
	stx updateObjectTimerSaveX		; save register
	
	ldx objectMode				; get current objectMode

	inx					; bump it
	cpx #3					; if its > 2 then set it back to 0
	bne updateObjectTimerColor
	ldx #0
	
.updateObjectTimerColor

	stx objectMode				; update objectMode
	
	lda updateObjectTimerPalette, x		; update color palette
	sta ulaPalette
	
	lda updateObjectTimerFrames, x		; update timer
	sta objectModeTimer

	ldx updateObjectTimerSaveX		; restore register
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; screenClear					fill entire screen with black (pixelCol0)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.screenClear

	lda #hi(screenAddr)			; set start page of screen
	sta screenClearLoop + 2

	ldx #lo(screenAddr)			; start x at low 8 bits of screen start address

	lda #pixelCol0				; fill screen with black

.screenClearLoop

	sta addr16, x				; fill page with data
	inx
	bne screenClearLoop
	
	inc screenClearLoop + 2			; next page
	bpl screenClearLoop			; repeat until page=&80 (end of screen ram)

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; end of page0100 functions
;-----------------------------------------------------------------------------------------------------------------------------------------------------
.page0100End
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.stack						; stack area from here to &01ff



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; page0200 irq vector, functions, break key vector
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org page0200

						; 4 spare memory locations, can be used if needed




;-----------------------------------------------------------------------------------------------------------------------------------------------------
; set irq interrupt vector
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org irqVector

	equw irqInterrupt			; set bbc os irq1v interrupt vector to our irqInterrupt function



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; keyboardScan					check up down left right start and esc keys
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			playerInput		bit 0=up 1=down 2=left 3=right 4=start 5=esc
;			A			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.keyboardScan

	lda #&7f				; set port A bit 7 as input ( from keyboard output )
	sta viaPortDdrA
	
	lda #3 + 0				; keyboard -enable low
	sta viaPortB
	
	lda #0					; clear player input flags
	sta playerInput

	lda #keyEsc				; check esc key
	jsr keyboardScanKey

	lda #keyReturn				; check start key
	jsr keyboardScanKey

	lda optionKeys + 0			; check right key
	jsr keyboardScanKey

	lda optionKeys + 1			; check left key
	jsr keyboardScanKey

	lda optionKeys + 2			; check down key
	jsr keyboardScanKey
	
	lda optionKeys + 3			; check up key
	jsr keyboardScanKey
	
	lda #3 + 8				; keyboard -enable high
	sta viaPortB
	
	lda #&ff				; set port A all bits output
	sta viaPortDdrA

	rts					; return

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.keyboardScanKey

	sta viaPortA				; select key

	lda viaPortA				; read key status

	asl a					; shift key status into player input bits
	rol playerInput
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; end of page0200 functions
;-----------------------------------------------------------------------------------------------------------------------------------------------------
.page0200End
;-----------------------------------------------------------------------------------------------------------------------------------------------------



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; page0258					fx 200 0 to make sure the ram is not erased by the os
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org page0258
	
	equb 0



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; end of page0258
;-----------------------------------------------------------------------------------------------------------------------------------------------------
.page0258End
;-----------------------------------------------------------------------------------------------------------------------------------------------------



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; set the break key jump vector
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org page0287				; set the break jump vector to the clean reset function

	jmp cleanReset



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; random					generate an 8 bit random number
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		randomSeed		2 bytes used to calculate next random number
;						total length = 65535 random bytes before pattern repeats
;						add more seed bytes if longer run length is required
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			8 bit random number (randomSeed + 1)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.random

	lda randomSeed + 1			; get hi 8 bits of seed
	lsr a					; shift it right to put bit 0 into carry
	lda randomSeed				; get lo 8 bits of seed
	ror a					; shift it right putting carry into bit 7 and bit 0 into carry
	eor randomSeed + 1			; eor with hi 8 bits
	sta randomSeed + 1			; store in high 8 bits
	ror a					; shift it right putting carry into bit 7
	eor randomSeed				; eor with lo 8 bits
	sta randomSeed				; store in lo 8 bits
	eor randomSeed + 1			; eor with hi 8 bits
	sta randomSeed + 1			; store in hi 8 bits

	rts					; return with random number in A



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; initPlayfieldMiddle				copy the initial maze tiles to tileMap
;						increment levelEdibles when a dot it placed
;						finally subtrack levelSkulls from levelEdibles
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed by continue to initTimerTiles
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.initPlayfieldMiddle

	ldx #0					; copy initial maze layout from mazeData to tileMap

	stx levelEdibles			; initialize edibles
	
.initPlayfieldMiddleLoop0

	lda mazeData, x
	sta tileMap + 23, x
	
	cmp #mapTileDot				; if a dot is found during copy of maze to tilemap then increment levelEdibles
	bne initPlayfieldMiddleLoop0Next
	inc levelEdibles

.initPlayfieldMiddleLoop0Next

	inx
	bne initPlayfieldMiddleLoop0
	
.initPlayfieldMiddleLoop1

	lda mazeData + &100, x
	sta tileMap + &100 + 23, x
	
	cmp #mapTileDot				; if a dot is found during copy of maze to tilemap then increment levelEdibles
	bne initPlayfieldMiddleLoop1Next
	inc levelEdibles

.initPlayfieldMiddleLoop1Next

	inx
	cpx #lo(21 * 23)
	bne initPlayfieldMiddleLoop1
	
	jsr placeTileMapHearts			; place 3 hearts at random positions in the tileMap

	jsr placeTileMapLetters			; place the 3 random letters at random positions in the tileMap

	jsr placeTileMapSkulls			; place the correct number of skull at random positions in the tileMap

	sec					; levelEdibles -= levelSkulls
	lda levelEdibles
	sbc levelSkulls
	sta levelEdibles

	; continue on to initTimerTiles below



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; initTimerTiles				fill the outer edges of tileMap with timer tiles
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.initTimerTiles

	lda #mapTileTimerTopLeft + wallSolid	; top left tile
	sta tileMap

	lda #mapTileTimerTopRight + wallSolid	; top right tile
	sta tileMap + 22
	
	lda #mapTileTimerBottomLeft + wallSolid	; bottom left tile
	sta tileMap + 22 * 23
	
	lda #mapTileTimerBottomRight + wallSolid; bottom right tile
	sta tileMap + 22 * 23 + 22
	
	ldx #21					; do 21 copies of
	
.initTimerTilesHorizontal

	lda #mapTileTimerTop + wallSolid	; top tile
	sta tileMap, x
	
	lda #mapTileTimerBottom + wallSolid	; bottom tile
	sta tileMap + 22 * 23, x
	
	dex					; until done
	bne initTimerTilesHorizontal

	ldx #21					; do 21 copies of

	lda #lo(tileMap + 1 * 23)
	sta initTimerTilesAddr
	lda #hi(tileMap + 1 * 23)
	sta initTimerTilesAddr + 1

.initTimerTilesVertical

	lda #mapTileTimerLeft + wallSolid	; left tile
	ldy #0
	sta (initTimerTilesAddr), y

	lda #mapTileTimerRight + wallSolid	; right tile
	ldy #22
	sta (initTimerTilesAddr), y

	clc					; move to next row
	lda #23
	adc initTimerTilesAddr
	sta initTimerTilesAddr
	lda #0
	adc initTimerTilesAddr + 1
	sta initTimerTilesAddr + 1

	dex					; until done
	bne initTimerTilesVertical
	
	lda #0					; zero enemy timer position
	sta enemyTimer

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; tileMapfindDot				find a random tileMap location that contains a dot and isnt near a turnstile
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			tileMapAddr		contains the address of the dot in the tileMap
;			A			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.tileMapfindDot

	jsr random				; get random value (0-255)
	
	and #&1c				; masked value becomes 0-28 in steps of 4, we only need values 0 to 20
	cmp #21					; if its higher than 20 then try again
	bcs tileMapfindDot
	
	tay					; get tileMap row address
	lda tileMapRowsLo, y
	sta tileMapAddr
	lda tileMapRowsHi, y
	sta tileMapAddr + 1
	
.tileMapfindDotX

	jsr random				; get random value (0-255)
	
	and #&1c				; masked value becomes 0-28 in steps of 4, we only need values 0 to 20
	cmp #21					; if its higher than 20 then try again
	bcs tileMapfindDotX
	
	clc					; add x to tileMapAddr
	adc tileMapAddr				; tileMapAddr now points to the top left of the 3x3 tile cube to investigate
	sta tileMapAddr
	lda #0
	adc tileMapAddr + 1
	sta tileMapAddr + 1

	ldy #24					; if center tile does not contain a dot then try again
	lda (tileMapAddr), y
	cmp #mapTileDot
	bne tileMapfindDot

	ldy #1					; if center top tile contains turnstile then try again
	lda (tileMapAddr), y
	and #&c0
	eor #&80
	beq tileMapfindDot
	
	ldy #47					; if center bottom tile contains turnstile then try again
	lda (tileMapAddr), y
	and #&c0
	eor #&80
	beq tileMapfindDot
	
	ldy #23					; if center left tile contains turnstile then try again
	lda (tileMapAddr), y
	and #&c0
	eor #&80
	beq tileMapfindDot

	ldy #25					; if right center tile contains turnstile then try again
	lda (tileMapAddr), y
	and #&c0
	eor #&80
	beq tileMapfindDot
	
	clc					; location found so adjust address to center tile containing the dot
	lda #24
	adc tileMapAddr
	sta tileMapAddr
	bcc tileMapFindDotExit
	inc tileMapAddr + 1

.tileMapFindDotExit

	rts					; return with tile address



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; chooseLetters					choose 3 random letters for the current level
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.chooseLetters

	jsr random				; pick 1st random letter (0-9)
	
	and #&0f
	cmp #&0a
	bcs chooseLetters
	
	sta levelLetters			; and store it in 1st
	
.chooseLetters2nd

	jsr random				; pick 2nd random letter (0-9)
	
	and #&0f
	cmp #&0a
	bcs chooseLetters2nd
	
	cmp levelLetters			; if its the same as 1st then try again
	beq chooseLetters2nd
		
	sta levelLetters + 1			; else store it in 2nd
	
.chooseLetters3rd

	jsr random				; pick 3rd random letter (0-9)
	
	and #&0f
	cmp #&0a
	bcs chooseLetters3rd
	
	cmp levelLetters			; if its the same as 1st then try again
	beq chooseLetters3rd
		
	cmp levelLetters + 1			; if its the same as 2nd then try again
	beq chooseLetters3rd

	sta levelLetters + 2			; else store it in 3rd

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; placeTileMapHearts				place 3 hearts at random locations in the map
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.placeTileMapHearts

	ldx #3					; 3 hearts

.placeTileMapHeartsLoop

	jsr tileMapfindDot			; pick a random tileMap location containing a dot

	lda #mapTileHeart			; replace it with a heart
	ldy #0
	sta (tileMapAddr), y
	
	dex					; contine until all hearts placed
	bne placeTileMapHeartsLoop
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; placeTileMapLetters				place the 3 previously chosen letters at random locations in the map
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.placeTileMapLetters

	jsr tileMapfindDot			; pick a random tileMap location containing a dot

	lda levelLetters			; replace it with 1st letter
	clc
	adc #objectTileIndex
	ldy #0
	sta (tileMapAddr), y
	
	jsr tileMapfindDot			; pick a random tileMap location containing a dot

	lda levelLetters + 1			; replace it with 2nd letter
	clc
	adc #objectTileIndex
	ldy #0
	sta (tileMapAddr), y
	
	jsr tileMapfindDot			; pick a random tileMap location containing a dot

	lda levelLetters + 2			; replace it with 3rd letter
	clc
	adc #objectTileIndex
	ldy #0
	sta (tileMapAddr), y
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; placeTileMapSkulls				place the correct number of skulls for the current level at random locations in the map
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.placeTileMapSkulls

	ldx levelSkulls				; get number of skulls for level

.placeTileMapSkullsLoop

	jsr tileMapfindDot			; pick a random tileMap location containing a dot

	lda #mapTileSkull			; replace it with a skull
	ldy #0
	sta (tileMapAddr), y
	
	dex					; contine until all skulls placed
	bne placeTileMapSkullsLoop
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; initSprites					; turn off all sprites, mark all sprites as erased, disable animation
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.initSprites

	ldx #spritesTotal - 1			; total number of sprites to initialise

.initSpritesLoop

	lda #spriteBlanking + moveStop		; sprite disabled
	sta spritesDir, x

	lda #&ff				; sprite erase buffer disabled
	sta spritesErased, x

	dex
	bpl initSpritesLoop			; repeat until all sprites initialised

	lda #0					; no active enemys yet
	sta enemysActive
	
	sta animateLadybugActive		; disable ladybug animation

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawScore					draw score with leading zero blanking (single digit at a time)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		drawDigitsEnable	flag for leading zero blanking
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			drawMapTileAddr		points to next tile position on screen
;			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawScore

	if debugRaster				; if debug set background color

	lda #palRed
	sta ulaPalette

	endif

	lda #score				; set address for score display
	sta drawScoreDigitRead + 1

	dec drawScoreIndex			; move to next digit
	bpl drawScoreDigit			; if index < 0
	
	lda #5					; then index = 5
	sta drawScoreIndex
	lda #0					; blanking = true
	sta drawScoreBlanking

						; set screen address to lower panel for score score

	lda #lo(screenAddr + 16 * chrColumn + 24 * chrRow)
	sta drawScoreAddr
	lda #hi(screenAddr + 16 * chrColumn + 24 * chrRow)
	sta drawScoreAddr + 1

.drawScoreDigit

	lda drawScoreAddr			; copy digit address to tile print address
	sta drawMapTileAddr
	lda drawScoreAddr + 1
	sta drawMapTileAddr + 1

	lda drawScoreIndex			; get digit index / 2 for byte
	lsr a
	tax
	
.drawScoreDigitRead

	lda addr8, x				; get byte from score
	
	bcc drawScoreDigitPrint			; if its an odd digit then shift bits to get the upper bcd digit
	
	lsr a
	lsr a
	lsr a
	lsr a

.drawScoreDigitPrint

	and #&0f				; just use lowest 4 bits

	beq drawScoreDigitCheckBlanking		; if digit != 0 then disable blanking
	dec drawScoreBlanking

.drawScoreDigitCheckBlanking

	bit drawScoreBlanking			; if blanking enabled
	bmi drawScoreNext

	lda #extraTileBlank			; print a blank tile instead of digit 0

.drawScoreNext

	jsr drawExtraTile			; print digit tile
	
	lda drawMapTileAddr			; copy next digit address for next time
	sta drawScoreAddr
	lda drawMapTileAddr + 1
	sta drawScoreAddr + 1
	
	rts					; return
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawHighScore					; draw highScore using 6 calls to drawscore
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawHighScore

	if debugRaster				; if debug set background color

	lda #palGreen
	sta ulaPalette

	endif

	lda #highScore				; set address for highScore display
	sta drawScoreDigitRead + 1

	lda #5					; index = 5
	sta drawScoreIndex
	lda #0					; blanking = true
	sta drawScoreBlanking

						; set screen address to lower panel for high score score

	lda #lo(screenAddr + 16 * chrColumn + 25 * chrRow)
	sta drawScoreAddr
	lda #hi(screenAddr + 16 * chrColumn + 25 * chrRow)
	sta drawScoreAddr + 1

.drawHighScoreLoop

	jsr drawScoreDigit			; print a digit of high score
	
	dec drawScoreIndex			; repeat until all 6 digits done
	bpl drawHighScoreLoop
	
	lda #0					; final 0 digit
	jsr drawExtraTile

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; eraseSprite					erase the sprite block of 10x14 pixels on screen
;						redraw the tile behind the sprite
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			X			sprite number (index into spritesErase table containing x, y, dir information)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.eraseSpriteTileX

	equb 7, 7, 14, 0			; y offset for tile behind sprite
	
.eraseSpriteTileY

	equb 14, 0, 7, 7			; y offset for tile behind sprite

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.eraseSprite

	stx eraseSpriteSaveX			; preserve registers
	sty eraseSpriteSaveY
	
	if debugRaster				; if debug then set background color

	lda #palMagenta
	sta ulaPalette

	endif

	lda spritesEraseX, x			; erase 10x14 pixel area at coordinates
	sta drawSpriteX
	lda spritesEraseY, x
	sta drawSpriteY
	jsr eraseBlock

	bcc eraseSpriteExit			; exit now if coordinates were invalid

.eraseSpriteTile

	ldx eraseSpriteSaveX			; get sprite index

	lda spritesEraseDir, x			; if sprite was moving
	and #moveStop
	bne eraseSpriteExit

	lda spritesEraseDir, x			; get direction it was moving
	and #3
	tay
	
	lda spritesEraseX, x			; calculate the address of the tile to the left/right/above/below the sprite (the tile that is behind sprite)
	clc
	adc eraseSpriteTileX, y
	sta spriteToAddrX
	
	lda spritesEraseY, x
	clc
	adc eraseSpriteTileY, y
	sta spriteToAddrY
	jsr spriteToAddr

	ldy #0					; get tile from map and if its not a blank tile then redraw it on screen
	lda (tileMapAddr), y
	cmp #mapTileBlank
	beq eraseSpriteAdjustAddress

	jsr drawMapTile
	jmp eraseSpriteCheckLeft
	
.eraseSpriteAdjustAddress

	clc					; if the tile wasnt drawn then move to next screen tile location
	lda #chrColumn
	adc drawMapTileAddr
	sta drawMapTileAddr
	lda #0
	adc drawMapTileAddr + 1
	sta drawMapTileAddr + 1
	
.eraseSpriteCheckLeft
	
	lda spritesEraseDir,x			; if sprite was moving left
	and #3
	cmp #moveLeft
	bne eraseSpriteCheckUp

	ldy #1					; then check tile to the right
	lda (tileMapAddr), y
	bmi eraseSpriteExit			; if its a maze tile then exit

	cmp #objectTileIndex			; if its an object tile then redraw it
	bcc eraseSpriteExit

	jsr drawMapTile				; draw tile and exit.

.eraseSpriteExit

	ldy eraseSpriteSaveY			; restore registers
	ldx eraseSpriteSaveX

	rts					; return

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

	lda (tileMapAddr), y			; draw tile
	jsr drawMapTile

	jmp eraseSpriteExit



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; eraseBlock					erase a 10x14 block of pixels
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			spriteX			sprite coordinates for erasure
;			spriteY
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.eraseBlock

	jsr spriteToScreen			; convert sprite xy to screen address
	bcs eraseBlockAddr

.eraseBlockExit

	rts					; exit if coordinates were invalid

.eraseBlockAddr

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

	lda #pixelCol0				; fill byte = black

.eraseBlockWrite

	sta addr16				; write to screen

	sec
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

	lda #pixelCol0				; reload A with black

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

	bpl eraseBlockColumn			; and do it again

;*****************************************************************************************************************************************************

	clc					; we should never reach this point
	rts					; but if we do then exit with carry clear to show there was an issue

;*****************************************************************************************************************************************************



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawExtraTile					draw tile to screen, move to next tile position
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			tile img to be drawn
;			drawMapTileAddr		current screen location for tile
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			drawMapTileAddr		points to next tile position on screen
;			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawExtraTile

	sty drawMapTileSaveY			; save Y register

	sta drawMapTileRead + 1			; save tile number

	tay					; convert to mapTile index to address
	lda extraTileAddrLo, y
	sta drawMapTileRead + 1
	lda extraTileAddrHi, y
	sta drawMapTileRead + 2

	lda #mapTileBytes - 1			; regular 6 pixel map tile
	sta drawMapTileTransfer + 1

	bne drawMapTileTransfer			; draw the extra tile



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawMapTile					draw tile to screen, move to next tile position
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			tile img to be drawn
;			drawMapTileAddr		current screen location for tile
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			drawMapTileAddr		points to next tile position on screen
;			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

; bits 7 and 6 have a special function

; bits	76	function
;	00	tile 0-63 (path)
;	01	tile 64-127 (path)
;	10	tile 0-63 (solid wall to enemy only)
;	11	tile 0-63 (solid wall to enemy and player)

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawMapTile4Pixel

	sty drawMapTileSaveY			; save Y register

	ora #0					; if bit 7 = 1 then tileImg = tileImg and #&3f
	bpl drawMapTileCalcAddr4pixel
	and #&3f

.drawMapTileCalcAddr4pixel

	cmp #objectTileIndex			; if its an object tile then use drawObjectTile instead
	bcs drawObjectTile
	
	tay					; convert to mapTile index to address
	lda mapTileAddrLo, y
	sta drawMapTileRead + 1
	lda mapTileAddrHi, y
	sta drawMapTileRead + 2

	lda #mapTileBytes - 9			; 4 pixel wide map tile
	sta drawMapTileTransfer + 1

	jmp drawMapTileTransfer

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawMapTile

	sty drawMapTileSaveY			; save Y register

	ora #0					; if bit 7 = 1 then tileImg = tileImg and #&3f
	bpl drawMapTileCalcAddr
	and #&3f

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

.drawMapTileTransfer

	ldy #0					; bytes to transfer
	
.drawMapTileRead

	lda addr16, y				; read byte from tile
	
.drawMapTileWrite

	sta addr16, y				; write byte to screen
	
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

	ldy drawMapTileSaveY			; restore y register

	rts					; return

drawMapTileAddr 		= drawMapTileWrite + 1



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawObjectTile				draw object tile to screen, move to next tile position
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			tile img to be drawed
;			drawObjectTileAddr	current screen location for tile
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			drawObjectTileAddr	points to next tile position on screen
;			A			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawObjectTile

	sec					; subtract starting index so objectTile is indexed from 0
	sbc #objectTileIndex

	tay					; convert to objectTile index to address
	lda objectTileAddrLo, y
	sta drawMapTileRead + 1
	lda objectTileAddrHi, y
	sta drawMapTileRead + 2

	sec					; offset drawMapTileAddress -8 to allow for larger sized objectTile
	lda drawMapTileAddr
	sbc #8
	sta drawMapTileAddr
	lda drawMapTileAddr + 1
	sbc #0
	sta drawMapTileAddr + 1

	lda #objectTileBytes - 1		; number of bytes to transfer from tile to screen
	sta drawMapTileTransfer + 1
	bne drawMapTileTransfer



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawHex					draw A as 2 digits of hexadecimal characters (can be used for bcd numbers too)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			value to display as hex
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		stack			1 byte stack space
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			drawChrAddr		points to next chr position on screen
;			A			destroyed
;			X			preserved
;			y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawHex

	pha					; save value
	
	lsr a					; draw high nybble
	lsr a
	lsr a
	lsr a
	ora #'0'
	jsr drawChr
	
	pla					; get value
	
	and #&0f				; draw low nybble
	ora #'0'



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawChr					draw chr to screen, move to next chr position ready for next
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			chr to be drawn 0-50
;			drawChrAddr		current screen location for chr
;			drawChrColor		bit mask for pixel colors
;						if chr = " " then use color 8 instead
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		drawChrFontData		data read from font to be converted to pixels
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			drawChrAddr		points to next chr position on screen
;			A			destroyed
;			X			preserved
;			y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

; chrs are 6 x 6 pixels stored in pixel pairs from to bottom, left to right using 5 bytes per character, the last 4 bits are unused

; 763276
; 541054
; 327632
; 105410
; 763276
; 541054

;-----------------------------------------------------------------------------------------------------------------------------------------------------

pixelLeft		= &aa			; bit mask for left pixel color F
pixelRight		= &55			; bit mask for right pixel color F

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawChr

	stx drawChrSaveX			; save registers
	sty drawChrSaveY

	sec					; set character code origin to 0
	sbc #' '

	sta drawChrFontRead + 1			; save chr in lsb address
	lda #0					; zero msb address
	sta drawChrFontRead + 2

	ldy drawChrColor			; get color

	lda drawChrFontRead + 1			; get chr
	bne drawChrCalcAddr			; if chr = space

	ldy #pixelCol8				; then use color 8 instead of drawChrColor

.drawChrCalcAddr

	sty drawChrWriteColor + 1		; store chr color

	asl a					; calculate chr address A * 5 + chrTable
	rol drawChrFontRead + 2
	asl a
	rol drawChrFontRead + 2
	clc
	adc drawChrFontRead + 1
	sta drawChrFontRead + 1
	lda #0
	adc drawChrFontRead + 2
	sta drawChrFontRead + 2

	clc					; add chrTable address
	lda drawChrFontRead + 1
	adc #lo(fontBin)
	sta drawChrFontRead + 1
	lda drawChrFontRead + 2
	adc #hi(fontBin)
	sta drawChrFontRead + 2

	ldy #6					; 6 lines per column
	
	ldx #0					; counter for bit pairs

.drawChrLoop

	txa					; every 4 pairs get a byte from chr table
	and #3
	bne drawChrPixelLeft
	
.drawChrFontRead

	lda addr16				; read 4 pixel pairs from font and save it
	sta drawChrFontData

	inc drawChrFontRead + 1			; bump font read address
	bne drawChrPixelLeft
	inc drawChrFontRead + 2

.drawChrPixelLeft

	lda #&00				; get 2 bits from chr data and convert to 2 pixels
	asl drawChrFontData
	bcc drawChrPixelRight
	ora #pixelLeft

.drawChrPixelRight

	asl drawChrFontData
	bcc drawChrWriteColor
	ora #pixelRight

.drawChrWriteColor	

	and #&ff				; mask pixels with color value previously stored here

	beq drawChrNextLine			; skip writing if byte = &00

.drawChrWriteScreen

	sta addr16				; write to screen

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

	cpx #(6 * 6) / 2			; are all pixels done yet ?
	bcc drawChrLoop
	
	ldy drawChrSaveY			; restore registers
	ldx drawChrSaveX

	rts					; return

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; alias to self modifying code address
;-----------------------------------------------------------------------------------------------------------------------------------------------------

drawChrAddr		= drawChrWriteScreen + 1; screen address to write chr



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; pauseWait					; wait n * 25th's of a second
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.pauseWait

	sta pauseCounter			; setup wait time
	
.pauseWaitLoop

	bit pauseCounter			; wait until counter hits 0
	bne pauseWaitLoop

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; ladybug main
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.main						; main program starts here

	cli					; enable interrupts

	jsr screenClear				; clear the screen

	lda #6					; enable display
	sta crtcAddr
	lda #screenHeight
	sta crtcData

	if debugSpriteTest

	jmp spriteTest
	
	endif

	lda #pause * 1.00			; wait 1.00 seconds
	jsr pauseWait
	
	jsr drawPlayfieldUpper			; display the upper play field



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; display the main screen with player options, wait for start to be pressed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.gameIntroScreen

	lda highScore				; backup high score for saving to disk
	sta highScoreBackup
	lda highScore + 1
	sta highScoreBackup + 1
	lda highScore + 2
	sta highScoreBackup + 2

	jsr drawPlayfieldLower			; display lower playfield

	jsr mainMenu				; display the main menu screen

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup a new game for level 1
	;---------------------------------------------------------------------------------------------------------------------------------------------

.gameStartNew

	;---------------------------------------------------------------------------------------------------------------------------------------------
	
	if debugZeroHighScore			; if debug mode set highScore = 0

	lda #0
	sta highScore
	sta highScore + 1
	sta highScore + 2
	
	endif



	;---------------------------------------------------------------------------------------------------------------------------------------------

	if debugLevel == 0

	lda #&01				; start game on level 1
	sta level

	lda #&10				; start with a cucumber at 1000 points
	sta vegetableScore
	lda #&00
	sta vegetableImg

	sta shield				; no shield at start, ladybug is vulnerable to skulls

	else
	
	lda #debugLevel				; start game on debugLevel
	sta level

	lda #&95				; start with a horse radish at 9500 points
	sta vegetableScore
	lda #17
	sta vegetableImg

	lda #0
	sta shield				; no shield at start, ladybug is vulnerable to skulls

	endif



	sta score				; zero the player score
	sta score + 1
	sta score + 2

	lda optionLadybugLives			; initialize player lives
	sta lives



	;---------------------------------------------------------------------------------------------------------------------------------------------

	if debugInstantHighScore		; if debugInstantHighScore == true then

	sed
	clc					; score = highScore + 100000
	lda highScore + 2
	adc #1
	sta score + 2
	cld

	lda #0					; lives = 0
	sta lives
	
	endif



	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #&ff				; clear the bonus bits
	sta bonusBits
	sta bonusBits + 1

	sta ladybugEntryEnable			; enable ladybug entry animation

	sta bonusDiamondEnable			; enable the possibility of getting a diamond bonus

	;---------------------------------------------------------------------------------------------------------------------------------------------

	if debugBonus				; if debugBonus then

	lda bonusBits + 1			; enable all special and extra letters
	and #bonusBitsSpecial eor &ff
	sta bonusBits + 1

	lda bonusBits + 0
	and #bonusBitsExtra eor &ff
	sta bonusBits + 0

	endif



	;---------------------------------------------------------------------------------------------------------------------------------------------

	if debugDiamondBonus			; if debugDiamondBonus then

	lda #&ff				; enable diamond bonus
	sta bonusDiamondActive

	endif



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; setup level here too so that instructions page shows correct settings
	;---------------------------------------------------------------------------------------------------------------------------------------------


	jsr initLevelSettings			; setup skulls, letters, enemy settings etc for current level

	jsr drawPlayfieldUpper			; draw playfield upper section (top bonus panel)

	jsr drawPlayfieldLower			; draw playfield lower section (info panel)

	jsr instructions			; display the game instructions
	bcs gameIntroScreen			; return to intro screen if esc was pressed


	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #sfxExtraLife			; play extra life sound effect for first level
	jsr playSound



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; setup current level ready for play
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.gameLevelStart

	jsr initLevelSettings			; setup skulls, letters, enemy settings etc for current level

	jsr drawPlayfieldUpper			; draw playfield upper section (top bonus panel)

	jsr drawPlayfieldLower			; draw playfield lower section (info panel)

	jsr drawLevelIntro			; draw the level intro screen

	jsr initPlayfieldMiddle			; initialize playfield middle section tileMap and count number of dots

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

	jsr initTimerTiles			; fill edges with timer tiles
	
	jsr drawPlayfieldMiddle			; draw playfield middle section from tileMap

	jsr enemySpawn				; spawn 1st enemy in center box ready to be active on release timer

	jsr ladybugSpawn			; spawn ladybug

	lda #0					; unpause ladybug
	sta pauseLadybug
	
	sta vegetableActive			; deactivate center vegetable

	sta vegetableScoreActive		; deactivate center vegetable score display

	sta objectScoreImg			; deactivate object score display

	sta enemyReleaseEnable			; disable enemy release

	lda #escTime				; reset esc counter
	sta escCounter



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; game loop					wait interrupts, draw graphics, update sprites and game data. everything !
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; wait for timer middle interrupt then process stuff for upper half of screen
	;---------------------------------------------------------------------------------------------------------------------------------------------

.gameLoopUpper

	jsr waitVsyncUpper			; wait for vsync interrupt for upper area

	jsr ladybugEntryAnimation		; ladybug entry movement if enabled

	jsr checkLevelEnd			; check if current level has ended (object count == 0)
	bcs gameLevelStart			; if level has ended then start new level

	jsr checkBonus				; check if a special, extra or diamond bonus screen is required
	bcs gameLevelStart			; if bonus was awarded then start a new level

	jsr drawVegetableCenter			; draw the vegetable in the center bug box (if active)

	jsr redrawSprites			; erase and redraw sprites in upper area

	jsr ladybugDeathAnimation		; ladybug death animation (if enabled)
	bcs gameLevelRestart			; restart level if ladbug death animation has just completed

	jsr drawVegetableScore			; draw the vegetable bonus score in center (if active)

	jsr drawObjectScore			; draw object score (if active)

	jsr drawTurnstile			; draw any pending turnstile movement

	jsr updateSkullColor			; update the skull palette color

	jsr updateBonusColor			; update the bonus palette colors

	jsr checkPauseGame			; if game is paused then skip movement and timer stuff
	bcs gameLoopLower

	jsr updateLadybug			; update ladybug direction, handle turnstile and object detection

	jsr moveSprites				; move all sprites

	jsr updateObjectTimer			; update object timer, mode and palette

	jsr drawScore				; draw score (1 digit per loop)



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; wait for vsync top interrupt and process stuff for lower half of screen
	;---------------------------------------------------------------------------------------------------------------------------------------------

.gameLoopLower

	jsr waitVsyncLower			; wait for vsync interrupt for lower area
	
	jsr processSound			; process sound effects and music

	jsr redrawSprites			; erase and redraw sprites in lower area
	
	jsr ladybugDeathAnimation		; draw ladybug death animation (if enabled)
	bcs gameLevelRestart			; restart level if needed

	jsr drawVegetableScore			; draw the vegetable bonus score in center (if active)

	jsr drawObjectScore			; draw object score (if active)

	jsr updateAnimationFrame		; update the animtion frame number

	jsr keyboardScan			; read keyboard input

	jsr checkEsc				; check if we need to quit the game
	bcc gameLoopLowerCheckPause
	jmp gameIntroScreen

.gameLoopLowerCheckPause

	jsr checkPauseGame			; if game is paused then skip movement and timer stuff
	bcs gameLoopUpper

	jsr updateEnemyTimer			; update the enemy timer and draw tile when needed

	jsr enemyRelease			; release an enemy (if enabled)

	jsr updatePauseTimers			; update ladybug and enemy pause timers

	if debugCoordinates			; if debugCoordinates == true
	
	lda #lo(screenAddr)			; then display lady coordinates
	sta drawChrMiniAddr
	lda #hi(screenAddr)
	sta drawChrMiniAddr + 1
	lda spritesX + 0
	jsr drawHexMini
	lda spritesY + 0
	jsr drawHexMini

	endif					; endif

	jmp gameLoopUpper



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

	lda escCounter				; if escCounter == 0 return true
	beq checkEscReturnTrue
	
	dec escCounter				; else decrement escCounter and return false
	bne checkEscReturnFalse
	
.checkEscReturnTrue

	jsr playSoundSilence			; terminate any current sound effects or music

	lda #sfxSkull				; play skull sound
	jsr playSound

	sec					; return true
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; gameOver					; clear the screen and display game over message
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.gameOver

	lda #gameOverTime			; set the screen timeout
	sta pauseCounter

	jsr playfieldMiddleWithTimer		; initialize and draw playfield with only timer

	jsr initSprites				; initialize all sprites as blanked and erased

	jsr drawString				; draw game over message
	equb pixelCol1
	equw screenAddr + 2 + 7 * chrColumn + 12 * chrRow
	equs "GAME OVER", &ff
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; process some color stuff and wait for the timer to expire
	;---------------------------------------------------------------------------------------------------------------------------------------------

.gameOverLoop

	jsr waitVsyncUpper			; wait upper half
	jsr waitVsyncLower			; wait lower half

	jsr updateBonusColor			; update the bonus palette colors

	lda pauseCounter			; repeat until time expires
	bne gameOverLoop

	jsr checkHighScore			; check if highScore was beaten

	jmp gameIntroScreen			; return to game intro screen



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; checkHighScore				check if score > highScore and do something
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.checkHighScore

	sec					; subtract score from highScore
	lda highScore
	sbc score
	lda highScore + 1
	sbc score + 1
	lda highScore + 2
	sbc score + 2
	bcc checkHighScoreUpdate		; if there was a borrow
	
	rts
	
.checkHighScoreUpdate

	jsr nameReg				; get name registration from player

	lda score				; set highScore = score
	sta highScore
	lda score + 1
	sta highScore + 1
	lda score + 2
	sta highScore + 2
	
	lda #sfxMusicLetters			; play high score music and return
	jmp playSound



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; playfieldMiddleWithTimer			clear tilemap
;						fill tilemap with timer
;						draw tilemap
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.playfieldMiddleWithTimer

	jsr clearTileMap			; fill tilemap with blank tile
	
	jsr initTimerTiles			; fill edges with timer tiles
	
	jmp drawPlayfieldMiddle			; draw the middle playfield



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; ladybugEntryAnimation
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.ladybugEntryAnimation

	lda ladybugEntryEnable			; if ladybug entry animation enabled
	beq ladybugEntryAnimationExit

	lda spritesX + 0			; if ladybug at entry position
	cmp #ladybugEntryX
	bne ladybugEntryAnimationNext

	lda spritesY + 0
	cmp #ladybugEntryY
	bne ladybugEntryAnimationNext
	
	lda #sfxMusicEntry			; play the entry music
	jsr playSound

	lda #ladybugEntryTime			; pause enemy timer during ladybug entry animation
	sta pauseEnemy

.ladybugEntryAnimationNext

	jsr animateLadybug			; do the animation
	
	lda spritesX + 0			; if ladybug at start position
	cmp #ladybugStartX
	bne ladybugEntryAnimationExit
	
	lda spritesY + 0
	cmp #ladybugStartY
	bne ladybugEntryAnimationExit
	
	lda #0					; then disable entry animation
	sta ladybugEntryEnable

.ladybugEntryAnimationExit

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; checkLevelEnd					check levelEnd flag and levelEdibles, trigger an end if needed (set the carry flag)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.checkLevelEnd

	lda levelEnd				; if level not ended
	bne checkLevelEnded

	lda levelEdibles			; if theres still edible objects then exit
	bne checkLevelEndExit
	
	lda #endLevelTime			; pause ladybug and enemys
	sta pauseLadybug
	sta pauseEnemy

	lda soundTimers + 0			; if sound effect not playing on channel 0
	bne checkLevelEndExit

	lda #&ff				; flag level as ended
	sta levelEnd

	jsr playSoundSilence			; silence all sound channels

	lda #sfxEndLevel			; play end of level sound
	jsr playSound

.checkLevelEndExit

	clc					; continue game
	rts					; return

.checkLevelEnded

	lda pauseLadybug			; if pause is over (ladybug and enemy unpaused)
	ora pauseEnemy
	bne checkLevelEndExit

	jsr levelAdvance			; advance game to next level

	sec					; end level
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; updateEnemyTimer				update the enemy timer, draw timer tile when needed
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
	
	lda enemysActive			; if theres an enemy waiting in the center
	cmp #spritesTotal - 1
	beq updateEnemyTimerExit

	lda #sfxEnemyWarning			; then play enemy release warning and enable enemy release
	jsr playSound
	lda #&ff
	sta enemyReleaseEnable

.updateEnemyTimerExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; moveSprites					update coordinates of all sprites
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		moveSpritesIndex	index to current sprite
;			moveSpritesPathCounter	count the number of paths from sprite location to check for valid junction
;
;			moveSpritesSaveDirection
;						temporary storage for sprite direction
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; enemy control table
;-----------------------------------------------------------------------------------------------------------------------------------------------------

moveSpritesJunctionPaths= 3			; must be at least this number of paths at a grid location to be valid junction
						; at a valid junction an enemy will either turn to ladybugs direction (attack)
						; or will choose a random available direction
						; if the attack direction is not available then a random available direction is chosen
.enemyRandomChance

	equb (99.96 * 256) / 100		; % chance of enemy turning randomly instead of towards ladybug
	equb (91.63 * 256) / 100
	equb (83.30 * 256) / 100
	equb (74.97 * 256) / 100
	equb (66.64 * 256) / 100
	equb (58.31 * 256) / 100
	equb (49.98 * 256) / 100
	equb (41.65 * 256) / 100
	equb (33.32 * 256) / 100
	equb (24.99 * 256) / 100
	equb (16.66 * 256) / 100
	equb (08.33 * 256) / 100
	equb (00.00 * 256) / 100



;-----------------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesDirX

	equb 0, 0, -1 , 1			; X up down left right

.moveSpritesDirY

	equb -1, 1, 0, 0			; Y up down left right
	
.moveSpritesAvoidSkull

	equb 1, 47, 23, 25			; direction offset for avoiding skulls

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.moveSprites

	if debugRaster				; if debug then set background color

	lda #palGreen
	sta ulaPalette

	endif

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	ldx #0					; move sprites 0 to max 1 pixel (ladybug and enemys)
	jsr moveSpritesPixel
	
	clc					; do the speed fraction counter see if we need to move enemys another pixel
	lda enemySpeedCounter
	adc enemySpeed
	sta enemySpeedCounter
	bcs moveSpritesEnemy			; if carry generated then move enemys 1 extra pixel
	jmp moveSpritesExit
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesEnemy

	ldx #1					; else move sprites 1 to max 1 pixel (enemys only, not ladybug sprite 0)

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesPixel	

	stx moveSpritesIndex

.moveSpritesLoop

	ldx moveSpritesIndex			; get current sprite index

.moveSpritesIsLadybugEnabled

	cpx #0					; if index = 0 (ladybug) and ladybug is paused then skip it
	bne moveSpritesIsEnemyEnabled
	lda pauseLadybug
	beq moveSpritesGetDirection
	jmp moveSpritesNext
	
.moveSpritesIsEnemyEnabled

	lda pauseEnemy				; if index > 0 (enemys) and enemys are paused then skip them
	beq moveSpritesGetDirection
	jmp moveSpritesNext

.moveSpritesGetDirection

	lda spritesDir, x			; if sprite is blanked or not moving then skip it and go on to next sprite
	and #spriteBlanking + moveStop
	beq moveSpritesDirection
	
	jmp moveSpritesNext

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

.moveSpritesCollision

	lda spritesDir + 0			; if ladybug is not blanked
	and #spriteBlanking
	bne moveSpritesCheckAlignmentX

	lda spritesX, x				; if abs(enemyX - ladybugX) < ladybugEnemyRange
	sec
	sbc spritesX + 0
	bcs moveSpritesCollisionX
	eor #&ff
	clc
	adc #1

.moveSpritesCollisionX

	cmp #ladybugEnemyRange
	bcs moveSpritesCheckAlignmentX

	lda spritesY, x				; if abs(enemyY - ladybugY) < ladyBugEnemyRange
	sec
	sbc spritesY + 0
	bcs moveSpritesCollisionY
	eor #&ff
	clc
	adc #1

.moveSpritesCollisionY

	cmp #ladybugEnemyRange
	bcs moveSpritesCheckAlignmentX

	if debugInvulnerable == false

	jsr ladybugKill				; then kill ladybug

	endif

.moveSpritesCheckAlignmentX

	lda spritesX, x				; if sprite x != 8 then skip to next sprite
	and #&0f
	cmp #8
	beq moveSpritesCheckAlignmentY
	jmp moveSpritesNext

.moveSpritesCheckAlignmentY

	lda spritesY, x				; if sprite y != 8 then skip to next sprite
	and #&0f
	cmp #8
	beq moveSpritesGetMapAddr
	jmp moveSpritesNext

.moveSpritesGetMapAddr

	jsr spriteToAddr			; convert sprite XY to tileMapAddr

.moveSpritesCheckSkull

	ldy #24					; if tile under enemy = skull
	lda (tileMapAddr), y
	cmp #mapTileSkull
	bne moveSpritesCheckValidJunction

.moveSpritesKillEnemy

	lda #mapTileBlank			; erase skull from map
	sta (tileMapAddr), y

	lda #lo(chrRow + chrColumn)		; adjust drawMapTileAddr to skull screen address
	clc
	adc drawMapTileAddr
	sta drawMapTileAddr
	lda #hi(chrRow + chrColumn)
	adc drawMapTileAddr + 1
	sta drawMapTileAddr + 1

	lda #mapTileBlankObj			; erase skull from screen
	jsr drawMapTile

	lda #sfxSkull				; play skull sound
	jsr playSound

	dec enemysActive			; reduce number of active enemys
	
	lda #spriteBlanking			; deactivate the current enemy
	ora spritesDir, x
	sta spritesDir, x

	jsr enemySpawn				; spawn enemy

	jmp moveSpritesNext			; skip to next sprite

; ****************************************************************************

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

.moveSpritesRandomOrAttack

	stx moveSpritesSaveX			; preserve X

	txa					; adjust X with enemyAttack value
	clc
	adc optionEnemyAttack
	tax

	jsr random				; compare random number with enemyRandomChance table
	cmp enemyRandomChance - 1, x

	ldx moveSpritesSaveX			; restore X

	bcs moveSpritesAttack			; if random < random chance table

.moveSpritesRandom

	jsr random				; then pick a random direction
	and #&03
	tay

	bpl moveSpritesFindPath			; go check if this direction is a path
	
.moveSpritesAttack

	jsr random				; else choose random attack check order, either up/down/left/right or left/right/up/down
	bmi moveSpritesAttackReversedOrder

	jsr moveSpritesCheckRightLeft		; first check right and left
	jsr moveSpritesCheckUpDown		; second check up and down
	jmp moveSpritesFindPath
	
.moveSpritesAttackReversedOrder

	jsr moveSpritesCheckUpDown		; first check up and down
	jsr moveSpritesCheckRightLeft		; second check right and left

.moveSpritesFindPath

	sty moveSpritesSaveDirection		; save current direction
	
	lda moveDirMap, y			; get direction offset for tile map

	tay					; if tileMap is a wall then try again
	lda (tileMapAddr), y
	bmi moveSpritesRandom

	lda moveSpritesSaveDirection		; get saved direction
	sta spritesDir, x			; set sprite direction

.moveSpritesNext

	inc moveSpritesIndex			; repeat until all sprites have been processed
	lda moveSpritesIndex
	cmp #spritesTotal
	beq moveSpritesExit
	jmp moveSpritesLoop

.moveSpritesExit

	if debugRaster				; if debug then set background color

	lda #palBlack
	sta ulaPalette

	endif

	rts					; return

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesCheckUpDown

	lda spritesY, x
	cmp spritesY + 0
	beq moveSpritesCheckUpDownExit		; if enemyY = ladybugY then return with current direction unchanged

	ldy #moveDown				; else choose down or up
	bcc moveSpritesCheckUpDownExit
	ldy #moveUp

.moveSpritesCheckUpDownExit

	rts

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.moveSpritesCheckRightLeft

	lda spritesX, x
	cmp spritesX + 0
	beq moveSpritesCheckRightLeftExit	; if enemyX = ladybugX then return with current direction

	ldy #moveRight				; else choose right or left
	bcc moveSpritesCheckRightLeftExit
	ldy #moveLeft

.moveSpritesCheckRightLeftExit

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawPlayfieldUpper				draws the top bar with text "special" "extra" "*2*3*5"
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawPlayfieldUpper

	lda #lo(screenAddr)
	sta drawMapTileAddr
	lda #hi(screenAddr)
	sta drawMapTileAddr + 1
	
	lda #extraTileUpper			; red left
	jsr drawExtraTile

	ldx #7
	
.drawPlayfieldUpperLoop1

	lda #extraTileUpper + 1			; red top
	jsr drawExtraTile

	dex
	bne drawPlayfieldUpperLoop1
	
	lda #lo(screenAddr + 16 + 7 * chrColumn)
	sta drawMapTileAddr
	lda #hi(screenAddr + 16 + 7 * chrColumn)
	sta drawMapTileAddr + 1
	
	lda #extraTileUpper + 2			; red right
	jsr drawExtraTile

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	lda #extraTileUpper + 3			; yellow left
	jsr drawExtraTile

	ldx #5
	
.drawPlayfieldUpperLoop2

	lda #extraTileUpper + 4			; yellow top
	jsr drawExtraTile

	dex
	bne drawPlayfieldUpperLoop2

	lda #lo(screenAddr + 8 + 14 * chrColumn)
	sta drawMapTileAddr
	lda #hi(screenAddr + 8 + 14 * chrColumn)
	sta drawMapTileAddr + 1

	lda #extraTileUpper + 5			; yellow right
	jsr drawExtraTile

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	lda #extraTileUpper + 6			; cyan left
	jsr drawExtraTile

	ldx #6
	
.drawPlayfieldUpperLoop3

	lda #extraTileUpper + 7			; cyan top
	jsr drawExtraTile

	dex
	bne drawPlayfieldUpperLoop3

	lda #lo(screenAddr + 22 * chrColumn)
	sta drawMapTileAddr
	lda #hi(screenAddr + 22 * chrColumn)
	sta drawMapTileAddr + 1

	lda #extraTileUpper + 8			; cyan right
	jsr drawExtraTile

	jsr drawString				; draw the 3 multipliers
	equb pixelCol6
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

.drawPlayfieldUpperBonus

	stx drawPlayfieldUpperBonusSaveX	; save x

	jsr drawString				; set address
	equb pixelCol7
	equw screenAddr + 2 + 16
	equb &ff
	
	lda bonusBits				; copy bonus bits (shifting everything left one bit to drop the unused bit 15)
	asl a
	sta bonusBitsTemp
	lda bonusBits + 1
	rol a
	sta bonusBitsTemp + 1

	ldx #0					; index for bonus tables

.drawPlayfieldUpperText

	lda #pixelCol7				; if bit = 1 use white
	bit bonusBitsTemp + 1
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
	lda #0
	adc drawChrAddr + 1
	sta drawChrAddr + 1
	
	asl bonusBitsTemp			; shift to next bonus bit
	rol bonusBitsTemp + 1

	inx					; repeat until all chrs done
	cpx #15
	bne drawPlayfieldUpperText

	ldx drawPlayfieldUpperBonusSaveX	; restore x

	rts					; return

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.upperBonusText

	equs "SPECIAL"
	equs "EXTRA"
	equs chrMultiplier2, chrMultiplier3, chrMultiplier5
	
.upperBonusOffset

	equb 0,0,0,0,0,0,40			; special offsets
	equb 0,0,0,0,64				; extra offsets
	equb 24,24,0				; *2 *3 *5 offsets (last byte is used but has no effect as no more chrs printed so could be removed)

.upperBonusColor				; colors for special, extra, 235

	equb pixelColA, pixelColB, pixelColA, pixelColB, pixelColA, pixelColB, pixelColA
	equb pixelColD, pixelColC, pixelColD, pixelColC, pixelColD
	equb pixelCol6, pixelCol6, pixelCol6

bonusBitsSpecial	= &7f			; bit mask for special bits on bonusBits + 1
bonusBitsExtra		= &f8			; bit mask for extra bits on bonusBits + 0
bonusBitsMultiplier	= &07			; bit mask for x2x3x5 multiplier bits on bonusBits + 0


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawPlayfieldLower				draws the bottom info panel showing lives, vegetable, level, score, highScore, highScore name
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

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawPlayfieldLowerLives		; draw 2 digit lives value

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	lda #34					; draw vegetable
	sta drawSpriteX
	lda #23 * 8 + 6
	sta drawSpriteY
	jsr drawVegetable

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawString				; draw 4 spaces to erase old vegetable value
	equb pixelCol2
	equw screenAddr + 2 + 16 + 5 * chrColumn + 24 * chrRow
	equs "    ", &ff

	jsr drawString				; draw vegetable value
	equb pixelCol2
	equw screenAddr + 2 + 16 + 5 * chrColumn + 24 * chrRow
	equb &ff

	lda vegetableScore			; 2 digit vegetable score
	jsr drawHex
	lda #&00				; 2 digit 00
	jsr drawHex

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawString				; "L"
	equb pixelCol4
	equw screenAddr + 2 + 8 + 10 * chrColumn + 24 * chrRow
	equs "L", &ff

	jsr drawString				; draw 2 spaces to erase old level value
	equb pixelCol5
	equw screenAddr + 2 + 8 + 11 * chrColumn + 24 * chrRow
	equs "  ", &ff

	jsr drawString				; draw 2 digit level number
	equb pixelCol5
	equw screenAddr + 2 + 8 + 11 * chrColumn + 24 * chrRow
	equb &ff

	lda level
	jsr drawHex

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawString				; "1P"
	equb pixelCol3
	equw screenAddr + 2 + 16 + 13 * chrColumn + 24 * chrRow
	equs "1P", &ff
	
						; draw 0 digit tile
	lda #lo(screenAddr + 22 * chrColumn + 24 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 22 * chrColumn + 24 * chrRow)
	sta drawMapTileAddr + 1

	lda #0
	jsr drawExtraTile

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawString				; draw 10 spaces to erase old highScore name
	equb pixelCol1
	equw screenAddr + 2 + 16 + 5 * chrColumn + 25 * chrRow
	equs "          ", &ff

	jsr drawString				; draw high score name in red
	equb pixelCol1
	equw screenAddr + 2 + 16 + 5 * chrColumn + 25 * chrRow
	equb &ff
	
	lda #lo(highScoreName)
	sta drawTextAddr
	lda #Hi(highScoreName)
	sta drawTextAddr + 1
	
	jsr drawText

	jmp drawHighScore			; draw highScore and exit



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawPlayfieldLowerLives			; erase and redraw the lives value
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawPlayfieldLowerLives

	jsr drawString				; draw 2 spaces to erase old lives value
	equb pixelCol3
	equw screenAddr + 2 + 16 + 1 * chrColumn + 24 * chrRow
	equs "  ", &ff

	jsr drawString				; draw 2 digit lives
	equb pixelCol3
	equw screenAddr + 2 + 16 + 1 * chrColumn + 24 * chrRow
	equb &ff

	lda lives
	jmp drawHex				; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawPlayfieldMiddle				draw middle playfield tiles (mapTiles and objectTiles)
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
	
	ldx #0
	
.drawPlayfieldMiddleLoop0

	lda tileMap, x				; draw 1st &100 of &211 bytes
	jsr drawMapTile
	
	inx
	bne drawPlayfieldMiddleLoop0
	
.drawPlayfieldMiddleLoop1			; draw 2nd &100 of &211 bytes

	lda tileMap + &100, x
	jsr drawMapTile
	
	inx
	bne drawPlayfieldMiddleLoop1
	
.drawPlayfieldMiddleLoop2

	lda tileMap + &200, x			; draw last &11 of &211 bytes
	jsr drawMapTile
	
	inx
	cpx #&11
	bne drawPlayfieldMiddleLoop2
	
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawObjectScore				draws object score img at xy if enabled
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawObjectScore

	lda objectScoreX			; copy objectScorexy ready for drawing
	sta drawSpriteX
	lda objectScoreY
	sta drawSpriteY
	
	lda objectScoreImg			; if objectScoreImg != 0 (active)
	bne drawSprite10x10			; then draw it
	
	rts					; else return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawVegetableCenter				draw vegetable sprite in center bug box (if active) or draw diamond sprite (if active and correct level)
; drawVegetable					draw vegetable sprite from current address
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawVegetableCenter

	lda vegetableActive			; if the vegetable not active
	beq drawVegetableCenterExit		; then exit

	lda enemysActive			; if maximum number of enemys are active
	cmp #spritesTotal - 1
	beq drawVegetableCenterActive

.drawVegetableCenterExit

	rts
	
.drawVegetableCenterActive

	lda #centerBoxX				; then we can draw vegetable at the center
	sta drawSpriteX
	lda #centerBoxY + 2
	sta drawSpriteY

	lda bonusDiamondEnable			; if bonusDiamondEnable == true
	beq drawVegetable

	lda level				; if level >= bonusDiamondLevel
	cmp #bonusDiamondLevel
	bcc drawVegetable
	
	lda #bonusDiamondImg			; then draw a diamond
	bne drawSprite10x10

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawVegetable

	lda vegetableImg			; get vegetable img

.drawSprite10x10

	sta drawSpriteImg			; use it for sprite

	stx drawSpriteSaveX			; save registers
	sty drawSpriteSaveY

	lda #0					; draw column from 0
	sta drawSpriteColumnVinit + 1

	lda #sprite10x10Height			; to vegetableTileHeight - 1
	sta drawSpriteColumnVtest + 1

	sta drawSpriteColumnTileHeight + 1	; set tile height to vegetableTileHeight
	
	lda #opcodeINX				; drawing normal so use INX instruction
	sta drawSpriteNextLineInstruction

	lda #sprite10x10Bytes			; store number of bytes in sprite in counter
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
; workspace		drawSpriteScreenAddr	calculated screen position
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; note 3 will be automatically added to spriteImg when the x position requires a pixel shifted image
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

;-----------------------------------------------------------------------------------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawSpriteGetX

	jsr spriteToScreen			; convert sprite XY to screen address
	bcs drawSpriteGetIdAddr

	jmp drawSpriteNotDrawn

.drawSpriteGetIdAddr

	lda drawSpriteImg			; get sprite data address from spriteImg address table

	tay
	lda spriteImgAddrLo, y
	sta drawSpriteRead + 1
	lda spriteImgAddrHi, y
	sta drawSpriteRead + 2

	if debugRaster				; if debug set background color

	lda #palCyan
	sta ulaPalette

	endif

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
	; data value of following instruction modified by drawSprite/drawSpriteVflip/drawVegetable
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #&ff

.drawSpriteRead

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; sprite source address written into the following instruction
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda addr16, x				; read byte from sprite

.drawSpriteWrite	

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; screen destination address written into the following instruction
	;---------------------------------------------------------------------------------------------------------------------------------------------

	sta addr16, y				; write byte to screen

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

.drawSpriteNextLineInstruction

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; the following instruction is replaced with inx/dex by drawSprite/drawSpriteVflip/drawVegetable
	;---------------------------------------------------------------------------------------------------------------------------------------------

	nop

.drawSpriteColumnVtest

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; the value in the following instruction is replaced by drawSprite/drawSpriteVflip/drawVegetable
	;---------------------------------------------------------------------------------------------------------------------------------------------

	cpx #&ff
	bne drawSpriteRead			; if still processing column then continue reading and writing sprite data
	
	clc					; adjust sprite address for next column
	lda drawSpriteRead + 1

.drawSpriteColumnTileHeight

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; the value in the following instruction modified by drawSprite/drawSpriteVflip/drawVegetable
	;---------------------------------------------------------------------------------------------------------------------------------------------

	adc #spriteTileHeight
	sta drawSpriteRead + 1
	lda drawSpriteRead + 2
	adc #0
	sta drawSpriteRead + 2

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


.drawSpriteNotDrawn

	clc					; exit with not drawn status
	bcc drawSpriteExit



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawVegetableScore				draws vegetable score in the center box if enabled
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawVegetableScore

	lda vegetableScoreActive		; if vegetableScoreActive != 0 (active)
	beq drawVegetableScoreExit

						; set draw position to the center box

	lda #lo(screenAddr + vegetableScoreX * chrColumn + vegetableScoreY * chrRow)
	sta drawChrMiniAddr
	lda #hi(screenAddr + vegetableScoreX * chrColumn + vegetableScoreY * chrRow)
	sta drawChrMiniAddr + 1

	

	lda vegetableScore			; draw the top 2 digits
	jsr drawHexMini
	lda #0					; draw 00
	jsr drawHexMini

.drawVegetableScoreExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
.page0287End
;-----------------------------------------------------------------------------------------------------------------------------------------------------



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; page 0d00
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org page0d00


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; nmi interrupt					it shouldnt happen but just in case it does
;
;						then again if something generated an nmi and requires some hardware register write
;						to clear it then we're screwed anyway as we'll have an infinite stream of nmi's
;						and the game will freeze but I guess freezing is better than crashing ?
;
;						we'll set the background color to red to show that an nmi was received
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.nmiInterrupt

	pha					; save A

	lda #palRed				; set background color to show that an nmi was received
	sta ulaPalette

	pla					; restore A
	rti					; return



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

;-----------------------------------------------------------------------------------------------------------------------------------------------------

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

	lda #diamondBonusScore			; add the diamond bonus score to score + 2 (bcd)
	sed
	clc
	bcc addScoreTop



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; addScoreSpecial				add special bonus to score
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.addScoreSpecial

	lda #specialBonusScore			; add the special bonus score to score + 2 (bcd)
	sed
	clc
	bcc addScoreTop



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; generateValidation				generate a validation code for the config data (game settings, high score, high score name)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.generateValidation

	ldx #0					; zero the validation code
	stx validationCode
	
.generateValidationLoop

	lda configData, x			; calculate validation code (validationCode += byte eor #&69)
	eor #&69
	clc
	adc validationCode
	sta validationCode
	
	inx
	cpx #configDataEnd - configData
	bne generateValidationLoop
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawHexMini					draw A as 2 digits of hexadecimal mini chr tiles (can be used for bcd numbers too)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			value to display as hex
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		stack			1 byte stack space
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			drawChrMiniAddr		points to next mini chr tile position on screen
;			A			destroyed
;			X			preserved
;			y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawHexMini

	pha					; save value
	
	lsr a					; draw high nybble
	lsr a
	lsr a
	lsr a
	jsr drawChrMini
	
	pla					; get value
	
	and #&0f				; draw low nybble

	; continue on into drawChrMini for the 2nd digit



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawChrMini					draw mini chr to screen, move to next chr position
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			A			chr to be drawn 0-9
;			drawChrMiniAddr		current screen location for chr
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		drawChrSaveX		storage to preserve X
;			drawChrSaveY		storage to preserve Y
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawChrMini

	stx drawChrSaveX			; save registers
	sty drawChrSaveY

	asl a					; tile address = A * 16 + miniFontBin
	asl a
	asl a
	asl a
	clc
	adc #lo(miniFontBin)
	sta drawChrMiniLoop + 1
	lda #0
	adc #hi(miniFontBin)
	sta drawChrMiniLoop + 2

	ldy #miniFontBytes - 1			; number of bytes to transfer
	
.drawChrMiniLoop

	lda addr16, y				; get byte from minifont
	
.drawChrMiniWrite

	sta addr16, y				; store it on screen
	
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
; drawMapTileAdjust				advance the map tile address by value in accumilator
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawMapTileAdjust

	clc					; add value in A to mapTileAddr
	adc drawMapTileAddr
	sta drawMapTileAddr
	lda #0
	adc drawMapTileAddr + 1
	sta drawMapTileAddr + 1
	
	rts					; return


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; levelAdvance					if level < 99 then increase by 1
;						if level < 20 then increase vegetable score by 500
;						if shield != 0 then reduce by 1
;						increase vegetable image by 1
;						if vegetable image >=0 then vegetable image = 0
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.levelAdvance

	sed					; switch to bcd mode

	lda level				; if level < &99
	cmp #&99
	bcs levelAdvanceVegetableScore

	adc #1					; add 1 to level (carry already clear from previous CMP)
	sta level
	
.levelAdvanceVegetableScore	

	lda level				; if level < &19
	cmp #&19
	bcs levelAdvanceshield
	
	lda vegetableScore			; add 500 to vegetable score value (carry already clear from previous test)
	adc #5
	sta vegetableScore
	
.levelAdvanceshield

	cld					; switch to binary mode

	lda shield				; if shield != 0 then decrement it
	beq levelAdvanceVegetableImage

	dec shield

.levelAdvanceVegetableImage

	inc vegetableImg			; add 1 to vegetable imgage

	lda vegetableImg			; if vegetableImg >= 18
	cmp #18
	bcc levelAdvanceExit
	
	lda #0					; then vegetableImg = 0 (cucumber)
	sta vegetableImg

.levelAdvanceExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; updatePauseTimers				; update ladybug and enemy pause timers
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updatePauseTimers

	lda vsyncCounter			; if vsync counter and 1 = 0
	and #1
	bne updatePauseTimersExit

	lda pauseLadybug			; if pauseLadybug timer > 0 then decrement
	beq updatePauseTimersEnemy
	dec pauseLadybug

	bne updatePauseTimersEnemy		; if pauseLadybug timer == 0 then
	
	lda spritesDir + 0			; unblank ladybug sprite
	and #spriteBlanking eor &ff
	sta spritesDir + 0

	lda #0					; disable object score display
	sta objectScoreImg

	lda objectScoreX			; erase object score from screen
	sta drawSpriteX
	lda objectScoreY
	sec
	sbc #2
	sta drawSpriteY
	jsr eraseBlock

	lda vegetableScoreActive		; if vegetable score was also active then
	beq updatePauseTimersEnemy
	
	lda #0					; deactive vegetable score display
	sta vegetableScoreActive
	
						; position to left wall of center box

	lda #lo(screenAddr + vegetableScoreX * chrColumn + vegetableScoreY * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + vegetableScoreX * chrColumn + vegetableScoreY * chrRow)
	sta drawMapTileAddr + 1
	
	lda #mapTileVerticalBar			; draw vertical wall
	jsr drawMapTile
	
	lda #mapTileBlank			; draw blank
	jsr drawMapTile
	
	lda #mapTileVerticalBar			; draw vertical wall
	jsr drawMapTile

.updatePauseTimersEnemy

	lda pauseEnemy				; if pauseEnemy timer > 0 then decrement
	beq updatePauseTimersExit
	dec pauseEnemy
	
.updatePauseTimersExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; updateAnimationFrame				; update the frame number for sprite animation
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateAnimationFrame

	dec spritesImgFrameCounter		; bump animation timer
	bne updateAnimationFrameExit
	
	lda #spritesAnimationSpeed		; reset animation timer on zero
	sta spritesImgFrameCounter
	
	dec spritesImgFrame			; bump animation frame
	bpl updateAnimationFrameExit

	lda #3					; reset animation frame on negative
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

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.instructions

	jsr playSoundSilence			; kill any current sounds

	jsr playfieldMiddleWithTimer		; initialize and draw playfield with only timer

	jsr initSprites				; initialize all sprites as blanked and erased

	lda #&f0 + palRed			; set special letters to red
	sta ulaPalette

	jsr drawFlowers				; draw two random flowers

	jsr drawString				; draw instruction text
	equb pixelCol1
	equw screenAddr + 2 + 8 + 5 * chrColumn + 4 * chrRow
	equs "INSTRUCTIONS", &ff
	
	lda #lo(screenAddr + 8 + 2 * chrColumn + 7 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 8 + 2 * chrColumn + 7 * chrRow)
	sta drawMapTileAddr + 1
	lda #mapTileBlueHeart
	jsr drawMapTile

	lda #column				; advance 1 column
	jsr drawMapTileAdjust
	lda #mapTileBlueHeart
	jsr drawMapTile

	lda #column				; advance 1 column
	jsr drawMapTileAdjust
	lda #mapTileBlueHeart
	jsr drawMapTile
	
	jsr drawString
	equb pixelCol2
	equw screenAddr + 2 + 16 + 6 * chrColumn + 7 * chrRow
	equs "MULTIPLY SCORE", &ff

	lda #lo(screenAddr + 8 + 3 * chrColumn + 10 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 8 + 3 * chrColumn + 10 * chrRow)
	sta drawMapTileAddr + 1

	lda #lo(screenAddr + 8 + 3 * chrColumn + 10 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 8 + 3 * chrColumn + 10 * chrRow)
	sta drawMapTileAddr + 1

	ldx #0

.instructionsLettersLoop

	lda instructionsLetters, x

	bmi instructionsLettersAddrAdjust

	jsr drawMapTile

.instructionsLettersAddrAdjust

	lda #column				; advance 1 column
	jsr drawMapTileAdjust
	
	inx

	cpx #15
	bne instructionsLettersLoop

	jsr drawString
	equb pixelCol2
	equw screenAddr + 2 + 3 * chrColumn + 11 * chrRow
	equs "COLLECT FOR BONUS", &ff

	jsr drawString
	equb pixelCol2
	equw screenAddr + 2 + 5 * chrColumn + 12 * chrRow
	equs "GARDEN PRIZES", &ff

	jsr drawString
	equb pixelCol5
	equw screenAddr + 2 + 6 * chrColumn + 15 * chrRow
	equs "DURING GAME", &ff

	jsr drawString
	equb pixelCol3
	equw screenAddr + 2 + 5 * chrColumn + 16 * chrRow
	equs "RETURN PAUSES", &ff

	jsr drawString
	equb pixelCol1
	equw screenAddr + 2 + 8 + 3 * chrColumn + 17 * chrRow
	equs "HOLD ESC TO QUIT", &ff

	jsr drawString
	equb pixelColE
	equw screenAddr + 2 + 8 + 6 * chrColumn + 20 * chrRow
	equs "START GAME", &ff

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

	cmp #keyBitStart			; if start key was pressed then exit with false status
	beq instructionsReturnFalse

	cmp #keyBitEsc				; if esc key was pressed then exit with true status
	beq instructionsReturnTrue
	
	bne instructionsRelease			; else loop back and wait for key release

.instructionsReturnFalse

	jsr playSoundSilence			; kill any current sounds

	clc					; return false
	rts

.instructionsReturnTrue

	jsr playSoundSilence			; kill any current sounds

	lda #sfxSkull				; play skull sound effect
	jsr playSound

	sec					; return true
	rts

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.instructionsFunctions

	jsr instructionsLadybugAnimation	; do the animation

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; wait upper sync
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr waitVsyncUpper			; wait upper half

	jsr animateLadybug			; update ladybug direction and frame counter
	
	jsr redrawSprites			; draw sprites

	jsr moveSprites				; move sprites

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; wait lower sync
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr waitVsyncLower			; wait lower half

	jsr processSound			; process sound effects and music

	jsr redrawSprites			; draw sprites

	jsr updateAnimationFrame		; update the animtion frame number

	jsr updateSkullColor			; update the skull palette color

	jsr updateBonusColor			; update the bonus palette colors

	jsr drawScore				; draw score (1 digit per loop)

	jsr keyboardScan			; scan keyboard inputs

	lda playerInput				; return with input bits

	rts

;-----------------------------------------------------------------------------------------------------------------------------------------------------

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

	equb -2,  0, -2, -1,  0, -1
	equb  2,  0,  2, -1,  0, -1

;-----------------------------------------------------------------------------------------------------------------------------------------------------

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

	lda #spriteBlanking			; erase all enemys
	sta spritesDir + 1
	sta spritesDir + 2
	sta spritesDir + 3
	sta spritesDir + 4

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
	
	ldy #0					; redraw 9 tile background
	lda (tileMapAddr), y
	jsr drawMapTile
	iny
	lda (tileMapAddr), y
	jsr drawMapTile
	iny
	lda (tileMapAddr), y
	jsr drawMapTile

	lda #lo(chrRow - 3 * chrColumn)
	clc
	adc drawMapTileAddr
	sta drawMapTileAddr
	lda #hi(chrRow - 3 * chrColumn)
	adc drawMapTileAddr + 1
	sta drawMapTileAddr + 1
	
	ldy #23
	lda (tileMapAddr), y
	jsr drawMapTile
	iny
	lda (tileMapAddr), y
	jsr drawMapTile
	iny
	lda (tileMapAddr), y
	jsr drawMapTile

	lda #lo(chrRow - 3 * chrColumn)
	clc
	adc drawMapTileAddr
	sta drawMapTileAddr
	lda #hi(chrRow - 3 * chrColumn)
	adc drawMapTileAddr + 1
	sta drawMapTileAddr + 1
	
	ldy #46
	lda (tileMapAddr), y
	jsr drawMapTile
	iny
	lda (tileMapAddr), y
	jsr drawMapTile
	iny
	lda (tileMapAddr), y
	jsr drawMapTile

	lda spriteBaseImg + 9			; draw angel sprite
	sta drawSpriteImg
	lda spritesX + 0
	sta drawSpriteX
	lda spritesY + 0
	sta drawSpriteY

	lda pauseLadybug
	and #4
	bne ladybugDeathAnimationDrawAngelSprite
	
	inc drawSpriteImg

.ladybugDeathAnimationDrawAngelSprite

	jsr drawSprite

	lda pauseLadybug			; if its time to move
	cmp #256 - ladybugDeathFlashTime - ladybugDeathWaitTime
	bcs ladybugDeathAnimationDrawAngelExit

	ldx ladybugDeathAnimationIndex		; adjust angel xy postion and clip within map area

	lda spritesX + 0
	clc
	adc ladybugDeathAnimationTable, x
	cmp #1 * 8
	bcs ladybugDeathAnimationLimitXhi
	lda #1 * 8

.ladybugDeathAnimationLimitXhi

	cmp #21 * 8
	bcc ladybugDeathAnimationLimitXstore

	lda #21 * 8

.ladybugDeathAnimationLimitXstore
	sta spritesX + 0
	inx

	lda spritesY + 0
	clc
	adc ladybugDeathAnimationTable, x
	cmp #8
	bcs ladybugDeathAnimationLimitYstore

	lda #8

.ladybugDeathAnimationLimitYstore

	sta spritesY + 0
	inx

	lda vsyncCounter
	and #7
	bne ladybugDeathAnimationDrawAngelExit
	
	cpx #12
	bne ladybugDeathAnimationFrame

	ldx #0
	
.ladybugDeathAnimationFrame

	stx ladybugDeathAnimationIndex

.ladybugDeathAnimationDrawAngelExit

	jmp ladybugDeathAnimationCheckMusic



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; keyboardScanFull				scan all keys
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			C			clear if no key pressed
;			C			set if key was pressed
;			A			key index if key was pressed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.keyAsciiCodes

	equs "0123456789-^\", chrLeft, chrRight
	equs "QWERTYUIOP@[_", chrUp, chrDown
	equs "ASDFGHJKL;:]"
	equs "ZXCVBNM,./"

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.keyScanCodes

	equb key0, key1, key2, key3, key4, key5, key6, key7, key8, key9, keyMinus, keyRaise, keyBackslash, keyLeft, keyRight
	equb keyQ, keyW, keyE, keyR, keyT, keyY, keyU, keyI, keyO, keyP, keyAt, keyBracketOpen, keyUnderscore, keyUp, keyDown
	equb keyA, keyS, keyD, keyF, keyG, keyH, keyJ, keyK, keyL, keySemicolon, keyColon, keyBracketClosed
	equb keyZ, keyX, keyC, keyV, keyB, keyN, keyM, keyComma, keyPeriod, keySlash

.keyScanCodesEnd

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.keyboardScanFull

	stx keyboardScanFullSaveX		; save register

	lda #&7f				; set port A bit 7 as input ( from keyboard output )
	sta viaPortDdrA
	
	lda #3 + 0				; keyboard -enable low
	sta viaPortB
	
	ldx #(keyScanCodesEnd - keyScanCodes) - 1; start at end of table
	
.keyboardScanFullLoop

	lda keyScanCodes, x			; get key scan code from table and check key is pressed
	jsr keyboardScanFullKey

	bmi keyboardScanFullPressed		; if pressed then exit with scancode

	dex					; else try next code until all tested
	bpl keyboardScanFullLoop

	lda #3 + 8				; keyboard -enable high
	sta viaPortB
	
	ldx keyboardScanFullSaveX		; restore register

	clc					; no key pressed so return with false
	rts

.keyboardScanFullPressed

	lda #3 + 8				; keyboard -enable high
	sta viaPortB
	
	txa					; A = scan index
	
	ldx keyboardScanFullSaveX		; restore register

	sec					; return true
	rts

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.keyboardScanFullKey

	sta viaPortA				; select key

	lda viaPortA				; read key status and return
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; initLevelSettings				setup num dots, letters, skulls and enemy settings etc, everything for the current level
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.enemySpeedTable

						; enemy speed option 0
	equb 256 * 0.00				; level 1-6
	equb 256 * 0.05				; level 7-11
	equb 256 * 0.10				; level 12-17
	equb 256 * 0.15				; level 18-99
	
						; enemy speed option 1
	equb 256 * 0.00				; level 1-6
	equb 256 * 0.07				; level 7-11
	equb 256 * 0.14				; level 12-17
	equb 256 * 0.21				; level 18-99
	
						; enemy speed option 2
	equb 256 * 0.05				; level 1-6
	equb 256 * 0.13				; level 7-11
	equb 256 * 0.21				; level 12-17
	equb 256 * 0.29				; level 18-99

						; enemy speed option 3
	equb 256 * 0.10				; level 1-6
	equb 256 * 0.20				; level 7-11
	equb 256 * 0.30				; level 12-17
	equb 256 * 0.40				; level 18-99
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------

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

	lda optionEnemySpeed			; index = optionEnemySpeed * 4
	asl a
	asl a
	tax

	lda level				; get current level (bcd)

	cmp #&07				; if level >= 7 then index++
	bcc initLevelSettingsSpeed
	inx

	cmp #&12				; if level >= 12 the index++
	bcc initLevelSettingsSpeed
	inx

	cmp #&18				; if level >= 18 then index++
	bcc initLevelSettingsSpeed
	inx

.initLevelSettingsSpeed

	lda enemySpeedTable, x			; enemySpeed = enemySpeedTable[index]
	sta enemySpeed

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; set enemy timer speed
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda level				; get current level (bcd)

	ldx #&08				; 8 frames
	cmp #&02				; level 1
	bcc initLevelSettingsTimer
	
	ldx #&05				; 5 frames
	cmp #&05				; level 2-4
	bcc initLevelSettingsTimer
	
	ldx #&03				; 3 frames level 5-99

.initLevelSettingsTimer

	stx enemyTimerSpeed			; set enemy timer speed
	stx enemyTimerSpeedCounter
	
	lda #0					; zero enemy timer position
	sta enemyTimer
	sta enemyReleaseEnable			; disable enemy release

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; set object mode to start with red
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #objectModeCyan			; set mode to cyan and timer to 1 so that the color and mode instantly changes to red
	sta objectMode
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

	if debugDiamondBonus == false

	sta bonusDiamondActive			; diamond bonus deactivated

	endif

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; level not ended yet
	;---------------------------------------------------------------------------------------------------------------------------------------------

	sta levelEnd

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; game not paused
	;---------------------------------------------------------------------------------------------------------------------------------------------

	sta pauseGame

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; clear turnstile address
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #&ff
	sta drawTurnstileAddr
	sta drawTurnstileAddr + 1

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; choose random letters
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr chooseLetters			; choose 3 random letters

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; mainMenu
;
;					this is a huge mess and needs rewriting much smaller
;
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.optionSettingsMin

	equb  0,  0,  1,  0,  0			; minimum value for speed, attack, lives, sound, volume
	
.optionSettingsMax

	equb  4, 10, 10,  2,  4			; maximum value + 1 for speed, attack, lives, sound, volume

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenu

	jsr playfieldMiddleWithTimer		; initialize and draw playfield with only timer

	jsr initSprites				; initialize all sprites as blanked and erased

	jsr mainMenuDrawLogo			; draw the ladybug logo

	jsr drawFlowers				; draw the random flowers

	jsr mainMenuDrawText			; draw the menu text

	jsr mainMenuDrawSettings		; draw the current settings

	jsr mainMenuDrawEnemys			; place 4 enemys on screen

	lda #0					; make sure the "START GAME" text is flashing (skull color) by setting shield to 0
	sta shield

	sta pauseLadybug			; unpause ladybug so that it will animate

	sta mainMenuCursor			; select first entry
	sta mainMenuCursorOld			; make old cursor same for now

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; update cursor
	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuUpdateCursor

	jsr mainMenuDrawCursor			; draw updated cursor

	jsr generateValidation			; update the validation code

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; process functions and wait key release
	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuWaitRelease

	jsr mainMenuFunctions			; update colors and scan keyboard

	bne mainMenuWaitRelease			; if key is pressed then loop back and wait for release
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; process functions and wait key press
	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuWaitPress

	jsr mainMenuFunctions			; update colors and scan keyboard

	beq mainMenuWaitPress			; if key not pressed then loop back and wait for key press

	jsr mainMenuProcess			; process the key pressed option

	bcc mainMenuUpdateCursor		; loop back until start game selected

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; return to start a new game
	;---------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuExit

	jmp generateValidation			; update the validation code and return (start a new game)



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; mainMenuLadybugAnimation			; if ladybug isnt animated then trigger an animation
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuLadybugAnimation

	lda animateLadybugActive		; if ladybug animation not active
	bne mainMenuLadybugAnimationExit
	
	lda #animateLadybugMainMenu		; then start the animation
	jsr animateLadybugInitialize

.mainMenuLadybugAnimationExit

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; mainMenuDrawCursor				erase old cursor and draw new
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuDrawCursor

	ldy mainMenuCursorOld			; erase old cursors
	clc
	lda screenRowLo, y
	adc #lo(2 + 3 * chrColumn + 12 * chrRow)
	sta drawChrAddr
	lda screenRowHi, y
	adc #hi(2 + 3 * chrColumn + 12 * chrRow)
	sta drawChrAddr + 1
	lda #' '
	jsr drawChr

	ldy mainMenuCursorOld			; erase old cursors
	clc
	lda screenRowLo, y
	adc #lo(2 + 19 * chrColumn + 12 * chrRow)
	sta drawChrAddr
	lda screenRowHi, y
	adc #hi(2 + 19 * chrColumn + 12 * chrRow)
	sta drawChrAddr + 1
	lda #' '
	jsr drawChr

	lda #pixelColA				; draw new cursors
	sta drawChrColor

	ldy mainMenuCursor
	clc
	lda screenRowLo, y
	adc #lo(2 + 3 * chrColumn + 12 * chrRow)
	sta drawChrAddr
	lda screenRowHi, y
	adc #hi(2 + 3 * chrColumn + 12 * chrRow)
	sta drawChrAddr + 1
	lda #chrRight
	jsr drawChr

	ldy mainMenuCursor			; draw new cursors
	clc
	lda screenRowLo, y
	adc #lo(2 + 19 * chrColumn + 12 * chrRow)
	sta drawChrAddr
	lda screenRowHi, y
	adc #hi(2 + 19 * chrColumn + 12 * chrRow)
	sta drawChrAddr + 1
	lda #chrLeft
	jsr drawChr

	lda mainMenuCursor			; copy new cursor to old
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

	jsr waitVsyncUpper			; wait upper half

	jsr animateLadybug			; update ladybug direction and frame counter
	
	jsr redrawSprites			; draw sprites

	jsr moveSprites				; move sprites

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; wait for lower sync and process
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr waitVsyncLower			; wait lower half

	jsr processSound			; process sound effects and music

	jsr redrawSprites			; draw sprites

	jsr updateAnimationFrame		; update the animtion frame number

	jsr updateSkullColor			; update the skull palette color

	jsr updateBonusColor			; update the bonus palette colors

	jsr drawScore				; draw score (1 digit per loop)

	jsr random				; keep polling random so that when the player starts a game the letters are truely random

	jsr keyboardScan			; scan keyboard inputs

	lda mainMenuCursor			; if cursor on the timer volume selection
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

	bmi mainMenuProcessUpNegative		; if mainMenuCursor < 0 then set it back to 7

	lda mainMenuCursor			; if mainMenuCursor == 1
	cmp #1
	bne mainMenuProcessUpExit

	lda #0					; then mainMenuCursor == 0
	beq mainMenuProcessUpExit

.mainMenuProcessUpNegative
	
	lda #7					; mainMenuCursor = 7
	
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

	lda mainMenuCursor			; if mainMenuCursor == 1
	cmp #1
	bne mainMenuProcessDownLast
	
	lda #2					; then mainMenuCursor == 2
	bne mainMenuProcessDownExit
	
.mainMenuProcessDownLast

	cmp #8					; if mainMenuCursor > 7
	bne mainMenuProcessDownExit
	
	lda #0					; then mainMenuCursor == 0

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

	ldx mainMenuCursor			; if mainMenuCursor != 0 then return true
	beq mainMenuProcessReturnTrue
	
	cpx #7					; if mainMenuCursor == 7 then redefine the keyboard
	beq mainMenuProcessKeyboard

	dex					; index = cursor - 2 as first adjustable setting is 2 away from the start game option
	dex

	inc optionEnemySpeed, x			; option[x] += 1

	lda optionEnemySpeed, x			; if option[x] == max[x]
	cmp optionSettingsMax, x
	bne mainMenuProcessStartExit
	
	lda optionSettingsMin, x		; then option[x] = min[x]
	sta optionEnemySpeed, x

.mainMenuProcessStartExit

	jsr mainMenuDrawSettings		; draw the updated settings

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	lda mainMenuCursor			; get cursor
	
	cmp #2					; if cursor == 2 (enemySpeed) then
	bne mainMenuProcessStartEnemyAttack
	
	jsr mainMenuDrawEnemys			; place 4 random enemys on screen
	
	clc					; return false
	rts

.mainMenuProcessStartEnemyAttack

	cmp #3					; if cursor == 3 (enemyAttack) then
	bne mainMenuProcessStartLadybugLives
	
	jsr mainMenuDrawEnemys			; place 4 random enemys on screen
	
	clc					; return false
	rts

.mainMenuProcessStartLadybugLives

	cmp #4					; if cursor == 4 (ladybugLives) then
	bne mainMenuProcessReturnFalse

	lda optionLadybugLives			; lives = optionLadybugLives
	sta lives

	jsr drawPlayfieldLowerLives		; update the lives value in the lower playfield

.mainMenuProcessReturnFalse

	clc					; return false
	rts
	
.mainMenuProcessReturnTrue

	sec
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; .mainMenuProcessKeyboard			; redefine the keys
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessKeyboard

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	jsr drawString				; erase text
	equb pixelColC
	equw screenAddr + 2 + 4 * chrColumn + 20 * chrRow
	equs "               ", &ff

	jsr drawString				; draw text
	equb pixelColC
	equw screenAddr + 2 + 4 * chrColumn + 20 * chrRow
	equs "PRESS", &ff

	jsr drawString				; draw text
	equb pixelColB
	equw screenAddr + 2 + 10 * chrColumn + 20 * chrRow
	equs "UP   ", &ff
	
	jsr mainMenuProcessKeyboardKey		; get key index
	
	tay					; get key scan code
	lda keyScanCodes, y

	sta optionKeys + 3			; store up key scan code

	lda #pixelCol7				; set color to white
	sta drawChrColor

	lda keyAsciiCodes, y			; get ascii chr of key

	sta optionKeysAscii + 3			; store in list

	jsr drawChr				; draw it on screen

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	jsr drawString				; erase test
	equb pixelColB
	equw screenAddr + 2 + 10 * chrColumn + 20 * chrRow
	equs "     ", &ff

	jsr drawString				; draw text
	equb pixelColB
	equw screenAddr + 2 + 10 * chrColumn + 20 * chrRow
	equs "DOWN ", &ff
	
.mainMenuProcessKeyboardDown

	jsr mainMenuProcessKeyboardKey		; get key index
	
	tay					; get key scan code
	lda keyScanCodes, y

	cmp optionKeys + 3			; if key is same as up then try again
	beq mainMenuProcessKeyboardDown

	sta optionKeys + 2			; store down key scan code

	jsr drawString				; position for printing
	equb pixelCol7
	equw screenAddr + 2 + 16 * chrColumn + 20 * chrRow
	equb &ff

	lda keyAsciiCodes, y			; get ascii chr of key

	sta optionKeysAscii + 2			; store in list

	jsr drawChr				; draw it on screen

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	jsr drawString				; erase test
	equb pixelColB
	equw screenAddr + 2 + 10 * chrColumn + 20 * chrRow
	equs "     ", &ff

	jsr drawString				; draw text
	equb pixelColB
	equw screenAddr + 2 + 10 * chrColumn + 20 * chrRow
	equs "LEFT ", &ff
	
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
	equb pixelCol7
	equw screenAddr + 2 + 17 * chrColumn + 20 * chrRow
	equb &ff

	lda keyAsciiCodes, y			; get ascii chr of key

	sta optionKeysAscii + 1			; store in list

	jsr drawChr				; draw it on screen

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	jsr drawString				; erase test
	equb pixelColB
	equw screenAddr + 2 + 10 * chrColumn + 20 * chrRow
	equs "     ", &ff

	jsr drawString				; draw text
	equb pixelColB
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
	equb pixelCol7
	equw screenAddr + 2 + 18 * chrColumn + 20 * chrRow
	equb &ff

	lda keyAsciiCodes, y			; get ascii chr of key

	sta optionKeysAscii + 0			; store in list

	jsr drawChr				; draw it on screen

	lda #sfxTurnstile			; play sound effect
	jsr playSound

	jsr drawString				; erase test
	equb pixelCol3
	equw screenAddr + 2 + 4 * chrColumn + 20 * chrRow
	equs "           ", &ff

	jsr drawString				; restore original text
	equb pixelCol3
	equw screenAddr + 2 + 4 * chrColumn + 20 * chrRow
	equs "CONTROLS   ", &ff

	clc					; return
	rts
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuProcessKeyboardKey

	jsr mainMenuFunctions			; update colors animation movement etc

	jsr keyboardScanFull			; wait for key release
	bcs mainMenuProcessKeyboardKey
	
.mainMenuProcessKeyboardKeyWaitPress

	jsr mainMenuFunctions			; update colors animation movement etc

	jsr keyboardScanFull			; wait for key press
	bcc mainMenuProcessKeyboardKeyWaitPress

	rts					; return with key press index in A



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; draw the ladybug logo
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuDrawLogoData

	equb -1,  0,  1, -1, -1,  2, -1, -1,  3,  4,  5, -1, -1, -1, -1
	equb  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
	equb 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35

;-----------------------------------------------------------------------------------------------------------------------------------------------------

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

	lda addr16				; get byte from logo tile data

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
; draw the menu text
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuDrawText

	jsr drawString
	equb pixelCol1
	equw screenAddr + 2 + 8 + 4 * chrColumn + 7 * chrRow
	equs "UNIVERSAL", chrCopyright, "1981", &ff

	jsr drawString
	equb pixelCol5
	equw screenAddr + 2 + 5 * chrColumn + 9 * chrRow
	equs "PROGRAMMED BY", &ff

	jsr drawString
	equb pixelCol3
	equw screenAddr + 2 + 8 + 5 * chrColumn + 10 * chrRow
	equs "LOVEBUG 2021", &ff

	jsr drawString
	equb pixelColE
	equw screenAddr + 2 + 8 + 6 * chrColumn + 13 * chrRow
	equs "START GAME", &ff

	jsr drawString
	equb pixelCol2
	equw screenAddr + 2 + 4 * chrColumn + 15 * chrRow
	equs "ENEMY SPEED", &ff
	
	jsr drawString
	equb pixelCol3
	equw screenAddr + 2 + 4 * chrColumn + 16 * chrRow
	equs "ENEMY ATTACK", &ff
	
	jsr drawString
	equb pixelCol2
	equw screenAddr + 2 + 4 * chrColumn + 17 * chrRow
	equs "LADYBUG LIVES", &ff
	
	jsr drawString
	equb pixelCol3
	equw screenAddr + 2 + 4 * chrColumn + 18 * chrRow
	equs "SOUND", &ff
	
	jsr drawString
	equb pixelCol2
	equw screenAddr + 2 + 4 * chrColumn + 19 * chrRow
	equs "TIMER VOLUME", &ff

	jsr drawString
	equb pixelCol3
	equw screenAddr + 2 + 4 * chrColumn + 20 * chrRow
	equs "CONTROLS", &ff
	
	jsr drawString
	equb pixelCol7
	equw screenAddr + 2 + 15 * chrColumn + 20 * chrRow
	equb &ff

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

	jsr drawString				; draw enemy speed
	equb pixelCol1
	equw screenAddr + 2 + 18 * chrColumn + 15 * chrRow
	equs " ", &ff
	
	jsr drawString
	equb pixelCol1
	equw screenAddr + 2 + 18 * chrColumn + 15 * chrRow
	equb &ff
	
	lda #pixelCol7
	sta drawChrColor
	lda optionEnemySpeed
	ora #'0'
	jsr drawChr
	
	jsr drawString				; draw enemy attack
	equb pixelCol2
	equw screenAddr + 2 + 18 * chrColumn + 16 * chrRow
	equs " ", &ff
	
	jsr drawString
	equb pixelCol2
	equw screenAddr + 2 + 18 * chrColumn + 16 * chrRow
	equb &ff
	
	lda #pixelCol7
	sta drawChrColor
	lda optionEnemyAttack
	ora #'0'
	jsr drawChr
	
	jsr drawString				; draw ladybug lives
	equb pixelCol3
	equw screenAddr + 2 + 18 * chrColumn + 17 * chrRow
	equs " ", &ff
	
	jsr drawString
	equb pixelCol3
	equw screenAddr + 2 + 18 * chrColumn + 17 * chrRow
	equb &ff
	
	lda #pixelCol7
	sta drawChrColor
	lda optionLadybugLives
	ora #'0'
	jsr drawChr
	
	jsr drawString				; draw sound on/off
	equb pixelCol4
	equw screenAddr + 2 + 16 * chrColumn + 18 * chrRow
	equs "   ", &ff
	
	lda optionSound
	beq mainMenuDrawSettingsMute

	jsr drawString
	equb pixelCol7
	equw screenAddr + 2 + 16 * chrColumn + 18 * chrRow
	equs " ON", &ff
	jmp mainMenuDrawSettingsTimerVolume
	
.mainMenuDrawSettingsMute

	jsr drawString
	equb pixelCol7
	equw screenAddr + 2 + 16 * chrColumn + 18 * chrRow
	equs "OFF", &ff

	jsr playSoundSilence			; also mute the psg chip

.mainMenuDrawSettingsTimerVolume
	
	jsr drawString				; draw timer volume
	equb pixelCol4
	equw screenAddr + 2 + 18 * chrColumn + 19 * chrRow
	equs " ", &ff
	
	jsr drawString
	equb pixelCol4
	equw screenAddr + 2 + 18 * chrColumn + 19 * chrRow
	equb &ff
	
	lda #pixelCol7
	sta drawChrColor
	lda optionTimerVolume
	ora #'0'
	jsr drawChr
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; .mainMenuDrawEnemys				; draw 4 enemys on screen
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuEnemysX

	equb 27, 149, 157, 19			; enemy x positions

.mainMenuEnemysY

	equb 69, 69, 50, 50			; enemy y positions

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mainMenuDrawEnemys

	jsr random				; pick a random direction and enemy for the draw loop

	ldx #3					; 4 enemys to draw
	
.mainMenuDrawEnemysLoop

	lda mainMenuEnemysX, x			; set X and Y position
	sta spritesX + 1, x
	lda mainMenuEnemysY, x
	sta spritesY + 1, x
	
	lda randomSeed + 1			; set sprite image
	and #7
	tay
	lda spriteBaseImg + 1, y

	sta spritesImg + 1, x
	
	lda randomSeed				; set sprite direction
	and #3
	ora #moveStop

	sta spritesDir + 1, x

	inc randomSeed				; increment direction for next sprite

	inc randomSeed + 1			; increment sprite image for next sprite

	dex					; repeat until all enemys places
	bpl mainMenuDrawEnemysLoop
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; draw two random flowers
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawFlowers

	lda #lo(screenAddr + 1 * chrColumn + 3 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 1 * chrColumn + 3 * chrRow)
	sta drawMapTileAddr + 1
	jsr drawRandomFlower

	lda #lo(screenAddr + 20 * chrColumn + 3 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 20 * chrColumn + 3 * chrRow)
	sta drawMapTileAddr + 1
	jmp drawRandomFlower



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
	
	jsr drawString				; draw 10 spaces to erase highScore name
	equb pixelCol1
	equw screenAddr + 2 + 16 + 5 * chrColumn + 25 * chrRow
	equs "          ", &ff

	jsr drawString				; draw paused message
	equb pixelColA
	equw screenAddr + 2 + 8 + 8 * chrColumn + 25 * chrRow
	equs "PAUSED", &ff
	
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
	
	jsr drawString				; draw 6 spaces to erase pause message
	equb pixelCol1
	equw screenAddr + 2 + 8 + 8 * chrColumn + 25 * chrRow
	equs "      ", &ff

	jsr drawString				; draw high score name in red
	equb pixelCol1
	equw screenAddr + 2 + 16 + 5 * chrColumn + 25 * chrRow
	equb &ff

	lda #lo(highScoreName)
	sta drawTextAddr
	lda #Hi(highScoreName)
	sta drawTextAddr + 1
	
	jsr drawText
	
	clc					; return false
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; nameReg				draw the name registration screen and get high score name
;
;					this is a huge mess and needs rewriting much smaller
;
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.nameRegText

	equs "ABCDEFGHIJKLMNOPQRSTUVWXYZ", chrHeart, "!. "

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.nameReg

	jsr playfieldMiddleWithTimer		; initialize and draw playfield with only timer

	jsr initSprites				; initialize all sprites as blanked and erased

	lda #nameRegTimer			; set enemy timer speed
	sta enemyTimerSpeed

	lda #0					; unpause enemys so that the timer will tick
	sta pauseEnemy

	sta pauseLadybug			; unpause ladybug so that it will animate

	sta shield				; clear shields so that skull color will sequence

	lda #spritesTotal - 1			; prevent enemy warning sound
	sta enemysActive

	jsr drawString
	equb pixelCol1
	equw screenAddr + 2 + 16 + 4 * chrRow + 3 * chrColumn
	equs "CONGRATULATIONS!", &ff

	jsr drawString
	equb pixelCol5
	equw screenAddr + 2 + 6 * chrRow + 5 * chrColumn
	equs "REGISTER YOUR", &ff

	jsr drawString
	equb pixelCol3
	equw screenAddr + 2 + 8 * chrRow + 4 * chrColumn
	equs "HIGH SCORE NAME", &ff

	jsr drawString
	equb pixelCol4
	equw screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn
	equs "----------", &ff

	jsr drawString
	equb pixelCol2
	equw screenAddr + 2 + 14 * chrRow + 4 * chrColumn
	equs "A B C D E F G H", &ff
	
	jsr drawString
	equb pixelCol2
	equw screenAddr + 2 + 16 * chrRow + 4 * chrColumn
	equs "I J K L M N O P", &ff

	jsr drawString
	equb pixelCol2
	equw screenAddr + 2 + 18 * chrRow + 4 * chrColumn
	equs "Q R S T U V W X", &ff

	jsr drawString
	equb pixelCol2
	equw screenAddr + 2 + 20 * chrRow + 4 * chrColumn
	equs "Y Z ", chrHeart, " ! .   ", &ff

	lda #pixelCol1				; draw delete in red
	sta drawChrColor
	lda #chrLeft
	jsr drawChr
	
	lda #' '
	jsr drawChr
	
	lda #pixelColE				; draw enter in skull color
	sta drawChrColor
	lda #chrDown
	jsr drawChr
	
	ldy #9					; clear high score name
	lda #' '
	
.nameRegClear

	sta highScoreName, y
	dey
	bpl nameRegClear

	lda #0					; position cursor over first letter
	sta nameRegCursor
	sta nameRegCursorOld

	sta nameRegCursorText			; position text cursor on 1st character

	lda #animateLadybugHighScore		; initialize the highScore animation
	jsr animateLadybugInitialize

	jsr nameRegCursorUpdate			; display the initial cursor position

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegWaitRelease

	jsr nameRegFunctions			; update timer, colors and scan keyboard

	bcs nameRegExit				; if enemy timer timed out then exit

	bne nameRegWaitRelease			; if key is pressed then loop back and wait for release
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegWaitPress

	jsr nameRegFunctions			; update timer, colors and scan keyboard

	bcs nameRegExit				; if enemy timer timed out then exit

	beq nameRegWaitPress			; if key not pressed then loop back and wait for key press

	jsr nameRegProcess			; process the key pressed functions

	bcc nameRegWaitRelease			; if end not selected then loop back and wait for key release

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegExit

	jsr playSoundSilence			; kill any sounds playing

	rts					; return

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegFunctions

	jsr waitVsyncUpper			; wait upper half

	jsr animateLadybug			; update ladybug direction and frame counter
	
	jsr redrawSprites			; draw ladybug

	jsr moveSprites				; move ladybug

	jsr waitVsyncLower			; wait lower half

	jsr processSound			; process sound effects and music

	jsr redrawSprites			; draw ladybug

	jsr updateAnimationFrame		; update the animtion frame number

	jsr updateEnemyTimer			; update the enemy timer and draw tile when needed

	jsr updateBonusColor			; update the bonus palette colors

	jsr updateSkullColor			; update the skull palette color

	lda enemyTimer				; if timeout complete then exit
	cmp #87
	beq nameRegTimeout
	
	jsr keyboardScan			; and with keyboard status
	lda playerInput

	clc					; exit with timer active status

	rts					; return

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegTimeout

	jsr keyboardScan			; and with keyboard status
	lda playerInput

	sec					; enemy timer timed out so exit with timeout status
	
	rts					; return

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessAdd32

	clc
	lda nameRegCursor
	adc #32
	sta nameRegCursor
	bpl nameRegProcessCursor

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessSub32

	sec
	lda nameRegCursor
	sbc #32
	sta nameRegCursor
	bpl nameRegProcessCursor

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcess

	;---------------------------------------------------------------------------------------------------------------------------------------------

.nameRegProcessUp

	lsr playerInput				; if up pressed
	bcc nameRegProcessDown
	
	lda #sfxMunch				; play sound effect
	jsr playSound

	sec
	lda nameRegCursor
	sbc #8
	sta nameRegCursor
	bpl nameRegProcessCursor

	bmi nameRegProcessAdd32

.nameRegProcessDown

	lsr playerInput				; if down pressed
	bcc nameRegProcessLeft
	
	lda #sfxMunch				; play sound effect
	jsr playSound

	clc
	lda nameRegCursor
	adc #8
	sta nameRegCursor

	cmp #32
	bcc nameRegProcessCursor

	bcs nameRegProcessSub32

.nameRegProcessLeft

	lsr playerInput				; if left pressed
	bcc nameRegProcessRight
	
	lda #sfxMunch				; play sound effect
	jsr playSound

	sec
	lda nameRegCursor
	sbc #1
	sta nameRegCursor
	bpl nameRegProcessCursor

	bmi nameRegProcessAdd32

.nameRegProcessRight

	lsr playerInput				; if right pressed
	bcc nameRegProcessStart
	
	lda #sfxMunch				; play sound effect
	jsr playSound

	clc
	lda nameRegCursor
	adc #1
	sta nameRegCursor

	cmp #32
	bcc nameRegProcessCursor

	bcs nameRegProcessSub32

.nameRegProcessStart

	lsr playerInput				; if start pressed
	bcc nameRegProcessCursor

	lda nameRegCursor			; if cursor = 31 (end)
	cmp #31
	bne nameRegProcessDelete

	sec					; exit with carry set (end)
	
	rts

.nameRegProcessCursor

	jsr nameRegCursorUpdate

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
	
	lda nameRegCursorText			; if cursor != 0
	beq nameRegProcessCursor

	dec nameRegCursorText			; move cursor back 1

	ldy nameRegCursorText			; erase chr from screen
	clc
	lda screenColumnLo, y
	adc #lo(screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn)
	sta drawChrAddr
	lda screenColumnHi, y
	adc #Hi(screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn)
	sta drawChrAddr + 1
	lda #' '
	jsr drawChr

	ldy nameRegCursorText			; print blue -
	clc
	lda screenColumnLo, y
	adc #lo(screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn)
	sta drawChrAddr
	lda screenColumnHi, y
	adc #Hi(screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn)
	sta drawChrAddr + 1
	lda #pixelCol4
	sta drawChrColor
	lda #'-'
	jsr drawChr

	ldy nameRegCursorText			; replace chr in string with space
	lda #' '
	sta highScoreName, y

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

	sta highScoreName, y			; store character in string

	clc					; erase chr from screen
	lda screenColumnLo, y
	adc #lo(screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn)
	sta drawChrAddr
	lda screenColumnHi, y
	adc #Hi(screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn)
	sta drawChrAddr + 1
	lda #' '
	jsr drawChr

	ldy nameRegCursorText			; print chr
	clc
	lda screenColumnLo, y
	adc #lo(screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn)
	sta drawChrAddr
	lda screenColumnHi, y
	adc #Hi(screenAddr + 2 + 8 + 11 * chrRow + 6 * chrColumn)
	sta drawChrAddr + 1
	lda #pixelCol7
	sta drawChrColor
	lda highScoreName, y
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
	lda #registrationH
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
	lda #registrationH
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

	lda ladybugEntryEnable			; if entry enabled then exit
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

	lda #letterBonusTime			; pause ladybug and enemys
	sta pauseLadybug
	sta pauseEnemy

	lda soundTimers + 0			; if sound effect not playing on channel 0
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
	
	lda #letterBonusTime			; pause ladybug and enemys
	sta pauseLadybug
	sta pauseEnemy

	lda soundTimers + 0			; if sound effect not playing on channel 0
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

	lda soundTimers + 0			; if sound effect not playing on channel 0
	bne checkBonusExit

	jsr playSoundSilence			; silence all effects and music

	jsr drawBonusScreen			; draw the diamond bonus screen

	jsr levelAdvance			; advance game to next level

	jsr addScoreDiamond			; add the diamond bonus score (bcd)

	lda #sfxExtraLife			; play the extra life sound
	jsr playSound

	if debugDiamondBonus
	
	lda #0
	sta bonusDiamondActive
	
	endif

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
	
	sed					; then add 1 to shield (bcd) as the call to levelAdvance will reduce shield by 1
	clc					; so the player does not lose a shield for gaining extra shields
	lda #1
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

	lda #sfxExtraLife			; play the extra life sound
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
	
	sed					; then add 1 to shield (bcd) as the call to levelAdvance will reduce shield by 1
	clc					; so the player does not lose a shield for gaining an extra life
	lda #1
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

	lda #sfxExtraLife			; play the extra life sound
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
	
	if debugRaster				; if debug set background color

	lda #palYellow
	sta ulaPalette

	endif

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

	eor addr16
	sta addr16

	sta changeTimerTileDraw + 1		; store tile number for drawing

	clc
	lda screenRowLo, y			; convert x and y to screen address
	adc screenColumnLo, x
	sta drawMapTileAddr
	lda screenRowHi, y
	adc screenColumnHi, x
	sta drawMapTileAddr + 1
	
.changeTimerTileDraw

	lda #&ff				; tile number (#&ff replaced by tile number from previous code)

	cpx #0					; if tile is column 0
	bne changeTimerTileDraw6

	cpy #0					; and tile row is not 0 or 22
	beq changeTimerTileDraw6

	cpy #22
	beq changeTimerTileDraw6

.changeTimerTileDraw4

	jmp drawMapTile4Pixel			; then draw 4 pixel wide tile to not erase objects and return

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
;			carry			set if sprite is onscreen, clear if sprite was offcreen (only if debug protection enabled)
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

	cmp #spriteToAddrOffset - 1		; if X >= spriteToAddrOffset - 1
	bcs spriteToScreenCheckLower
	
	jmp spriteToScreenNotValid
	
.spriteToScreenCheckLower
						; if X < max then continue to calcX
						; max x position is unscaled so we need to scale up screen/sprite pixels for the test 4/3 or 1.333333333
	cmp #(((screenWidth * 2 - spriteTileWidth + 1) * 4) / 3) + spriteToAddrOffset
	bcc spriteToScreenCalcX

	jmp spriteToScreenNotValid		; else exit

.spriteToScreenCalcX

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

						; if Y position >= max + 3 + spriteToAddrOffset then exit
	cmp #23 * 8 + 3 + spriteToAddrOffset
	bcc spriteToScreenCalcY
	jmp spriteToScreenNotValid

.spriteToScreenCalcY
	
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

	equb &e0 + palRed
	equb &e0 + palMagenta
	equb &e0 + palGreen
	equb &e0 + palCyan
	equb &e0 + palYellow
	equb &e0 + palWhite
	equb &e0 + palWhite
	equb &e0 + palWhite
	equb &e0 + palWhite
	equb &e0 + palYellow
	equb &e0 + palCyan
	equb &e0 + palGreen
	equb &e0 + palMagenta
	equb &e0 + palRed
	equb &e0 + palBlue
	equb &e0 + palBlue

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateSkullColor

	ldx #0					; index=0
	
	lda shield				; if ladybug is invulnerable to skulls
	bne updateSkullColorFromTable		; then go set the color palette using index 0

	lda vsyncCounter			; else use index=(vsyncCounter / 4) & 15 for flashing color sequence
	lsr a
	lsr a
	and #15
	tax

.updateSkullColorFromTable

	lda updateSkullColorTable,x		; paletteRegister=updateSkullColorTable[index]
	sta ulaPalette

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; enemyRelease					if release is enabled then
;						release an enemy from the center box
;						increase active enemy count
;						disable enemy release (enabled by timer hitting top left when there is a pending release)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.enemyRelease

	lda enemyReleaseEnable			; if enemy release is enabled
	beq enemyReleaseExit

	lda enemyTimer				; if enemyTimer < 5 (it has just wrapped around)
	cmp #5
	bcs enemyReleaseExit

	lda enemysActive			; if enemysActive < spritesTotal - 1
	cmp #spritesTotal - 1
	bcs enemyReleaseExit

	ldx #spritesTotal - 1			; start with last enemy in list

.enemyReleaseLoop

	lda spritesDir, x			; if pending enemy not found
	and #spriteBlanking + moveStop
	cmp #moveStop
	beq enemyReleaseFound

	dex					; then try next enemy
	bne enemyReleaseLoop			; until all checked

	rts					; return

.enemyReleaseFound
	
	txa					; calculate frame to release enemy
	sec
	sbc #1
	asl a
	sta enemyReleaseFrame
	
	lda vsyncCounter			; if (vsyncCounter & 7) != enemyReleaseFrame then exit
	and #7
	cmp enemyReleaseFrame
	bne enemyReleaseExit

	lda #moveUp				; pending enemy found so release it by setting it moving upwards out of the box
	sta spritesDir, x

	inc enemysActive			; increase enemys active count

	lda #0					; disable enemy release until timer re-enables it
	sta enemyReleaseEnable

	lda enemysActive			; if maximum enemys not active yet
	cmp #spritesTotal - 1
	beq enemyReleaseMax
	
	jsr enemySpawn				; then spawn another enemy to the center box

.enemyReleaseExit

	rts					; and exit

.enemyReleaseMax

	lda #&ff				; maximum enemys released so activate center vegetable
	sta vegetableActive
	
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; enemySpawn					if the box is empty then place enemy waiting to be released
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.enemySpawn

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; first check that there are currently no enemys in the box
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx #spritesTotal - 1			; check that there are currently no enemys in the box

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
	bne enemySpawnFindInactive
	
	; we should never drop out of the above loop because the spriteBlanking test must find a sprite

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

.enemySpawnLevelImage

	tay					; pick enemy type from level number 1 to 8
	lda spriteBaseImg, y			; set enemy image

.enemySpawnImage

	sta spritesImg, x			; set enemy image to chosen enemy type
	
	lda #centerBoxX				; set enemy location to center box
	sta spritesX, x
	lda #centerBoxY
	sta spritesY, x

	lda #moveUp + moveStop			; set enemy direction up + stopped
	sta spritesDir, x

	lda #0					; deactivate center vegetable
	sta vegetableActive

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

	; lda #moveRight				; set ladybug direction right moving
	; sta spritesDir + 0
	
	; lda #ladybugEnterX			; set ladybug to bottom left
	; sta spritesX + 0
	; lda #ladybugEnterY
	; sta spritesY + 0

	; rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; redrawSprites					process sprite list for upper or lower area, erase and redraw each sprite
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			Y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

						; lines 0-91 upper, lines 92-183 lower

						; if sprite is 1/2 across then switch to other side
upperLowerThreshold	= 92 - (spriteTileHeight / 2)

spritesPerFrame		= 3			; maximum number of sprites in each half of the screen that can be safely erased and drawn

;-----------------------------------------------------------------------------------------------------------------------------------------------------

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

	ldx addr8				; get sprite index so we can continue

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

	lda spritesX, x				; store sprite x for drawing
	sta drawSpriteX
	sta spritesEraseX, x			; also store into list for later erasure
	
	lda spritesY, x				; store sprite y for drawing
	sta drawSpriteY
	sta spritesEraseY, x			; also store into list for later erasure

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

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; if sprite was actually drawn (not blanked or off screen)
	;---------------------------------------------------------------------------------------------------------------------------------------------

.redrawSpritesGetEraseInfo

	bcc redrawSpritesNext			; if result of drawSprite/drawSpriteFlipped was not drawn then skip to next sprite

	lda spritesX, x				; else copy sprite info into erase list
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

	stx addr8				; save current sprite index for continuation next frame
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawLevelIntro
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawLevelIntro

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; initialize the vsyncCounter and playfieldMiddle
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #levelIntroTime			; set screen timeout
	sta pauseCounter

	jsr playfieldMiddleWithTimer		; initialize and draw empty middle playfield with timer

	jsr initSprites				; initialize all sprites as blanked and erased

	lda #&f0 + palCyan			; letters and hearts in cyan
	sta ulaPalette

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw the level text and number
	;---------------------------------------------------------------------------------------------------------------------------------------------

	jsr drawString				; draw level text
	equb pixelCol2
	equw screenAddr + 2 + 8 + 7 * chrColumn + 4 * chrRow
	equs "LEVEL ", &ff
	
	lda #pixelCol5				; draw level number
	sta drawChrColor
	
	lda level
	jsr drawHex
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw the vegetable image and score value
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #72					; draw vegetable sprite
	sta drawSpriteX
	lda #5 * 8 + 2
	sta drawSpriteY
	jsr drawVegetable

	jsr drawString				; draw vegetable score
	equb pixelCol7
	equw screenAddr + 2 + 8 + 10 * chrColumn + 6 * chrRow
	equb &ff

	lda vegetableScore
	jsr drawHex
	lda #&00
	jsr drawHex

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw vegetable name centered
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #pixelCol3				; draw vegetable name
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

	lda drawMapTileAddr			; fix up lsb of addr to a multiple of 8
	and #&f8
	sta drawMapTileAddr

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw the skulls
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldx levelSkulls				; get the number of skulls

.drawLevelIntroSkullImg

	lda #mapTileSkull			; draw the skull
	jsr drawMapTile

	lda #chrColumn				; leave space between skull
	jsr drawMapTileAdjust

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
	clc
	adc #objectTileIndex
	jsr drawMapTile

	lda #chrColumn				; leave space between letters
	jsr drawMapTileAdjust

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
	jsr drawMapTileAdjust

	dex					; until all hearts done
	bne drawLevelIntroHeartsImg

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw shield and number of rounds if shield != 0
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda shield				; draw good luck or shield text
	beq drawLevelIntroGoodLuck

	jsr drawString
	equb pixelCol1
	equw screenAddr + 2 + 7 * chrColumn + 20 * chrRow
	equs "SHIELD ", &ff
	
	lda #pixelCol7
	sta drawChrColor
	lda shield
	jsr drawHex

	jmp drawLevelIntroWait

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; draw good luck if shield == 0
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawLevelIntroGoodLuck

	jsr drawString
	equb pixelCol1
	equw screenAddr + 2 + 7 * chrColumn + 20 * chrRow
	equs "GOOD LUCK", &ff
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; process some color stuff and wait for the timer to expire
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawLevelIntroWait

	jsr waitVsyncUpper			; wait upper half
	jsr waitVsyncLower			; wait lower half

	jsr processSound			; process sound effects and music

	jsr updateSkullColor			; update the skull palette color

	jsr updateBonusColor			; update the bonus palette colors

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

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusGraphics

	lda bonusSpecialActive			; if special bonus is active then draw some skulls in ladybugs path
	beq drawBonusGraphicsFlowers

	lda #lo(screenAddr + 9 * chrRow + 9 * chrColumn)
	sta drawMapTileAddr
	lda #hi(screenAddr + 9 * chrRow + 9 * chrColumn)
	sta drawMapTileAddr + 1

	lda #mapTileSkull
	jsr drawMapTile

	lda #chrColumn
	jsr drawMapTileAdjust

	lda #mapTileSkull
	jsr drawMapTile

	lda #chrColumn
	jsr drawMapTileAdjust

	lda #mapTileSkull
	jsr drawMapTile

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
; .drawRandomFlower				; draw a single random flower from the current drawMapTileAddr
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawRandomFlower

	jsr random				; pick random number 0 to 3
	and #3
	
	asl a					; make index for flower tile table
	asl a
	tay

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
; drawBonusScreen
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusScreen

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; initialize playfieldMiddle
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda #bonusTime				; set timeout
	sta pauseCounter

	jsr playfieldMiddleWithTimer		; initialize and draw empty middle playfield with timer

	jsr initSprites				; initialize all sprites as blanked and erased

	jsr drawBonusGraphics			; draw the bonus screen graphics

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; special bonus text, skulls, play special bonus music
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda bonusSpecialActive			; if special bonus active then draw special bonus text
	bne drawBonusScreenSpecialActive
	jmp drawBonusScreenExtra

.drawBonusScreenSpecialActive

	lda #&e0 + palRed			; make sure skulls are red
	sta ulaPalette

	jsr drawString				; draw the bonus text
	equb pixelCol1
	equw screenAddr + 2 + 8 + 3 * chrColumn + 16 * chrRow
	equs "CONGRATULATIONS!", &ff

	jsr drawString
	equb pixelCol5
	equw screenAddr + 2 + 8 + 2 * chrColumn + 18 * chrRow
	equs "YOU WIN ", &ff

	lda #pixelCol7
	sta drawChrColor
	
	lda #specialBonusScore + '0'
	jsr drawChr
	lda #0
	jsr drawHex
	lda #0
	jsr drawHex
	lda #'0'
	jsr drawChr

	jsr drawString
	equb pixelCol5
	equw screenAddr + 2 + 8 + 17 * chrColumn + 18 * chrRow
	equs "PTS", &ff

	if specialBonusShield = 1

	jsr drawString
	equb pixelCol3
	equw screenAddr + 2 + 8 + 5 * chrColumn + 20 * chrRow
	equs "+", &ff

	jsr drawString
	equb pixelCol7
	equw screenAddr + 2 + 8 + 7 * chrColumn + 20 * chrRow
	equb &ff

	lda #specialBonusShield + '0'
	jsr drawChr
	
	jsr drawString
	equb pixelCol3
	equw screenAddr + 2 + 8 + 9 * chrColumn + 20 * chrRow
	equs "SHIELD", &ff

	lda #lo(screenAddr + 8 + 16 * chrColumn + 20 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 8 + 16 * chrColumn + 20 * chrRow)
	sta drawMapTileAddr + 1

	lda #mapTileSkull
	jsr drawMapTile

	else

	jsr drawString
	equb pixelCol3
	equw screenAddr + 2 + 4 * chrColumn + 20 * chrRow
	equs "AND", &ff

	jsr drawString
	equb pixelCol7
	equw screenAddr + 2 + 8 * chrColumn + 20 * chrRow
	equb &ff

	lda #specialBonusShield + '0'
	jsr drawChr
	
	jsr drawString
	equb pixelCol3
	equw screenAddr + 2 + 10 * chrColumn + 20 * chrRow
	equs "SHIELDS", &ff

	lda #lo(screenAddr + 18 * chrColumn + 20 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 18 * chrColumn + 20 * chrRow)
	sta drawMapTileAddr + 1

	lda #mapTileSkull
	jsr drawMapTile

	endif

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
	equb pixelCol1
	equw screenAddr + 2 + 16 + 3 * chrColumn + 16 * chrRow
	equs "CONGRATULATIONS!", &ff

	jsr drawString
	equb pixelCol5
	equw screenAddr + 2 + 7 * chrColumn + 18 * chrRow
	equs "YOU WIN ", &ff

	lda #pixelCol7
	sta drawChrColor

	lda #extraBonusLives + '0'
	jsr drawChr

	if extraBonusLives = 1

	jsr drawString
	equb pixelCol3
	equw screenAddr + 2 + 8 + 4 * chrColumn + 20 * chrRow
	equs "EXTRA LADY BUG", &ff

	else

	jsr drawString
	equb pixelCol3
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
	equb pixelCol1
	equw screenAddr + 2 + 8 + 2 * chrColumn + 16 * chrRow
	equs "YOU DISCOVERED THE", &ff

	jsr drawString
	equb pixelCol3
	equw screenAddr + 2 + 8 + 4 * chrColumn + 18 * chrRow
	equs "DIAMOND GARDEN", &ff

	jsr drawString
	equb pixelCol5
	equw screenAddr + 2 + 2 * chrColumn + 20 * chrRow
	equs "AND WIN ", &ff

	lda #pixelCol7
	sta drawChrColor
	
	lda #diamondBonusScore
	jsr drawHex
	lda #0
	jsr drawHex
	lda #0
	jsr drawHex
	lda #'0'
	jsr drawChr

	jsr drawString
	equb pixelCol5
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

	jsr waitVsyncUpper			; wait upper half

	jsr animateLadybug			; update ladybug direction and frame counter
	
	jsr redrawSprites			; draw ladybug

	jsr moveSprites				; move ladybug

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; wait for lower sync and process
	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusScreenWaitLower

	jsr waitVsyncLower			; wait lower half

	jsr processSound			; process sound effects and music

	jsr redrawSprites			; draw ladybug

	jsr updateAnimationFrame		; update the animtion frame number

	jsr updateObjectTimer			; update object timer, mode and palette

	jsr updateBonusColor			; update the bonus palette colors

	jsr drawScore				; draw score (1 digit per loop)

	lda pauseCounter			; repeat until time expires
	bne drawBonusScreenWaitUpper

	;---------------------------------------------------------------------------------------------------------------------------------------------

.drawBonusScreenExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; ladybug animation tables
;-----------------------------------------------------------------------------------------------------------------------------------------------------

ladybugEntryX		= 8			; sprite coordinates in pixels
ladybugEntryY		= 168

.animateLadybugEntryTable

	equb (ladybugStartX - ladybugEntryX) - 1, moveRight
	equb (ladybugEntryY - ladybugStartY) - 1, moveUp
	equb 1, moveUp + moveStop
	equb 0

;-----------------------------------------------------------------------------------------------------------------------------------------------------

ladybugBonusX		= 168			; sprite coordinates in pixels
ladybugBonusY		= 11

.animateLadybugBonusTable

	equb 34, moveLeft
	equb 53, moveDown
	equb 87, moveLeft
	equb 43, moveDown
	equb 36, moveLeft
	equb 56, moveDown
	equb 1, moveDown + moveStop + spriteBlanking
	equb 0

;-----------------------------------------------------------------------------------------------------------------------------------------------------

ladybugHighScoreX	= 96			; sprite coordinates in pixels
ladybugHighScoreY	= 12

.animateLadybugHighScoreTable

	equb 65, moveRight
	equb 56, moveDown
	equb 145, moveLeft
	equb 56, moveUp
	equb 145, moveRight
	equb 56, moveDown
	equb 145, moveLeft
	equb 56, moveUp
	equb 145, moveRight
	equb 56, moveDown
	equb 145, moveLeft
	equb 56, moveUp
	equb 145, moveRight
	equb 56, moveDown
	equb 145, moveLeft
	equb 56, moveUp
	equb 145, moveRight
	equb 56, moveDown
	equb 145, moveLeft
	equb 56, moveUp
	equb 145, moveRight
	equb 56, moveDown
	equb 145, moveLeft
	equb 56, moveUp
	equb 145, moveRight
	equb 56, moveDown
	equb 145, moveLeft
	equb 56, moveUp
	equb 145, moveRight
	equb 56, moveDown
	equb 145, moveLeft
	equb 56, moveUp
	equb 145, moveRight
	equb 68, moveDown
	equb 16, moveLeft
	equb 1, moveLeft + moveStop
	equb 0

;-----------------------------------------------------------------------------------------------------------------------------------------------------

ladybugMainMenuX	= 96			; sprite coordinates in pixels
ladybugMainMenuY	= 85

.animateLadybugMainMenuTable

	equb 65, moveRight
	equb 79, moveDown
	equb 145, moveLeft
	equb 79, moveUp
	equb 79, moveRight
	equb 0

;-----------------------------------------------------------------------------------------------------------------------------------------------------

ladybugInstructionsX	= 96			; sprite coordinates in pixels
ladybugInstructionsY	= 141

.animateLadybugInstructionsTable

	equb 41, moveRight
	equb 23, moveDown
	equb 100, moveLeft
	equb 23, moveUp
	equb 58, moveRight
	equb 0

;-----------------------------------------------------------------------------------------------------------------------------------------------------

animateLadybugEntry		= 0		; entry animation index
animateLadybugBonus		= 1		; bonus animation index
animateLadybugHighScore		= 2		; high score animation index
animateLadybugMainMenu		= 3		; main menu animation index
animateLadybugInstructions	= 4		; instructions animation index

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.animateLadybugInitTable

	equw animateLadybugEntryTable
	equb ladybugEntryX
	equb ladybugEntryY
	
	equw animateLadybugBonusTable
	equb ladybugBonusX
	equb ladybugBonusY
	
	equw animateLadybugHighScoreTable
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

	asl a					; create index to init table
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

	lda #0					; deactivate animation
	sta animateLadybugActive

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; updateBonusColor				update the special extra letters and multiplier colors
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateBonusColor

	lda vsyncCounter			; if vsyncCounter & 0x08 != 0
	and #&08
	beq updateBonusColorOff
	
.updateBonusColorOn

	lda #&a0 + palRed			; color set 1
	sta ulaPalette
	lda #&b0 + palMagenta
	sta ulaPalette
	lda #&c0 + palYellow
	sta ulaPalette
	lda #&d0 + palGreen
	sta ulaPalette
	
	rts					; return
	
.updateBonusColorOff

	lda #&a0 + palMagenta			; color set 0
	sta ulaPalette
	lda #&b0 + palRed
	sta ulaPalette
	lda #&c0 + palGreen
	sta ulaPalette
	lda #&d0 + palYellow
	sta ulaPalette
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; clearTileMap					fill tileMap with blank tile
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

						; tileMap = 23 * 23 tiles = &211 bytes

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.clearTileMap

	ldx #0
	
	lda #mapTileBlank			; fill tileMap with blank tile

.clearTileMapLoop0

	sta tileMap, x				; store blank tile in 2 pages of &211 bytes
	sta tileMap + &100, x
	
	inx
	bne clearTileMapLoop0
	
.clearTileMapLoop1

	sta tileMap + &200, x			; store blank tile in the last &11 of &211 bytes

	inx
	cpx #&11
	bne clearTileMapLoop1
	
	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; drawString					draw text string terminanated by byte with bit 7 set
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			string			string is read from memory following the jsr
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			PC			returns to address following string terminator
;			drawChrAddr		points to next chr position on screen
;			A			destroyed
;			X			preserved
;			y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawString

	sty drawStringSaveY			; preserve register

	pla					; pop return address and add 1 to get address of text (6502 quirk)
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
	
	rts					; return



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
; processSound					; process sound effects tables and send to psg
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------


.processSound

	if debugRaster				; if debug then set background color

	lda #palWhite
	sta ulaPalette

	endif

	ldy #soundChannels - 1			; process from soundChannels - 1 to 0
	ldx #2 * (soundChannels - 1)		; offset to pointers

.processSoundLoop

	lda soundTimers, y			; if soundTimer = 0 then skip it
	beq processSoundNext

	sec					; timer = timer - 1
	sbc #1
	sta soundTimers, y
	bne processSoundNext			; if timer > 0 then skip it
	
.processSoundGetData

	lda (soundAddrPtrs, x)			; get byte from sound table

	and #&c0				; if its timer data then do the timer stuff
	cmp #&40
	beq processSoundGetTimer

	lda optionSound				; else if sound enabled
	beq processSoundNextByte

	lda (soundAddrPtrs, x)			; write data to psg chip
	jsr psgWrite

.processSoundNextByte

	inc soundAddrPtrs, x			; inc pointer and go get another byte from table
	bne processSoundGetData
	inc soundAddrPtrs + 1, x
	bne processSoundGetData			; and get another sound byte from table

.processSoundGetTimer

	lda (soundAddrPtrs, x)			; get new timer value
	and #&3f

	sta soundTimers, y			; store new timer value

	inc soundAddrPtrs, x			; inc pointer
	bne processSoundNext
	inc soundAddrPtrs + 1, x

.processSoundNext

	dex					; goto next sound effect
	dex
	dey

	bpl processSoundLoop			; continue until all 4 channels processed

.processSoundExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; playSoundSilence				; silence all psg and software channels
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.playSoundSilence

	lda #0					; shut down all software channels
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
; .playSoundTimer				play the timer sound at the correct volume
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.playSoundTimer

	lda optionTimerVolume			; if optionTimerVolume != 0
	bne playSoundTimerVolume
	rts
	
.playSoundTimerVolume

	clc					; then play the required timer bleep and return
	adc #sfxTimerLow - 1



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; playSound					send sound data to soundEffect to trigger sound
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		playSoundAddr		1st entry to sound data
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			y			preserved
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

	lda playSoundSaveA			; make table offset
	asl a
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
	; channel 0 high priority
	;---------------------------------------------------------------------------------------------------------------------------------------------

	cpy #0					; if channel = 0 (music)
	bne playSoundLowPriority
	
	sty soundTimers + 0			; then shut down all channels except 5 (sfxMunch)
	sty soundTimers + 1
	sty soundTimers + 2
	sty soundTimers + 3
	sty soundTimers + 4

	lda #&9f				; and silence psg channels 0,1,2
	jsr psgWrite
	lda #&bf
	jsr psgWrite
	lda #&df
	jsr psgWrite

	jmp playSoundNow			; finally play the sound

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; channels 1 to 5 low priorty
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

	lda playSoundSaveA			; get sound number

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

	clc					; copy sound data address to soundAddr
	lda playSoundAddr
	adc #1
	sta soundAddrPtrs, x
	lda playSoundAddr + 1
	adc #0
	sta soundAddrPtrs + 1, x

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
; checkForObjects				read tile under ladybug and handle any objects found
;						also check if ladybug is over the bonus vegetable
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.bonusBitsMultiplierFlags			; multiplier bit flags

	equb &fb, &f9, &f8

.objectScore 

	equb &10, &80, &30			; cyan 100, red 800, yellow 300

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjects

	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; handle vegetable bonus
	;-------------------------------------------------------------------------------------------------------------------------------------------------

	lda vegetableActive			; if vegetable is active
	beq checkForObjectsTile

	lda spritesX + 0			; and ladybug is in the center box
	cmp #centerBoxX
	bne checkForObjectsTile
	lda spritesY + 0
	cmp #centerBoxY
	bne checkForObjectsTile
	
	lda #0					; deactivate the vegetable
	sta vegetableActive
	
	lda bonusDiamondEnable			; if diamond is enabled then
	beq checkForObjectsVegetableScore

	lda level				; if level >= bonusDiamondLevel
	cmp #bonusDiamondLevel
	bcc checkForObjectsVegetableScore

	lda #&ff				; enable diamond bonus
	sta bonusDiamondActive
	
	lda #0					; disable the diamond after this
	sta bonusDiamondEnable
	
	lda #letterBonusTime			; pause ladybug and enemys
	sta pauseLadybug
	sta pauseEnemy

	lda #sfxMusicLetters			; play bonus letters music
	jsr playSound

	rts					; and return

.checkForObjectsVegetableScore

	lda #&ff				; activate the vegetable score display
	sta vegetableScoreActive

	lda spritesDir + 0			; blank ladybug sprite
	ora #spriteBlanking
	sta spritesDir + 0

	lda #sfxMusicVegetable			; play vegetable bonus sound
	jsr playSound

	lda #vegetableLadybugTime		; pause ladybug
	sta pauseLadybug

	lda #vegetableEnemyTime			; pause enemys
	sta pauseEnemy

	jsr addScoreVegetable			; add the vegetable bonus score

	rts					; return
	
	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; check tiles
	;-------------------------------------------------------------------------------------------------------------------------------------------------
	
.checkForObjectsTile

	lda #lo(chrRow + chrColumn)		; adjust drawMapTileAddr to be under ladybug
	clc
	adc drawMapTileAddr
	sta drawMapTileAddr
	lda #hi(chrRow + chrColumn)
	adc drawMapTileAddr + 1
	sta drawMapTileAddr + 1

	ldy #24					; read tile at ladybug location
	lda (tileMapAddr), y

	cmp #mapTileDot				; if tile = dot then use dot function
	beq checkForObjectsDot
	
	cmp #mapTileSkull			; if tile = skull then use skull function
	beq checkForObjectsSkull

	cmp #mapTileHeart			; if tile = heart then use heart function
	beq checkForObjectsHeart

	cmp #mapTileS				; if tile >= mapTileS and < mapTileR + 1
	bcc checkForObjectTileExit
	
	cmp #mapTileR + 1
	bcc checkForObjectLetter		; then use letters function

.checkForObjectTileExit

	rts					; return
	
	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; handle dot tile
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectsDot

	lda #sfxMunch				; play munch sound
	jsr playSound

	lda #mapTileBlank			; replace dot with blank tile
	sta (tileMapAddr), y
	
	dec levelEdibles			; dec edibles

	lda #1					; bump score 10 points x multiplier
	jsr addScoreMultiply

	rts					; return

	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; handle skull tile
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectsSkull

	if debugInvulnerable == false

	lda shield				; if we are vulnerable
	bne checkForObjectSkullExit

	lda #mapTileBlank			; replace skull with blank tile
	sta (tileMapAddr), y
	
	lda #mapTileBlankObj			; erase skull from screen
	jsr drawMapTile

	jsr ladybugKill				; kill ladybug

	endif

.checkForObjectSkullExit

	rts					; return

	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; handle heart tile
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectsHeart

	lda #mapTileBlank			; replace heart with blank tile
	sta (tileMapAddr), y

	dec levelEdibles			; dec edibles

	stx checkForObjectsSaveX		; preserve X register

	lda objectMode				; if object mode is cyan then update the multiplier
	bne checkForObjectsHeartScore

	ldx scoreMultiplier			; adjust bonusBits multiplier flags
	lda bonusBitsMultiplierFlags, x
	and bonusBits				; and update multiplier bits
	sta bonusBits

	jsr drawPlayfieldUpperBonus		; update upper bonus display

	inc scoreMultiplier			; inc the score multiplier
	
.checkForObjectsHeartScore

	ldx objectMode				; add score value for heart cyan = 100, red = 500, yellow = 300
	lda objectScore, x
	jsr addScoreMultiply

	cpx #objectModeCyan			; if the object was not cyan then
	beq checkForObjectsHeartRestoreX

	lda #0					; remove the possibility of getting a diamond bonus
	sta bonusDiamondEnable

.checkForObjectsHeartRestoreX

	ldx checkForObjectsSaveX		; restore X register

	jsr displayobjectScore			; setup object score display

	lda spritesDir + 0			; blank ladybug sprite
	ora #spriteBlanking
	sta spritesDir + 0

	lda #sfxObject				; play object sound
	jsr playSound

	lda #objectTime				; pause ladybug
	sta pauseLadybug

	cmp pauseEnemy				; if enemy already paused (vegetable bonus) and enemy pause time < objectTime
	bcc checkForObjectHeartExit

	sta pauseEnemy				; then set enemy pause time

.checkForObjectHeartExit

	rts					; return

	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; handle letter tile
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectLetter

	stx checkForObjectsSaveX		; preserve X register

	sec					; convert letter into index range 0-9
	sbc #objectTileIndex

	ldx objectMode				; get object color mode

	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; if objectMode = objectModeRed then bonusBits + 1 &= objectLetterBitsRed[letter]
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectLetterRed

	cpx #objectModeRed			; if objectMode = objectModeRed
	bne checkForObjectLetterYellow
	
	tax					; get red letter bit value and highlight it
	lda objectLetterBitsRed, x
	and bonusBits + 1
	sta bonusBits + 1
	
	jsr drawPlayfieldUpperBonus		; update upper bonus display

	lda #0					; remove the possibility of getting a diamond bonus
	sta bonusDiamondEnable

	jmp checkForObjectLetterErase

	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; if objectMode = objectModeYellow then bonusBits &= objectLetterBitsYellow[letter]
	;-------------------------------------------------------------------------------------------------------------------------------------------------

.checkForObjectLetterYellow

	cpx #objectModeYellow			; if objectMode = objectModeYellow
	bne checkForObjectLetterErase

	tax					; get yellow letter bit value and highlight it
	lda objectLetterBitsYellow, x
	and bonusBits
	sta bonusBits
	
	lda #0					; remove the possibility of getting a diamond bonus
	sta bonusDiamondEnable

	jsr drawPlayfieldUpperBonus		; update upper bonus display

.checkForObjectLetterErase

	lda #mapTileBlank			; replace letter with blank tile
	sta (tileMapAddr), y
	
	dec levelEdibles			; dec edibles

	;-------------------------------------------------------------------------------------------------------------------------------------------------
	; add correct value score to player (cyan, yellow, red): addScoreMultiply(objectScore[objectMode])
	;-------------------------------------------------------------------------------------------------------------------------------------------------

	ldx objectMode				; add score value * multiplier for letter: cyan = 100, yellow = 300, red = 500
	lda objectScore, x
	jsr addScoreMultiply

	ldx checkForObjectsSaveX		; restore X register

	jsr displayobjectScore			; setup the object score display

	lda spritesDir + 0			; blank ladybug sprite
	ora #spriteBlanking
	sta spritesDir + 0

	lda #sfxObject				; play object sound
	jsr playSound

	lda #objectTime				; pause ladybug
	sta pauseLadybug

	cmp pauseEnemy				; if enemy already paused (vegetable bonus) and enemy pause time < objectTime
	bcc checkForObjectLetterExit

	sta pauseEnemy				; then set enemy pause time

.checkForObjectLetterExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------

.objectLetterBitsRed				; red mode	S P E C I A L - - - = bits 6 5 4 3 2 1 0 - - -

	equb &bf, &df, &ef, &f7, &fb, &fd, &fe, &ff, &ff, &ff

.objectLetterBitsYellow				; yellow mode	- - E - - A - X T R = bits - - 7 - - 3 - 6 5 4

	equb &ff, &ff, &7f, &ff, &ff, &f7, &ff, &bf, &df, &ef



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; displayobjectScore				setup the img and position to display the object score
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.displayobjectScore

	lda spritesX + 0			; get ladybug position to the nearest 16 pixels in x and y and offset it by 8, 10
	and #&f0
	ora #8
	sta objectScoreX
	lda spritesY + 0
	and #&f0
	ora #10
	sta objectScoreY

	lda objectMode				; objectScoreImg = objectMode * 4 + scoreMultiplier + pointsImgBase
	asl a
	asl a
	clc
	adc scoreMultiplier
	adc #pointsBaseImg
	sta objectScoreImg

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; ladybugKill					reduce life by 1 and initiate death animation
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.ladybugKill

	lda #spriteBlanking			; blank ladybug sprite
	ora spritesDir + 0
	sta spritesDir + 0

	lda #&ff				; enable death animation
	sta ladybugDeathEnable

	lda #0					; remove the possibility of getting a diamond bonus
	sta bonusDiamondEnable

	sta vegetableActive			; deactivate vegetable/diamon

	sed					; reduce lives by 1
	sec
	lda lives
	sbc #1
	sta lives
	cld

	lda #sfxMusicDeath			; play ladybug death music
	jsr playSound

	lda #ladybugDeathTime			; pause ladybug and enemys
	sta pauseLadybug
	sta pauseEnemy

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; updateLadybug					check requested direction
;						move ladybug if possible
;						handle turnstile movement
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry			none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			destroyed
;			y			destroyed
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybug

	lda pauseLadybug			; if ladybug movement is paused then exit
	bne updateLadybugExit

	lda ladybugEntryEnable			; if ladybug entry animation is enabled then exit
	bne updateLadybugExit

	lda spritesDir + 0			; stop ladybug moving
	ora #moveStop
	sta spritesDir + 0

.updateLadybugUp

	ldx #moveUp				
	lsr playerInput				; if up was requested
	bcs updateLadybugMove
	
.updateLadybugDown

	ldx #moveDown

	lsr playerInput				; if down was requested
	bcs updateLadybugMove
	
.updateLadybugLeft

	ldx #moveLeft

	lsr playerInput				; if left was requested
	bcs updateLadybugMove
	
.updateLadybugRight

	ldx #moveRight

	lsr playerInput				; if right was requested
	bcs updateLadybugMove
	
.updateLadybugExit

	rts					; return

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugMove

	lda spritesX + 0			; convert ladybug XY to tileMapAddr
	sta spriteToAddrX
	lda spritesY + 0
	sta spriteToAddrY
	jsr spriteToAddr

	jsr checkForObjects			; handle object tile under ladybug

.updateLadybugGrid

	lda spritesX + 0			; if sprite is at exact grid
	sta spriteToAddrX
	and #&0f
	cmp #&08
	bne updateLadybugMoveCheckXY
	
	lda spritesY + 0
	sta spriteToAddrY
	and #&0f
	cmp #&08
	bne updateLadybugMoveCheckXY
	
	ldy moveDirMap, x			; get tile in front of ladybug
	lda (tileMapAddr), y

	bpl updateLadybugMoveCheckXY		; if its a path then move ladybug in direction

	and #&40				; if its a turnstile then push it
	beq updateLadybugTurnstile

	rts					; else its a wall so exit without moving

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugTurnstile

	lda #sfxTurnstile			; play turnstile sound
	jsr playSound

	lda (tileMapAddr), y			; get tile from movement direction
	and #&3f				; extract the tile number and check for which direction
	
	ldy #0					; check direction

	cmp #mapTileTurnstileU
	beq updateLadybugTurnstilePush

	iny
	cmp #mapTileTurnstileD
	beq updateLadybugTurnstilePush

	iny
	cmp #mapTileTurnstileL
	beq updateLadybugTurnstilePush

	iny
	cmp #mapTileTurnstileR
	beq updateLadybugTurnstilePush

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugMoveCheckXY

	cpx #moveUp				; if up/down pressed
	beq updateLadybugMoveCheckX		; then check if sprite X is aligned
	cpx #moveDown
	beq updateLadybugMoveCheckX
	
.updateLadybugMoveCheckY			; else left/right was pressed so check if sprite Y is aligned

	lda spritesY + 0			; if sprite Y is aligned with junction then move horizontally
	and #&0f
	cmp #8
	beq updateLadybugMoveExit

	bcs updateLadybugMoveCheckY2		; if sprite Y is above the junction then move down
	ldx #moveDown

.updateLadybugMoveCheckY2

	bcc updateLadybugMoveExit		; if sprite Y is below the junction then move up
	ldx #moveUp

.updateLadybugMoveExit

	lda spritesDir + 0			; combine the direction with the blanking
	and #spriteBlanking
	stx spritesDir + 0
	ora spritesDir + 0
	sta spritesDir + 0

	rts					; return

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugMoveCheckX			; up/down was pressed so check if sprite X is aligned

	lda spritesX + 0			; if sprite X is aligned with junction then move vertically
	and #&0f
	cmp #8
	beq updateLadybugMoveExit

	bcs updateLadybugMoveCheckX2		; if sprite X is left of the junction then move right
	ldx #moveRight

.updateLadybugMoveCheckX2

	bcc updateLadybugMoveExit		; if sprite X is right of the junction then move left
	ldx #moveLeft
	bcs updateLadybugMoveExit

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugTurnstileX

	equb 0,0,8,-8,0,0,8,-8,-8,-8,0,0,8,8,0,0
	
.updateLadybugTurnstileY

	equb 0,0,-8,-8,0,0,8,8,8,-8,0,0,8,-8,0,0

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugTurnstilePush

	sty updateLadybugSaveDir		; save the tileDir
	
	txa					; index = dir * 4 + tileDir
	and #&03
	asl a
	asl a
	ora updateLadybugSaveDir
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

	txa					; choose horizontal or vertical from direction
	and #&02
	bne updateLadybugTurnstileHorizontal
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugTurnstileVertical

	lda #mapTileTurnstileU + wallTurnstile	; place vertical turnStile into tileMap
	ldy #1
	sta (tileMapAddr), y

	lda #mapTileTurnstileCV + wallTurnstile
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

	jmp updateLadybugMoveCheckXY

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.updateLadybugTurnstileHorizontal

	lda #mapTileTurnstileL + wallTurnstile	; place horizontal turnstile into tileMap
	ldy #23
	sta (tileMapAddr), y

	lda #mapTileTurnstileCH + wallTurnstile
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

	jmp updateLadybugMoveCheckXY



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
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.drawTurnstileCheckDir

	sta drawMapTileAddr + 1			; then set drawTileAddr
	lda drawTurnstileAddr
	sta drawMapTileAddr

	lda #&ff				; mark as done after this draw is complete
	sta drawTurnstileAddr + 1

	lda drawTurnstileDir			; check direction and draw vertical or horizontal
	bne drawTurnstileHorizontal

;-----------------------------------------------------------------------------------------------------------------------------------------------------

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
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------

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
; spriteToAddr					convert spriteX + offset, spriteY + offset to tileMapAddr and drawMapTileAddr
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

spriteToAddrOffset	= 4			; correction factor

;-----------------------------------------------------------------------------------------------------------------------------------------------------

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
	sec
	sbc #1
	tax
	
	clc					; convert raw grid coordinates numbers to tileMap address
	adc tileMapRowsLo, y
	sta tileMapAddr
	lda tileMapRowsHi, y
	adc #0
	sta tileMapAddr + 1

	clc					; convert raw grid coordinates to drawMapTile screen address
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
; .spriteTest					debugging sprite animation test
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	if debugSpriteTest

.spriteTest

	jsr initSprites				; clear all sprites

	jsr clearTileMap			; fill tilemap with blank tile
	
	jsr drawString				; display text
	equb pixelCol7
	equw screenAddr + 2
	equs "PRESS START TO SELECT  "
	equs "NEXT SPRITE SET", &ff
	
	jsr drawString
	equb pixelCol1
	equw screenAddr + 2 + 3 * chrRow
	equs "SPRITE SET ", &ff

	lda #9 * 8				; initialise sprite positions

	sta spritesX + 1
	sta spritesY + 1

	sta spritesY + 2

	sta spritesX + 3

	lda #15 * 8

	sta spritesX + 2

	sta spritesY + 3

	sta spritesX + 4
	sta spritesY + 4

	lda #moveUp				; set sprite direction and visible
	sta spritesDir + 1
	lda #moveDown
	sta spritesDir + 2
	lda #moveLeft
	sta spritesDir + 3
	lda #moveRight
	sta spritesDir + 4

	lda #0					; start with sprite set 0
	sta randomSeed
	
.spriteTestImg

	lda #lo(screenAddr + 3 * chrRow + 11 * chrColumn)
	sta drawMapTileAddr
	lda #hi(screenAddr + 3 * chrRow + 11 * chrColumn)
	sta drawMapTileAddr + 1
	lda randomSeed
	jsr drawExtraTile

	ldx randomSeed

	lda spriteBaseImg, x			; set sprite img set
	sta spritesImg + 1
	sta spritesImg + 2
	sta spritesImg + 3
	sta spritesImg + 4

.spriteTestWaitRelease

	jsr waitVsyncUpper			; wait for vsync interrupt for upper area

	jsr redrawSprites			; erase and redraw sprites in upper area

	jsr waitVsyncLower			; wait for vsync interrupt for lower area
	
	jsr redrawSprites			; erase and redraw sprites in lower area
	
	jsr updateAnimationFrame		; update the animtion frame number

	jsr keyboardScan			; read keyboard input

	lda playerInput				; if start pressed then loop back
	and #keyBitStart
	bne spriteTestWaitRelease

.spriteTestWaitPress

	jsr waitVsyncUpper			; wait for vsync interrupt for upper area

	jsr redrawSprites			; erase and redraw sprites in upper area

	jsr waitVsyncLower			; wait for vsync interrupt for lower area
	
	jsr redrawSprites			; erase and redraw sprites in lower area
	
	jsr updateAnimationFrame		; update the animtion frame number

	jsr keyboardScan			; read keyboard input

	lda playerInput				; if start pressed then loop back
	and #keyBitStart
	beq spriteTestWaitPress

	inc randomSeed

	lda randomSeed
	cmp #9
	bcc spriteTestImg
	
	lda #0
	sta randomSeed

	beq spriteTestImg

	endif



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; vegetable names
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.vegetableCucumber

	equs "CUCUMBER", &ff

.vegetableEggPlant

	equs "EGG PLANT", &ff

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
; vegetable screen and name addresses
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.vegetableAddr

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 8) * chrColumn) / 2) and &fff8)
	equw vegetableCucumber;, 8

	equw screenAddr + 2 + 8 * chrRow + ((((23 - 9) * chrColumn) / 2) and &fff8)
	equw vegetableEggPlant;, 9

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
	skip 0

	incbin "img-font.bin"			; load font table into memory

.fontBinEnd
	skip 0



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; special ascii chr reassignment
;-----------------------------------------------------------------------------------------------------------------------------------------------------

chrCopyright		= '%'
chrUp			= '<'
chrDown			= '='
chrLeft			= '>'
chrRight		= '?'
chrMultiplierX		= '&'
chrMultiplier2		= '''
chrMultiplier3		= '('
chrMultiplier5		= ')'
chrHeart		= '*'



	align 8					; this is not needed, only here so that if someone *load ladybug in mode 2 then the graphics line up

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; minifont 0-F used for vegetable bonus and debugging
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.miniFontBin
	skip 0
	
	incbin "img-minifont-decimal.bin"	; load minifont into memory



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; mapTile data
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.mapTileBin
	skip 0

	incbin "img-tiles.bin"			; load tile table into memory, max 128

.mapTileBinEnd
	skip 0



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; objectTile data
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.objectTileBin
	skip 0

	incbin "img-objects.bin"		; load objects table into memory

.objectTileBinEnd
	skip 0

mapTileObject = (objectTileBin - mapTileBin) / mapTileBytes 
assert mapTileObject < 115			; more than 128 tiles




;-----------------------------------------------------------------------------------------------------------------------------------------------------
; extra tile data
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.extraTileBin
	skip 0

	incbin "img-extra.bin"			; load extra tile table into memory

.extraTileBinEnd
	skip 0



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
; table to extra tiles
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

	for x,11,22				; enemy timer clockwise from top middle
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

.mazeData					; note top and bottom rows removed as initTimerTiles initializes the top/bottom rows
						; as well as the left/right columns but these have been left here using 42 bytes
						; because the added complexity to the maze init to remove the two columns isnt worth it
						; plus the saving wouldnt be much due to the larger init code

	equb &00,&01,&00,&01,&00,&01,&00,&01,&00,&01,&00,&01,&00,&01,&00,&01,&00,&01,&00,&01,&00,&01,&00
	equb &00,&00,&d9,&a1,&9d,&a0,&d9,&00,&db,&d7,&d3,&00,&d2,&d7,&da,&00,&d9,&a1,&9d,&a0,&d9,&00,&00
	equb &00,&01,&d6,&01,&00,&01,&d6,&01,&9f,&01,&d6,&01,&d6,&01,&9f,&01,&d6,&01,&00,&01,&d6,&01,&00
	equb &00,&00,&d4,&d7,&da,&00,&d8,&00,&9c,&00,&d8,&00,&d8,&00,&9c,&00,&d8,&00,&db,&d7,&d5,&00,&00
	equb &00,&01,&9f,&01,&00,&01,&00,&01,&9e,&01,&00,&01,&00,&01,&9e,&01,&00,&01,&00,&01,&9f,&01,&00
	equb &00,&00,&9c,&00,&d9,&a1,&9d,&a0,&db,&d7,&d3,&00,&d2,&d7,&da,&a1,&9d,&a0,&d9,&00,&9c,&00,&00
	equb &00,&01,&9e,&01,&d6,&01,&00,&01,&00,&01,&d6,&01,&d6,&01,&00,&01,&00,&01,&d6,&01,&9e,&01,&00
	equb &00,&00,&db,&d7,&d5,&00,&d9,&a1,&9d,&a0,&d8,&00,&d8,&a1,&9d,&a0,&d9,&00,&d4,&d7,&da,&00,&00
	equb &00,&01,&00,&01,&00,&01,&d6,&01,&00,&01,&00,&01,&00,&01,&00,&01,&d6,&01,&00,&01,&00,&01,&00
	equb &00,&00,&db,&d7,&d3,&00,&d8,&00,&d9,&00,&d9,&00,&d9,&00,&d9,&00,&d8,&00,&d2,&d7,&da,&00,&00
	equb &00,&01,&00,&01,&d6,&01,&9f,&01,&d6,&01,&d6,&00,&d6,&01,&d6,&01,&9f,&01,&d6,&01,&00,&01,&00
	equb &00,&00,&d9,&00,&d8,&00,&9c,&00,&d8,&00,&d4,&d7,&d5,&00,&d8,&00,&9c,&00,&d8,&00,&d9,&00,&00
	equb &00,&01,&d6,&01,&9f,&01,&9e,&01,&9f,&01,&00,&01,&00,&01,&9f,&01,&9e,&01,&9f,&01,&d6,&01,&00
	equb &00,&00,&d6,&00,&9c,&00,&d9,&00,&9c,&00,&d9,&00,&d9,&00,&9c,&00,&d9,&00,&9c,&00,&d6,&00,&00
	equb &00,&01,&d6,&01,&9e,&01,&d6,&01,&9e,&01,&d6,&01,&d6,&01,&9e,&01,&d6,&01,&9e,&01,&d6,&01,&00
	equb &00,&00,&d4,&d7,&da,&00,&d8,&00,&db,&d7,&d5,&00,&d4,&d7,&da,&00,&d8,&00,&db,&d7,&d5,&00,&00
	equb &00,&01,&00,&01,&00,&01,&00,&01,&00,&01,&00,&00,&00,&01,&00,&01,&00,&01,&00,&01,&00,&01,&00
	equb &00,&00,&d2,&d7,&d7,&d7,&da,&a1,&9d,&a0,&d9,&00,&d9,&a1,&9d,&a0,&db,&d7,&d7,&d7,&d3,&00,&00
	equb &00,&01,&d6,&01,&00,&01,&00,&01,&00,&01,&d6,&00,&d6,&01,&00,&01,&00,&01,&00,&01,&d6,&01,&00
	equb &00,&00,&d8,&a1,&9d,&a0,&db,&d7,&da,&00,&d8,&00,&d8,&00,&db,&d7,&da,&a1,&9d,&a0,&d8,&00,&00
	equb &00,&01,&00,&01,&00,&01,&00,&01,&00,&01,&00,&00,&00,&01,&00,&01,&00,&01,&00,&01,&00,&01,&00

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; storage for the tileMap virtual screen
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.tileMap

	skip 23 * 23				; storage for tile id's for the middle screen

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; tileMap rows address table
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

.spriteBaseImg					; img index for base sprite of ladybug and each enemy set and the ladybug death

	for n, 0, 9
	equb n * 15 + sprite10x10
	next

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.spriteImgAddrLo

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; generate address table for the 10x10 pixel vegetable and points sprites
	;---------------------------------------------------------------------------------------------------------------------------------------------

	for n, sprite10x10Bin, sprite10x10BinEnd - 1, sprite10x10Bytes
	equb lo(n)
	next

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; generate address table for ladybug and enemys
	;---------------------------------------------------------------------------------------------------------------------------------------------

	for n, ladybugBin, spriteBinEnd - 1, spriteTileBytes
	equb lo(n)
	next

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.spriteImgAddrHi

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; generate address table for vegetable and point tiles
	;---------------------------------------------------------------------------------------------------------------------------------------------

	for n,  sprite10x10Bin, sprite10x10BinEnd - 1, sprite10x10Bytes
	equb hi(n)
	next

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; generate address table for ladybug and enemys
	;---------------------------------------------------------------------------------------------------------------------------------------------

	for n, ladybugBin, spriteBinEnd - 1, spriteTileBytes
	equb hi(n)
	next



;-----------------------------------------------------------------------------------------------------------------------------------------------------
.programEnd					; end of program
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print

include "relocator.asm"				; append relocation code



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; create disk files
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	save "!Boot", bootasmStart, bootasmEnd, &ffffff, 0
	putbasic "boot.bas", "$.Boot"
	save "Config", config, configEnd, &ffffff, &ff0000 + config
	save "$.Loader", swramStart, loaderEnd, &ff0000 + loaderStartReloc, &ff0000 + loaderPage
	save "$.LadyBug", progReloc, bootstrapEnd, &ff0000 + bootstrap + progOffset, &ff0000 + progLoad

	print
	print
	print



