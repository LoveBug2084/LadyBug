;-----------------------------------------------------------------------------------------------------------------------------------------------------
; loader					check for available sideways ram, copy data and then */LadyBug
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

.optionLadybugLives

	skip 1					; reserve space for ladybug lives
	
.optionEnemySpeed

	skip 1					; reserve space for enemy speed
	
.optionEnemyAttack

	skip 1					; reserve space for enemy attack
	
.optionTimerVolume

	skip 1					; reserve space for timer volume
	
.optionSound

	skip 1					; reserve space for sound mode off/on/demo
	
.optionKeys

	skip 4					; reserve space for control keys scan codes
	
.optionKeysAscii

	skip 4					; reserve space for control keys ascii codes

.validationCode

	skip 1					; reserve space for validation code for high score table and settings
	
.configDataEnd
	skip 0					; show this address in listing



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; save game mode so that menu.bas can read the last game mode on reboot
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrGameMode

	skip 1					; reserve space for last game mode standard/challenge for menu.bas



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; vegetables, points and sprites used by drawSprite
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.spriteBin
	skip 0					; show this address in listing

.sprite10x10Bin
	skip 0					; show this address in listing

.vegetablesBin
	skip 0					; show this address in listing

	incbin "bin/vegetables.bin"		; load the 10x10 pixel vegetable sprites into memory

.vegetablesBinEnd
	skip 0					; show this address in listing

.pointsBin
	skip 0					; show this address in listing

	incbin "bin/points.bin"			; load the 10x10 pixel points sprites into memory

.pointsBinEnd
	skip 0					; show this address in listing

.sprite10x10BinEnd
	skip 0					; show this address in listing

.ladybugBin
	skip 0					; show this address in listing

	incbin "bin/ladybug.bin"		; load ladybug sprite set into memory

.enemy1Bin
	skip 0					; show this address in listing

	incbin "bin/enemy1.bin"			; load 1st enemy sprite set into memory

.enemy2Bin
	skip 0					; show this address in listing

	incbin "bin/enemy2.bin"			; load 2nd enemy sprite set into memory

.enemy3Bin
	skip 0					; show this address in listing

	incbin "bin/enemy3.bin"			; load 3rd enemy sprite set into memory

.enemy4Bin
	skip 0					; show this address in listing

	incbin "bin/enemy4.bin"			; load 4th enemy sprite set into memory

.enemy5Bin
	skip 0					; show this address in listing

	incbin "bin/enemy5.bin"			; load 5th enemy sprite set into memory

.enemy6Bin
	skip 0					; show this address in listing

	incbin "bin/enemy6.bin"			; load 6th enemy sprite set into memory

.enemy7Bin
	skip 0					; show this address in listing

	incbin "bin/enemy7.bin"			; load 7th enemy sprite set into memory

.enemy8Bin
	skip 0					; show this address in listing

	incbin "bin/enemy8.bin"			; load 8th enemy sprite set into memory

.angel0Bin
	skip 0					; show this address in listing
	
	incbin "bin/angel0.bin"			; load ladybug angel sprite set (1st part) into memory

.diamondBin
	skip 0					; show this address in listing

	incbin "bin/diamond.bin"		; load diamond sprite set into memory
	
.diamondFiller					; fill remaining space so that 2nd part angel address is correctly aligned
	skip spriteTileBytes - (2 * diamondTileBytes)

.angel1Bin
	skip 0					; show this address in listing
	
	incbin "bin/angel1.bin"			; load ladybug angel sprite set (2nd part) into memory

.spriteBinEnd
	skip 0					; show this address in listing

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; generate image index for sprite sets
;-----------------------------------------------------------------------------------------------------------------------------------------------------

imgVegetables		= (vegetablesBin - spriteBin) / sprite10x10Bytes
ImgPoints		= (pointsBin - spriteBin) / sprite10x10Bytes
ImgLadybugEnemiesAngel	= (ladybugBin - spriteBin) / sprite10x10Bytes



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
	equw swrCleanResetB			; Master ET	No plans for this as unable to test :(
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
	
	cmp #lo(masterMos350)			; mos 3.50 supported
	beq swrCleanResetMaster350
	
	cmp #lo(masterMos320)			; mos 3.20 supported
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
; swrInitScreen					fill screen ram with zero
;						setup color palette
;-----------------------------------------------------------------------------------------------------------------------------------------------------
;						note this function is only run during first run
;						so the self modifying address doesn't need to be reset
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrInitScreen

	ldx #lo(screenAddr)			; start x at low 8 bits of screen start address

	lda #pixelsBlack			; fill screen with black

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

	equb pal0 + palBlack			; colors 0-7 are regular beeb colors
	equb pal1 + palRed
	equb pal2 + palGreen
	equb pal3 + palYellow
	equb pal4 + palBlue
	equb pal5 + palMagenta
	equb pal6 + palCyan
	equb pal7 + palWhite

	equb palMultiplier0 + palCyan		; color 8 multiplier (flashing cyan / blue)
	equb palMultiplier1 + palBlue		; color 9 multiplier (flashing blue / cyan)

	equb palSpecial0 + palRed		; color 10 special (flashing red / magenta)
	equb palSpecial1 + palMagenta		; color 11 special (flashing magenta / red)

	equb palExtra0 + palYellow		; color 12 extra (flashing yellow / green)
	equb palExtra1 + palGreen		; color 13 extra (flashing green / yellow)

	equb palSkull + palWhite		; color 14 skull (flashing effect or solid red when shield is active)
	equb palObject + palCyan		; color 15 object (cyan / red / yellow)



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; swrJoystickControl				combine playerInput (keyboard) with joystickInput
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
; swrDrawPlayfieldLowerDiamond			draw a diamond image (if enabled) or a blank image
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrDrawPlayfieldLowerDiamond

	stx drawSpriteSaveX			; save registers
	sty drawSpriteSaveY

	lda #0					; draw column from 0
	sta drawSpriteColumnVinit + 1

	lda #diamondTileHeight			; set height to diamondTileHeight
	sta drawSpriteColumnTileHeight + 1
	sta drawSpriteColumnVtest + 1

	lda #opcodeINX				; drawing normal so use INX instruction
	sta drawSpriteNextLineInstruction

	lda #diamondTileBytes			; store number of bytes for diamond tile in counter
	sta drawByteCount

	lda #lowerDiamondX			; set position for the lower diamond available
	sta drawSpriteX
	lda #lowerDiamondY
	sta drawSpriteY

	lda #spriteLowerDiamond			; set sprite to diamond image
	sta drawSpriteImg
	
	lda bonusDiamondEnable			; if diamond bonus not enabled
	bne swrDrawPlayfieldLowerDiamondExit

	inc drawSpriteImg			; then set sprite image to blank

.swrDrawPlayfieldLowerDiamondExit

	jmp drawSpriteGetAddr			; draw the diamond/blank image and return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; swrGameLevel					choose level 1 for regular game or random level for demo game
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrGameLevel

	lda #&01				; start with level 1
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

	lda optionLadybugLives			; initialize player lives
	sta lives

	lda #&ff				; clear the special, extra and multiplier bonus flag bits
	sta bonusBits + 0
	sta bonusBits + 1

	sta ladybugEntryEnable			; enable ladybug entry movement animation

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; enable the possibility of getting a diamond bonus (regular game) or disable diamond bonus completely (high score challenge)
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda highScoreChallenge
	eor #&ff
	sta bonusDiamondEnable

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; if demo mode then use default number of lives and choose a random level (50% chance level 1-8 or 50% change level 1-24)
	;---------------------------------------------------------------------------------------------------------------------------------------------

	lda demoMode				; if demo mode is active then
	beq swrGameLevelExit

	lda #defaultLadybugLives		; override the menu lives and use the default lives for the demo
	sta lives

.swrGameLevelLoHi

	lda #8					; 50% chance of picking level 1-8
	sta swrGameLevelChooseMaxValue + 1
	jsr random
	bpl swrGameLevelChoose	
	lda #24					; 50% chance of picking level 1-24
	sta swrGameLevelChooseMaxValue + 1

.swrGameLevelChoose

	jsr random				; pick random number 0 - 255
	beq swrGameLevelExit			; if = 0 then exit (no need to advance level)

.swrGameLevelChooseMaxValue

	cmp #0					; <- max value stored here from 50/50 code above, if chosen number >= this value then choose another
	bcs swrGameLevelChoose

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; advance the level 1 to 23 times (from chosen random number)
	;---------------------------------------------------------------------------------------------------------------------------------------------

	tay					; set the number of loops to advance the level

.swrGameLevelAdvance

	jsr levelAdvance			; advance game level

	dey					; and repeat until done
	bne swrGameLevelAdvance

.swrGameLevelExit

	rts					; return



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; swrDemo					move ladybug for demo mode
;						avoid skulls
;						turn towards dots/hearts/letters
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrDemoDirBits					; used to simulate key presses, convert demo direction 0-3 to key bit

	equb keyBitUp, keyBitDown, keyBitLeft, keyBitRight

.swrDemoMapDir

	equb 2, 94, 46, 50			; tileMap 2 tile offset from top left corner of 5x5 square for up, down, left, right

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrDemo

	lda demoMode				; if demo mode is enabled
	bne swrDemoCheckGrid

	rts

	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrDemoCheckGrid

	lda updateLadybugGridX			; if ladybug is off grid then use current demo direction and return
	ora updateLadybugGridY
	bne swrDemoSetDir
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

	sec					; ladybug is on grid so
	lda tileMapAddr + 0			; set demoMapAddr to tileMapAddr - 24 to be able to search a 5x5 square for tiles
	sbc #24
	sta demoMapAddr + 0
	lda tileMapAddr + 1
	sbc #0
	sta demoMapAddr + 1

	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrdemoCheckSides

	bit vsyncCounter			; choose to test side1/side2 or reversed side2/side1
	bmi swrDemoCheckSidesReversed

	jsr swrDemoCheckSide1			; check side 1 for tile
	bcs swrDemoSetDir

	jsr swrDemoCheckSide2			; check side 2 for tile
	bcs swrDemoSetDir

	bcc swrDemoCheckFront			; check front for tile

.swrDemoCheckSidesReversed

	jsr swrDemoCheckSide2			; check side 2 for tile
	bcs swrDemoSetDir

	jsr swrDemoCheckSide1			; check side 1 for tile
	bcs swrDemoSetDir

	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrDemoCheckFront

	ldx demoDir				; get current demo direction

	jsr updateLadybugCheckPath		; if 1 tile in front of ladybug is a wall
	beq swrDemoRandomDir			; then choose a random direction

	jsr swrDemoCheckTileEdible		; check 2 tiles in front for dot/heart/letters
	beq swrDemoSetDir			; if tile found then continue going forward

	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrDemoCheckSkull

	jsr swrDemoCheckTileSkull		; if 2 tiles in front of ladybug is a skull
	beq swrDemoRandomDir			; then choose random direction

	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrDemoRandomPercentage

	jsr random
	cmp #0.20 * 256				; clear path in front so 20% chance to turn randomly otherwise stay on current direction
	bcc swrDemoRandomDir

	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrDemoSetDir

	lda playerInput				; remove all inputs except esc and combine with demo direction
	and #keyBitEsc
	ldx demoDir
	ora swrDemoDirBits, x
	sta playerInput

	rts					; return

	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrDemoCheckSide1

	lda demoDir				; get current demo direction

	eor #%10				; check side 1 of ladybug
	tax

	jsr updateLadybugCheckPath		; if 1 tile to the side is a solid wall
	beq swrDemoCheckSideFalse		; then return false

	jsr swrDemoCheckTileEdible		; check 2 tiles to the side for dot/heart/letters
	bne swrDemoCheckSideFalse		; then return false

	stx demoDir				; tile found so set turn direction

.swrDemoCheckSideTrue

	sec					; return true
	rts

.swrDemoCheckSideFalse

	clc					; return false
	rts

	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrDemoCheckSide2

	lda demoDir				; get current demo direction

	eor #%11				; check side 2 of ladybug
	tax

	jsr updateLadybugCheckPath		; if 1 tile to the side is a solid wall
	beq swrDemoCheckSideFalse		; then return false

	jsr swrDemoCheckTileEdible		; check 2 tiles to the side for dot/heart/letters
	bne swrDemoCheckSideFalse		; then return false

	stx demoDir				; tile found so set turn direction
	jmp swrDemoCheckSideTrue

	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrDemoRandomDir

	ldy #1					; if all directions are blocked by a solid wall then give up and stay on current direction
	lda (tileMapAddr), y			; this test prevents an infinite loop that would occur while searching for a random direction
	ldy #23					; if ladybug was boxed in at starting position (unplayable map) 
	and (tileMapAddr), y
	ldy #25
	and (tileMapAddr), y
	ldy #47
	and (tileMapAddr), y
	bmi swrDemoSetDir

	;---------------------------------------------------------------------------------------------------------------------------------------------

.swrDemoRandomDirLoop

	jsr random				; pick a random direction that is not a solid wall
	and #%11
	tax
	jsr updateLadybugCheckPath
	beq swrDemoRandomDirLoop

	stx demoDir				; set chosen random direction
	jmp swrDemoCheckSkull			; and check there is no skull in this direction



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; swrDemoCheckTileSkull				check for skull tile in 2 tiles from ladybug position at supplied direction
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	X			direction 0-3
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			destroyed
;			X			preserved
;			Y			preserved
;			Z			= set (skull found), cleared (not found)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrDemoCheckTileSkull

	ldy swrDemoMapDir, x			; set index to 2 tiles from ladybug
	lda (demoMapAddr), y			; check for skull tile and return with result
	cmp #mapTileSkull

	rts					



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; swrDemoCheckTileEdible			check for edible tiles 2 tiles from ladybug at supplied direction
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	X			direction 0-3
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			A			= &00 edible found, = &ff not found
;			X			preserved
;			Y			preserved
;			Z			= set (edible found), = cleared (not found)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; workspace		none
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrDemoCheckTileEdible

	ldy swrDemoMapDir, x			; set index to 2 tiles from ladybug
	lda (demoMapAddr), y			; get tile

	cmp #mapTileBlank			; if its a blank tile then exit with false
	beq swrDemoCheckTileEdibleFalse

	cmp #mapTileSkull			; if its a skull tile then exit with false
	beq swrDemoCheckTileEdibleFalse

.swrDemoCheckTileEdibleTrue			; its neither a blank or a skull so it must be a dot/heart/letter so exit with true

	lda #0
	rts					

.swrDemoCheckTileEdibleFalse

	lda #&ff
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------

; regular 3x3 box offset (** tileMapAddr)
; ---------------
; | ** | 01 |    |
; ---------------
; | 23 |    | 25 |
; ---------------
; |    | 47 |    |
; ---------------

; extended 5x5 box offset (@@ demoMapAddr) = tileMapAddr - 24
; --------------------------
; | @@ |    | 02 |    |    |
; --------------------------
; |    | ** | 01 |    |    |
; --------------------------
; | 46 | 23 |    | 25 | 50 |
; --------------------------
; |    |    | 47 |    |    |
; --------------------------
; |    |    | 94 |    |    |
; --------------------------



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; swrKeyboardScan			scan all keys (used when redefining input keys)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; entry parameters	none
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; exit			C		set if key pressed, clear if no key pressed
;			A		key index if key was pressed, &ff if no key pressed
;			X		preserved
;			Y		preserved
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

.swrKeyboardScan

	stx swrKeyboardScanSaveX		; save register

	lda #%01111111				; set port A bit 7 as input ( from keyboard output )
	sta via1PortDdrA
	
	lda #sbKeyboard + sbLow			; keyboard -enable low (enable keyboard output to port a bit 7 input)
	sta via1PortB
	
	ldx #(keyScanCodesEnd - keyScanCodes) - 1; start at end of table
	
.swrKeyboardScanLoop				; repeat

	lda keyScanCodes, x			; get key scan code from table

	sta via1PortA				; select key in keyboard matrix

	lda via1PortA				; read key pressed status from bit 7

	bmi swrKeyboardScanPressed		; if pressed then return with scancode

	dex					; until all tested
	bpl swrKeyboardScanLoop

	clc					; no key pressed so return with false

.swrKeyboardScanExit

	lda #sbKeyboard + sbHigh		; keyboard -enable high (disable keyboard output)
	sta via1PortB
	
	lda #%11111111				; set port A all bits output
	sta via1PortDdrA

	txa					; A = scan index or ff (no key pressed)

	ldx swrKeyboardScanSaveX		; restore register

	rts					; return

.swrKeyboardScanPressed

	sec					; key pressed so set true status
	
	bcs swrKeyboardScanExit			; return with keyboard scan code



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; draw challenge mode number of shields on special bonus screen
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrDrawBonusScreenSpecialActiveShieldsChallenge

	if (bonusSpecialShield and 15) = 1
	{

	jsr drawString
	equw screenAddr + 2 + 8 + 6 * chrColumn + 20 * chrRow
	equb colorWhite, &ff

	lda #(bonusSpecialShield and 15) + '0'
	jsr drawChr
	
	jsr drawString
	equw screenAddr + 2 + 8 + 8 * chrColumn + 20 * chrRow
	equs colorYellow, "SHIELD", &ff

	lda #lo(screenAddr + 8 + 15 * chrColumn + 20 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 8 + 15 * chrColumn + 20 * chrRow)
	sta drawMapTileAddr + 1

	lda #mapTileSkull
	jsr drawMapTile

	}
	else
	{	

	jsr drawString
	equw screenAddr + 2 + 6 * chrColumn + 20 * chrRow
	equb colorWhite, &ff

	lda #(bonusSpecialShield and 15) + '0'
	jsr drawChr
	
	jsr drawString
	equw screenAddr + 2 + 8 * chrColumn + 20 * chrRow
	equs colorYellow, "SHIELDS", &ff

	lda #lo(screenAddr + 16 * chrColumn + 20 * chrRow)
	sta drawMapTileAddr
	lda #hi(screenAddr + 16 * chrColumn + 20 * chrRow)
	sta drawMapTileAddr + 1

	lda #mapTileSkull
	jsr drawMapTile

	}
	endif

	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; end of sideways ram code
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swramLastAddr
	skip 0					; show this address in listing

;-----------------------------------------------------------------------------------------------------------------------------------------------------




;-----------------------------------------------------------------------------------------------------------------------------------------------------
;
; loader main program				detect high ram, copy data to high ram, run main ladybug file
;
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	align &100				; start page aligned

.loaderMain
	skip 0					; show this address in listing


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; loaderMessages				various messages for ram test etc
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.loaderMessages
	skip 0					; show this address in listing

.loaderBuild

	equb 31,7,11				; position cursor
	equs 129,"Lady Bug",132,"Build",135
	incbin "output/projectBuild"
	equb &ff				; end

.loaderUsingBank

	equb 31,7,13				; position cursor
	equs 132,"Using sideways ram bank",135
	equb &ff				; end

.loaderUsingWorkspace

	equb 31,9,13				; position cursor
	equs 132, "Using",135,"B+",132,"workspace ram"
	equb &ff				; end

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
	cpx #5 * 40				; repeat clear until 5 rows of 40 colums have been erased (200 characters)
	bne loaderClearCenter

	sta screenCenterY			; clear the tail of the 'y'
	sta screenCenterY + 1
	
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; display the current build of Lady Bug
	;---------------------------------------------------------------------------------------------------------------------------------------------

	ldy #loaderBuild - loaderMessages	; display build number
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
	ldy #loaderUsingWorkspace - loaderMessages
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

	ldy #loaderUsingBank - loaderMessages	; else display sideways bank message
	jsr loaderPrint - loaderReloc

	ldy swrBank				; if swrbank < 10
	cpy #10
	bcs loaderBankTen

	lda #'0'				; then display '0' followed by bank number
	jsr oswrch

	tya
	clc
	adc #'0'
	jsr oswrch
	bne loaderCopyData

.loaderBankTen

	lda #'1'				; else display '1' followed by bank number - 10
	jsr oswrch

	tya
	clc
	adc #'0' - 10
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

	ldx #lo(runLadybug - loaderReloc)	; load and run the main ladybug program */LadyBug
	ldy #hi(runLadybug - loaderReloc)
	jmp oscli

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; sideways ram not found so display the error message
	;---------------------------------------------------------------------------------------------------------------------------------------------

.loaderFailed

	ldy #loaderRamFailed - loaderMessages	; display failure message
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
	; test sideways ram bank starting at bank 0 up to bank 15
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
	
	cmp swrTestLocation			; check swrTestLocation and exit with compare result
	rts

.swrTestFailed

	lda #&ff				; exit with not equal (failed)
	rts



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; swramCopy					copy data from main ram to sideways ram
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swramCopy

	ldx #hi(loaderMain - swramStart)	; number of pages to copy

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

