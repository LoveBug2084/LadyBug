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
; pageZero game variables, flags, counters etc
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org pageZero

	;---------------------------------------------------------------------------------------------------------------------------------------------

.vsyncCounter		skip 1			; 50Hz vsync counter (counts up and wraps around)
.pauseCounter		skip 1			; 25Hz pause counter (counts down every 2 vsyncs and wraps around)
.screenHalf		skip 1			; &00 = upper scanlines 0 to 155, &ff = lower scanlines 156 to 311

.escCounter		skip 1			; escape key counter (times how long esc is pressed)

.mazeMap		skip 1			; current maze map value 0-5   0,1=map1 2,3=map2 4,5=map3

.score			skip 3			; player score (BCD) last digit always 0 and not stored
.highScore		skip 3			; highest score (BCD) last digit always 0 and not stored
.highScorePtr		skip 2			; pointer to high score position check

.lives			skip 1			; number of lives

.ladybugEntryEnable	skip 1			; enable ladybug entry movement animation
.ladybugDeathEnable	skip 1			; enable ladybug death movement animation

.ladybugDeathAnimationIndex
			skip 1			; index into ladybug death movement animation table

.bonusBits		equw 0			; special, extra, x2 x3 x5 (1 bit each), initialize here for main menu first run all illuminated

.bonusBitsCopy		skip 2			; storage for working on bonus bits

bonusDiamondLevel	= 6			; level for releasing the diamond (if diamond bonus is enabled)

.bonusDiamondEnable	equb &ff		; diamond bonus is enabled if != 0, initialize here for main menu first run illuminated
.bonusDiamondActive	skip 1			; diamond bonus is active if != 0
.bonusSpecialActive	skip 1			; special bonus is active if != 0
.bonusExtraActive	skip 1			; extra bonus is active if != 0

.scoreMultiplier	skip 1			; multiplier 0=x1, 1=x2, 2=x3, 3=x5

.vegetableImg		equb centerCucumber	; vegetable image number for current level, , initialize here for main menu first run

.bonusItemActive	skip 1			; center bonus item active if != 0

.vegetableScore		equb &10		; vegetable score value (bcd), initialize here for main menu first run

.vegetableScoreActive	skip 1			; vegetable score displayed if != 0

.objectScoreImg		skip 1			; object score image index to display if != 0,
.objectScoreX		skip 1			; object score x position
.objectScoreY		skip 1			; object score y position

.level			equb 1			; game level (BCD), initialize here for main menu first run
.levelEndActive		skip 1			; level has ended if != 0

.levelEdibles		skip 1			; number of edible objects (dots, letters, hearts) remaining in current level
.levelSkulls		skip 1			; number of skulls in current level
.levelLetters		skip 3			; 3 random letters for current level

.shield			skip 1			; number of rounds remaining that ladybug is protected against skulls, 0 = no shield

.objectModeTimer	skip 1			; timer for object mode change
.objectMode		skip 1			; current object mode 0=cyan, 1=red, 2=yellow

.pauseGame		skip 1			; pause the whole game if != 0
.pauseLadybug		skip 1			; number of frames to pause ladybug movement
.pauseEnemy		skip 1			; number of frames to pause enemy movement

.enemySpeed		skip 1			; enemy speed fraction
.enemySpeedCounter	skip 1			; enemy speed fraction counter

.enemyReleaseEnable	skip 1			; enables release of enemy when != 0
.enemyReleaseFrame	skip 1			; frame number to release enemy

.enemiesActive		skip 1			; number of enemies currectly active
.enemyTimer		skip 1			; enemy release timer counter 0-87, enemy released when = 0 and enemy release enable != 0
.enemyTimerSpeed	skip 1			; enemy release timer speed (frames) level 1=8, level 2-4=5, level 5-99=3
.enemyTimerSpeedCounter skip 1			; enemy release timer speed counter (frame counter)

.enemySpawnSaveX	skip 1			; preserve register

.randomSeed		skip 2			; random number generator seed

.addScoreMultiplySaveA	skip 1			; preserve register
.addScoreMultiplySaveX	skip 1			; preserve register

.checkForObjectSaveX	skip 1			; preserve register

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

.spriteToAddrX		skip 1			; spriteToAddr sprite x position
.spriteToAddrY		skip 1			; spritetoAddr sprite y position
.spriteToAddrSaveX	skip 1			; preserve register
.spriteToAddrSaveY	skip 1			; preserve register

.tileMapAddr		skip 2			; tileMap address used by various routines

	;---------------------------------------------------------------------------------------------------------------------------------------------

.moveDirMap		equb 1, 47, 23, 25, 24	; tileMap offset from top left corner to up, down, left, right, center
	
.moveSpritesPathCounter	skip 1			; counter for number of paths for valid junction

.moveSpritesSaveX	skip 1			; preserve register

.moveSpritesSaveDirection
			skip 1			; save original sprite direction while calculating new path direction in enemy aim logic

.moveSpritesIndex	skip 1			; index of current sprite

	;---------------------------------------------------------------------------------------------------------------------------------------------

.spritesImg		skip spritesTotal	; sprite image, position and direction for drawing
.spritesX		skip spritesTotal
.spritesY		skip spritesTotal
.spritesDir		skip spritesTotal

.spritesErased		skip spritesTotal	; sprite erased flag, position and direction for erasing
.spritesEraseX		skip spritesTotal
.spritesEraseY		skip spritesTotal
.spritesEraseDir	skip spritesTotal

.spritesImgFrameCounter	equb 1			; timer for sprite animation frame speed
.spritesImgFrame	equb 0			; current sprite animation frame 0-3

.drawSpriteImg		skip 1			; drawSprite img
.drawSpriteX		skip 1			; drawSprite X position
.drawSpriteY		skip 1			; drawSprite Y position

.drawSpriteScreenAddr	skip 2			; calculated screen address from sprite x and y position

.drawSpriteSaveX	skip 1			; preserve register
.drawSpriteSaveY	skip 1			; preserve register

.spriteToScreenSaveX	skip 1			; preserve register
.spriteToScreenSaveY	skip 1			; preserve register

.eraseSpriteSaveX	skip 1			; preserve register
.eraseSpriteSaveY	skip 1			; preserve register

.eraseBlockBytes	skip 1			; byte count for full block erase

.redrawSpritesCount	skip 1			; counter for sprite list length

.redrawSpritesMax	skip 1			; counter for maximum number of sprites processed in 1 frame

.redrawSpritesIndexUpper			; index to current sprite in list for upper sprites
			equb 1

.redrawSpritesIndexLower			; index to current sprite in list for lower half
			equb 1

.updateObjectTimerSaveX	skip 1			; preserve register

.drawMapTileSaveA	skip 1			; preserve register
.drawMapTileSaveY	skip 1			; preserve register

soundChannels		= 6			; number of software defined sound channels
.soundAddrPtr		skip soundChannels * 2	; pointers to sound effect data
.soundTimers		skip soundChannels	; timers for sound effect data

.playSoundAddr		skip 2			; storage for sound table address
.playSoundSaveA		skip 1			; preserve register
.playSoundSaveX		skip 1			; preserve register
.playSoundSaveY		skip 1			; preserve register

.animateLadybugActive	skip 1			; ladybug movement animation activate when != 0
.animateLadybugAddr	skip 2			; address pointer to ladybug animation tables
.animateLadybugCounter	skip 1			; frame counter for ladybug animation

.nameRegCursor		skip 1			; high score name registration cursor position of selected letter
.nameRegCursorOld	skip 1			; high score name registration cursor position of previous selected letter
.nameRegCursorText	skip 1			; high score name registration cursor position in player name text

.mainMenuCursor		skip 1			; main menu cursor position
.mainMenuCursorOld	skip 1			; main menu previous cursor position

.keyboardScanSaveX	skip 1			; preserve register

.drawScoreTableZero	skip 1			; leading zero blanking flag for drawing score table

.initPlayfieldMiddleRows

			skip 1			; row counter for maze initialization

.playerInput		skip 1			; player input bits (see constants.asm)
.joystickInput		skip 1			; joystick input bits (see constants.asm)
.joystickAnalogueSave	skip 1			; preserve analogue joystick value

.updateLadybugOldDir	skip 1			; copy of ladybug direction

.updateLadybugNewDirX	skip 1			; new X direction for ladybug
.updateLadybugNewDirY	skip 1			; new Y direction for ladybug

.updateLadybugGridX	skip 1			; = 0 if ladybug x on exact grid
.updateLadybugGridY	skip 1			; = 0 if ladybug y on exact grid

.updateLadybugTileX	skip 1			; tile found in front of ladybug horiontally
.updateLadybugTileY	skip 1			; tile found in front of ladybug vertically

.updateLadybugSave	skip 1			; preserve tile

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; end of pageZero
;-----------------------------------------------------------------------------------------------------------------------------------------------------
.pageZeroEnd
	skip 0					; show this address in listing
;-----------------------------------------------------------------------------------------------------------------------------------------------------



print
print
print
