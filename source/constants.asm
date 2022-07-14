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
; important constants
;-----------------------------------------------------------------------------------------------------------------------------------------------------

magicNumber		= &69			; used for random seed, validation generation, swr test

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; game constants
;-----------------------------------------------------------------------------------------------------------------------------------------------------

spritesTotal		= 5			; total number of sprites in game (1 for lady bug and 4 for enemies)

spritesAnimationSpeed	= 8			; number of vsyncs per animation frame (6.25Hz)

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



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; object positions
;-----------------------------------------------------------------------------------------------------------------------------------------------------

ladybugStartX		= 88			; sprite coordinates in pixels
ladybugStartY		= 136

centerBoxX		= 88			; sprite coordinates in pixels
centerBoxY		= 88

vegetableScoreX		= 10			; tile coordinates
vegetableScoreY		= 12



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; analogue and digital joystick
;-----------------------------------------------------------------------------------------------------------------------------------------------------

adcControlB		= &fec0			; model b/b+ udp 7002 control register
adcHighB		= &fec1			; model b/b+ udp 7002 high byte of analogue data
adcLowB			= &fec2			; model b/b+ udp 7002 low byte of analogue data

adcControlM		= &fe18			; master 128 udp 7002 control register
adcHighM		= &fe19			; master 128 udp 7002 high byte of analogue data
adcLowM			= &fe1a			; master 128 udp 7002 low byte of analogue data

joystickFireAnalogue	= %00010000		; analogue joystick fire

joystickFire		= %00000001		; digital joystick fire
joystickLeft		= %00000010		; digital joystick left
joystickDown		= %00000100		; digital joystick down
joystickUp		= %00001000		; digital joystick up
joystickRight		= %00010000		; digital joystick right

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; playerInput bit assignments
;-----------------------------------------------------------------------------------------------------------------------------------------------------

keyBitStart		= %00000001
keyBitLeft		= %00000010
keyBitDown		= %00000100
keyBitUp		= %00001000
keyBitRight		= %00010000
keyBitEsc		= %00100000


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; key scan codes
;-----------------------------------------------------------------------------------------------------------------------------------------------------

keyEsc			= &70			; keyboard hardware scan codes
key1			= &30
key2			= &31
key3			= &11
key4			= &12
key5			= &13
key6			= &34
key7			= &24
key8			= &15
key9			= &26
key0			= &27
keyMinus		= &17
keyRaise		= &18
keyBackslash		= &78
keyLeft			= &19
keyRight		= &79

keyTab			= &60
keyQ			= &10
keyW			= &21
keyE			= &22
keyR			= &33
keyT			= &23
keyY			= &44
keyU			= &35
keyI			= &25
keyO			= &36
keyP			= &37
keyAt			= &47
keyBracketOpen		= &38
keyUnderscore		= &28
keyUp			= &39
keyDown			= &29

keyCapslock		= &40
keyCtrl			= &01
keyA			= &41
keyS			= &51
keyD			= &32
keyF			= &43
keyG			= &53
keyH			= &54
keyJ			= &45
keyK			= &46
keyL			= &56
keySemicolon		= &57
keyColon		= &48
keyBracketClosed	= &58
keyReturn		= &49

keyShiftLock		= &50
keyShift		= &00
keyZ			= &61
keyX			= &42
keyC			= &52
keyV			= &63
keyB			= &64
keyN			= &55
keyM			= &65
keyComma		= &66
keyPeriod		= &67
keySlash		= &68
keyDel			= &59
keyCopy			= &69

keySpace		= &62



;-----------------------------------------------------------------------------------------------------------------------------------------------------

opcodeBCC		= &90			; 6502 opcode for BCC instruction (used for self modifying code in sprite functions)
opcodeBCS		= &b0			; 6502 opcode for BCS instruction (used for self modifying code in sprite functions)
opcodeDEX		= &ca			; 6502 opcode for DEX instruction (used for self modifying code in sprite functions)
opcodeINX		= &e8			; 6502 opcode for INX instruction (used for self modifying code in sprite functions)
opcodeRTI		= &40			; 6502 opcode for RTI instruction

;-----------------------------------------------------------------------------------------------------------------------------------------------------

dummy8			= &ff			; dummy byte place holder (used in self modifying code in multiple functions)
dummy16			= &ff00

;-----------------------------------------------------------------------------------------------------------------------------------------------------

moveUp			= %0000			; bits 0 and 1 control the direction
moveDown		= %0001
moveLeft		= %0010
moveRight		= %0011

moveStop		= %0100			; bit 2 when set stops the sprite moving
spriteBlanking		= %1000			; bit 3 when set prevents the sprite being drawn (movement is still processed unless moveStop is also set)

;-----------------------------------------------------------------------------------------------------------------------------------------------------

wallSolid		= &c0			; solid tile to enemy and player
wallTurnstile		= &80			; solid tile to enemy only

mapTileBlank		= &00			; empty tile

mapTileDot		= &01			; dot tile

mapTileTimerTopLeft	= &02			; timer tiles
mapTileTimerTop		= &04
mapTileTimerTopRight	= &06
mapTileTimerLeft	= &08
mapTileTimerRight	= &0a
mapTileTimerBottomLeft	= &0c
mapTileTimerBottom	= &0e
mapTileTimerBottomRight	= &10


;-----------------------------------------------------------------------------------------------------------------------------------------------------

mapTileVerticalBar	= &16			; maze tile

mapTileTurnstileCV	= &1c			; turnstile tiles
mapTileTurnstileCH	= &1d
mapTileTurnstileD	= &1e
mapTileTurnstileU	= &1f
mapTileTurnstileR	= &20
mapTileTurnstileL	= &21

objectTileIndex		= &22			; object tile images start at this index

mapTileS		= objectTileIndex + 0
mapTileP		= objectTileIndex + 1
mapTileE		= objectTileIndex + 2
mapTileC		= objectTileIndex + 3
mapTileI		= objectTileIndex + 4
mapTileA		= objectTileIndex + 5
mapTileL		= objectTileIndex + 6
mapTileX		= objectTileIndex + 7
mapTileT		= objectTileIndex + 8
mapTileR		= objectTileIndex + 9
mapTileSkull		= objectTileIndex + 10
mapTileHeart		= objectTileIndex + 11
mapTilecyanHeart	= objectTileIndex + 12
mapTileYellowE		= objectTileIndex + 13
mapTileYellowX		= objectTileIndex + 14
mapTileYellowT		= objectTileIndex + 15
mapTileYellowR		= objectTileIndex + 16
mapTileYellowA		= objectTileIndex + 17
mapTileBlankObj		= objectTileIndex + 18

;-----------------------------------------------------------------------------------------------------------------------------------------------------

extraTileDigits		= 0			; digits 0-9
extraTileBlank		= 10			; blank tile
extraTileDiamond	= 11			; first tile of large diamond
extraTileLogo		= 27			; first tile of ladybug logo
extraTileUpper		= 63			; first tile of upper playfield bonus holders

extraTileLeavesL	= 72			; leaves
extraTileLeavesR	= 73

extraTileFlower0TL	= 74			; flowers
extraTileFlower0TR	= 75
extraTileFlower0BL	= 76
extraTileFlower0BR	= 77

extraTileFlower1TL	= 78
extraTileFlower1TR	= 79
extraTileFlower1BL	= 80
extraTileFlower1BR	= 81

extraTileFlower2TL	= 82
extraTileFlower2TR	= 83
extraTileFlower2BL	= 84
extraTileFlower2BR	= 85

extraTileFlower3TL	= 86
extraTileFlower3TR	= 87
extraTileFlower3BL	= 88
extraTileFlower3BR	= 89



;-----------------------------------------------------------------------------------------------------------------------------------------------------

						; name registration letter box

registrationTL		= 90
registrationTR		= 91
registrationBL		= 92
registrationBR		= 93
registrationVL		= 94
registrationVR		= 95
registrationTH		= 96
registrationBH		= 97

;-----------------------------------------------------------------------------------------------------------------------------------------------------

mapTileWidth		= 6
mapTileHeight		= 8
mapTileBytes		= mapTileHeight * mapTileWidth / 2 

;-----------------------------------------------------------------------------------------------------------------------------------------------------

spriteTileWidth		= 10
spriteTileHeight	= 14
spriteTileBytes		= spriteTileHeight * spriteTileWidth / 2 

;-----------------------------------------------------------------------------------------------------------------------------------------------------

objectTileWidth		= 8
objectTileHeight	= 8
objectTileBytes		= objectTileHeight * objectTileWidth / 2 

;-----------------------------------------------------------------------------------------------------------------------------------------------------

sprite10x10Width	= 10
sprite10x10Height	= 10
sprite10x10Bytes	= sprite10x10Height * sprite10x10Width / 2 

;-----------------------------------------------------------------------------------------------------------------------------------------------------

miniFontWidth		= 4
miniFontHeight		= 8
miniFontBytes		= miniFontHeight * miniFontWidth / 2

;-----------------------------------------------------------------------------------------------------------------------------------------------------

screenWidth		= 69			; screen is 69 bytes wide (138 pixels)
screenHeight		= 26			; screen is 26 * 8 lines high (208 raster lines)

screenAddr		= &8000 - screenWidth * screenHeight * 8

;-----------------------------------------------------------------------------------------------------------------------------------------------------

column			= 8			; 8 bytes to next column
chrColumn		= 3 * column		; 24 bytes to next chr column
chrRow			= screenWidth * column	; 552 bytes to next chr row

;-----------------------------------------------------------------------------------------------------------------------------------------------------

palBlack		= 0 eor 7		; palette color values
palRed			= 1 eor 7
palGreen		= 2 eor 7
palYellow		= 3 eor 7
palBlue			= 4 eor 7
palMagenta		= 5 eor 7
palCyan			= 6 eor 7
palWhite		= 7 eor 7

;-----------------------------------------------------------------------------------------------------------------------------------------------------

palSpecial0		= &a0			; palette color change indexs
palSpecial1		= &b0
palExtra0		= &c0
palExtra1		= &d0
palSkull		= &e0
palObject		= &f0

;-----------------------------------------------------------------------------------------------------------------------------------------------------

pixels0			= &00			; pixel mask color values
pixels1			= &03
pixels2			= &0c
pixels3			= &0f
pixels4			= &30
pixels5			= &33
pixels6			= &3c
pixels7			= &3f
pixelsUnused0		= &c0
pixelsUnused1		= &c3
pixelsSpecial0		= &cc
pixelsSpecial1		= &cf
pixelsExtra0		= &f0
pixelsExtra1		= &f3
pixelsSkull		= &fc
pixelsObject		= &ff

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; os vectors and functions
;-----------------------------------------------------------------------------------------------------------------------------------------------------

irqAcc			= &fc			; irq1 bbc os stores accumilator here
irqVector		= &204			; irq1 jump vector
fx200			= &0258			; *fx 200 value
breakVector		= &0287			; break key reset jump
resetVector		= &fffc			; os reset vector
bankSelectCopy		= &f4			; os stores current bank copy here
bankSelect		= &fe30			; bank select register for acorn/other
bankSelectSolidisk	= &fe32			; bank select register for solidisk
bankSelectWatford	= &ff30			; bank select register for watford electronics during write operations

acccon			= &fe34			; access control register
osbyte			= &fff4			; os function
oscli			= &fff7			; os function
osfile			= &ffdd			; os function
oswrch			= &ffee			; os function

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; define program addresses
;-----------------------------------------------------------------------------------------------------------------------------------------------------

pageZero		= &0000			; all variables here
page0100		= &0100			; page 1 functions
page0130		= &0130			; reset code
page0200		= &0200			; os vector redirect and misc
pagefx200		= &0258			; os fx 200
pageBreak		= &0287			; os break key intercept
pageNmi			= &0d00			; os nmi
pageConfig		= &7b80			; config / high score table load address
pageHigh		= &8000			; high ram

progReloc		= &0000			; relocation address of program
progLoad		= &2000			; load address of program
progOffset		= progLoad - progReloc	; relocation offset

swramStart		= pageHigh		; high/sideways ram, keep it b+ compatible (12K)
swramEnd		= pageHigh + &3000

pageBoot		= &f000			; temporary canvas address for !Boot file generation

mazeNames		= &f100			; temporary canvas address for Maps file generation

maze1Load		= &7800			; maze addresses used by boot.bas to load the 3 maps before running lady bug
maze2Load		= &7900
maze3Load		= &7a00

pageEditor		= &2b00			; editor code runs at this address
pageEditorCanvas	= &f200			; temporary canvas address to assemble code
						; offset
pageEditorOffset	= pageEditorCanvas - pageEditor



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; 6522 via 1
;-----------------------------------------------------------------------------------------------------------------------------------------------------

via1PortB		= &fe40			; via port B data
via1PortDdrB		= &fe42			; via port B io control

via1PortDdrA		= &fe43			; via Port A io control

via1T1CounterLo		= &fe44			; via timer 1 low counter
via1T1CounterHi		= &fe45			; via timer 1 high counter
via1T1LatchLo		= &fe46			; via timer 1 low latch
via1T1LatchHi		= &fe47			; via timer 1 high latch

via1T2CounterLo		= &fe48			; via timer 2 counter low
via1T2CounterHi		= &fe49			; via timer 2 counter high

via1Acr			= &fe4b			; via auxiliary control register

via1Ifr			= &fe4d			; via interrupt flags
via1Ier			= &fe4e			; via interrupt enable

via1PortA		= &fe4f			; via Port A data (no handshake)

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; 6522 via 2
;-----------------------------------------------------------------------------------------------------------------------------------------------------

via2PortB		= &fe60			; via port B data
via2PortDdrB		= &fe62			; via port B io control

via2Ifr			= &fe6d			; via interrupt flags
via2Ier			= &fe6e			; via interrupt enable

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; video control
;-----------------------------------------------------------------------------------------------------------------------------------------------------

crtcAddr		= &fe00			; 6845 address
crtcData		= &fe01			; 6845 data
ulaMode			= &fe20			; ula video mode
ulaPalette		= &fe21			; ula color palette

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; psg and keyboard connected to slow bus
;-----------------------------------------------------------------------------------------------------------------------------------------------------

sbPsg			= 0			; output line 0 connected to psg
sbKeyboard		= 3			; output line 3 connected to keyboard

sbHigh			= 8			; high low values for output lines
sbLow			= 0

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print
