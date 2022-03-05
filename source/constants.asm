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

true			= TRUE			; because caps burn the eyes !
false			= FALSE

magicNumber		= &69			; used for random seed, validation generation, swr test


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
; playerInput bit assignments
;-----------------------------------------------------------------------------------------------------------------------------------------------------

keyBitUp		= &01
keyBitDown		= &02
keyBitLeft		= &04
keyBitRight		= &08
keyBitStart		= &10
keyBitEsc		= &20


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

addr8			= &ff			; address place holders (used in self modifying code in multiple functions)
addr16			= &ff00

;-----------------------------------------------------------------------------------------------------------------------------------------------------

moveUp			= 0			; bits 0 and 1 control the direction
moveDown		= 1
moveLeft		= 2
moveRight		= 3

moveStop		= &04			; bit 2 when set stops the sprite moving
spriteBlanking		= &08			; bit 3 when set prevents the sprite being drawn although movement is still processed unless moveStop is set

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
pageVectors		= &0200			; os vector redirect
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
; 6522 via
;-----------------------------------------------------------------------------------------------------------------------------------------------------

viaPortB		= &fe40			; via port B data
viaPortDdrB		= &fe42			; via port B io control

viaPortDdrA		= &fe43			; via Port A io control

viaT1CounterLo		= &fe44			; via timer 1 low counter
viaT1CounterHi		= &fe45			; via timer 1 high counter
viaT1LatchLo		= &fe46			; via timer 1 low latch
viaT1LatchHi		= &fe47			; via timer 1 high latch

viaT2CounterLo		= &fe48			; via timer 2 counter low
viaT2CounterHi		= &fe49			; via timer 2 counter high

viaAcr			= &fe4b			; via auxiliary control register

viaIfr			= &fe4d			; via interrupt flags
via2Ifr			= &fe6d			; via interrupt flags
viaIer			= &fe4e			; via interrupt enable (via #1)
via2Ier			= &fe6e			; via interrupt enable (via #2)

viaPortA		= &fe4f			; via Port A data (no handshake)

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; video chips
;-----------------------------------------------------------------------------------------------------------------------------------------------------

crtcAddr		= &fe00			; 6845 address
crtcData		= &fe01			; 6845 data
ulaMode			= &fe20			; ula video mode
ulaPalette		= &fe21			; ula color palette

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print
