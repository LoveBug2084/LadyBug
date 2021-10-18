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
; important constants
;-----------------------------------------------------------------------------------------------------------------------------------------------------

true			= TRUE			; because caps burn the eyes !
false			= FALSE



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

mapTileBlank		= &00

mapTileDot		= &01

mapTileTimerTopLeft	= &02
;mapTileTimerTopLeft	= &03
mapTileTimerTop		= &04
;mapTileTimerTop	= &05
mapTileTimerTopRight	= &06
;mapTileTimerTopRight	= &07
mapTileTimerLeft	= &08
;mapTileTimerLeft	= &09
mapTileTimerRight	= &0a
;mapTileTimerRight	= &0b
mapTileTimerBottomLeft	= &0c
;mapTileTimerBottomLeft	= &0d
mapTileTimerBottom	= &0e
;mapTileTimerBottom	= &0f
mapTileTimerBottomRight	= &10
;mapTileTimerBottomRight= &11


;-----------------------------------------------------------------------------------------------------------------------------------------------------

mapTileVerticalBar	= &16

mapTileTurnstileCV	= &1c
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
mapTileBlueHeart	= objectTileIndex + 12
mapTileYellowE		= objectTileIndex + 13
mapTileYellowX		= objectTileIndex + 14
mapTileYellowT		= objectTileIndex + 15
mapTileYellowR		= objectTileIndex + 16
mapTileYellowA		= objectTileIndex + 17
mapTileBlankObj		= objectTileIndex + 18

;-----------------------------------------------------------------------------------------------------------------------------------------------------

extraTileDigits		= &00			; digits 0-9
extraTileBlank		= extraTileDigits + 10	; blank tile
extraTileDiamond	= extraTileBlank + 1	; diamond image base
extraTileLogo		= extraTileDiamond + 16	; ladybug logo tile image base
extraTileUpper		= extraTileLogo + 36	; upper playfield bonus holders

extraTileLeavesL	= extraTileUpper + 9	; leaves
extraTileLeavesR	= extraTileUpper + 10

extraTileFlower0TL	= extraTileUpper + 11
extraTileFlower0TR	= extraTileUpper + 12
extraTileFlower0BL	= extraTileUpper + 13
extraTileFlower0BR	= extraTileUpper + 14

extraTileFlower1TL	= extraTileUpper + 15
extraTileFlower1TR	= extraTileUpper + 16
extraTileFlower1BL	= extraTileUpper + 17
extraTileFlower1BR	= extraTileUpper + 18

extraTileFlower2TL	= extraTileUpper + 19
extraTileFlower2TR	= extraTileUpper + 20
extraTileFlower2BL	= extraTileUpper + 21
extraTileFlower2BR	= extraTileUpper + 22

extraTileFlower3TL	= extraTileUpper + 23
extraTileFlower3TR	= extraTileUpper + 24
extraTileFlower3BL	= extraTileUpper + 25
extraTileFlower3BR	= extraTileUpper + 26



;-----------------------------------------------------------------------------------------------------------------------------------------------------

registrationTL		= extraTileFlower3BR + 1
registrationTR		= extraTileFlower3BR + 2
registrationBL		= extraTileFlower3BR + 3
registrationBR		= extraTileFlower3BR + 4
registrationVL		= extraTileFlower3BR + 5
registrationVR		= extraTileFlower3BR + 6
registrationH		= extraTileFlower3BR + 7


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

pixelCol0		= &00			; pixel mask color values
pixelCol1		= &03
pixelCol2		= &0c
pixelCol3		= &0f
pixelCol4		= &30
pixelCol5		= &33
pixelCol6		= &3c
pixelCol7		= &3f
pixelCol8		= &c0
pixelCol9		= &c3
pixelColA		= &cc
pixelColB		= &cf
pixelColC		= &f0
pixelColD		= &f3
pixelColE		= &fc
pixelColF		= &ff

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; os vectors and functions
;-----------------------------------------------------------------------------------------------------------------------------------------------------

irqAcc			= &fc			; irq1 bbc os stores accumilator here
irqVector		= &204			; irq1 jump vector
fx200			= &0258			; *fx 200 value
breakVector		= &0287			; break key reset jump
osByte			= &fff4			; os function
osCli			= &fff7			; os function
osWrch			= &ffee			; os function
resetVector		= &fffc			; os reset vector
bankSelect		= &fe30			; paged rom/ram bank select
bankSelectCopy		= &f4			; os stores current bank copy here

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; define program addresses
;-----------------------------------------------------------------------------------------------------------------------------------------------------

page0000		= &0000
page0100		= &0100
page0130		= &0130
page0200		= &0200
page0258		= &0258
page0287		= &0287
page0d00		= &0d00
page1900		= &1900
page3000		= &3000
page8000		= &8000
pageB000		= &b000
pageC000		= &c000

progReloc		= page0000		; relocation address of program
progLoad		= page3000		; load address of program
						; relocation offset
progOffset		= progLoad - progReloc

swramStart		= page8000		; side ways ram, keep it b+ compatible (12K)
swramEnd		= pageB000

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
