;-----------------------------------------------------------------------------------------------------------------------------------------------------
; game constants
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " constants.asm"
	print "----------------------------------------------------"
	print

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; bonus constants (bcd)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

bonusSpecialScore	= 2			; special bonus score 2 * 100,000
bonusSpecialShield	= 6			; special bonus skull shield lasts 6 levels

bonusExtraLives		= 2			; extra bonus 2 extra lives

bonusDiamondLevel	= 6			; level for releasing the diamond (if diamond bonus is enabled)
bonusDiamondScore	= &10			; diamond bonus score value 10 * 100,000

;-----------------------------------------------------------------------------------------------------------------------------------------------------

magicNumber		= &69			; used for random seed initialization, data validation code generation and swr test

spritesTotal		= 5			; total number of sprites in game (1 for lady bug and 4 for enemies)

spritesAnimationSpeed	= 8			; number of vsyncs per animation frame (animation speed = 6.25Hz (50Hz / 8))

ladybugEnemyRange	= 6			; range allowed between enemy and ladybug to detect as a hit

frame			= 50			; 1 second = 50 * 50Hz vsync frames
pause			= 25			; 1 second pause = 25 * 25Hz

escTime			= frame * 2.00		; hold esc for 2.00 seconds to quit game and return back to menu

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

objectModeCyanTime	= pause * 7.00		; 7.00 second cyan objects
objectModeRedTime	= pause * 0.60		; 0.60 second red objects
objectModeYellowTime	= pause * 2.50		; 2.50 second yellow objects

objectModeCyan		= 0			; object modes
objectModeRed		= 1
objectModeYellow	= 2

enemyTimerTopLeft	= 78			; top left tile index for enemy timer for triggering enemy release warning
enemyTimerMax		= 87			; highest index for enemy timer, index reset to 0 when it increments to enemyTimerMax + 1

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; ascii chr reassignment
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

ladybugStartX		= 88			; sprite coordinates for ladybug starting position
ladybugStartY		= 136

centerBoxX		= 88			; sprite coordinates for center box
centerBoxY		= 88

vegetableScoreX		= 10			; tile coordinates for vegetable score
vegetableScoreY		= 12

lowerDiamondX		= 20			; sprite coordinates for diamond available indicator
lowerDiamondY		= 197



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; analogue and digital joystick
;-----------------------------------------------------------------------------------------------------------------------------------------------------

adcControlB		= &fec0			; model b/b+ udp 7002 control register
adcHighB		= &fec1			; model b/b+ udp 7002 high byte of analogue data
adcLowB			= &fec2			; model b/b+ udp 7002 low byte of analogue data

adcControlM		= &fe18			; master 128 udp 7002 control register
adcHighM		= &fe19			; master 128 udp 7002 high byte of analogue data
adcLowM			= &fe1a			; master 128 udp 7002 low byte of analogue data

joystickFireAnalogue	= %00010000		; analogue joystick fire bit mask

joystickFire		= %00000001		; digital joystick fire bit mask
joystickLeft		= %00000010		; digital joystick left bit mask
joystickDown		= %00000100		; digital joystick down bit mask
joystickUp		= %00001000		; digital joystick up bit mask
joystickRight		= %00010000		; digital joystick right bit mask



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; playerInput bit assignments
;-----------------------------------------------------------------------------------------------------------------------------------------------------

keyBitStart		= %00000001		; internal player control bit masks used by keyboard and joystick
keyBitLeft		= %00000010
keyBitDown		= %00000100
keyBitUp		= %00001000
keyBitRight		= %00010000
keyBitEsc		= %00100000



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; key scan codes
;-----------------------------------------------------------------------------------------------------------------------------------------------------

keyEsc			= &70			; keyboard matrix hardware scan codes
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
; opcodes used in self modifying code
;-----------------------------------------------------------------------------------------------------------------------------------------------------

opcodeBCC		= &90			; 6502 opcode for BCC instruction (used in self modifying code for sprite functions)
opcodeBCS		= &b0			; 6502 opcode for BCS instruction (used in self modifying code for sprite functions)
opcodeDEX		= &ca			; 6502 opcode for DEX instruction (used in self modifying code for sprite functions)
opcodeINX		= &e8			; 6502 opcode for INX instruction (used in self modifying code for sprite functions)

opcodeRTI		= &40			; 6502 opcode for RTI instruction (used in clean reset code)



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; 8 and 16 bit place holders used in self modifying code
;-----------------------------------------------------------------------------------------------------------------------------------------------------

dummy8			= &ff			; dummy byte place holder (used in self modifying code in multiple functions)
dummy16			= &ff00			; dummy word place holder (used in self modifying code in multiple functions)



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; sprite control bits
;-----------------------------------------------------------------------------------------------------------------------------------------------------

moveUp			= %0000			; bits 0 and 1 control the direction
moveDown		= %0001
moveLeft		= %0010
moveRight		= %0011

moveStop		= %0100			; bit 2 when set stops the sprite moving
spriteBlanking		= %1000			; bit 3 when set prevents the sprite being drawn (movement is still processed unless moveStop is also set)



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; center bonus vegetable and diamond image tiles
;-----------------------------------------------------------------------------------------------------------------------------------------------------

centerCucumber		= 0
centerEggplant		= 1
centerCarrot		= 2
centerRadish		= 3
centerParsley		= 4
centerTomato		= 5
centerPumpkin		= 6
centerBambooShoot	= 7
centerJapaneseRadish	= 8
centerMushroom		= 9
centerPotato		= 10
centerOnion		= 11
centerChineseCabbage	= 12
centerTurnip		= 13
centerGreenChilli	= 14
centerCelery		= 15
centerSweetPotato	= 16
centerHorseradish	= 17
centerDiamond		= 18



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; maze tile codes
;-----------------------------------------------------------------------------------------------------------------------------------------------------

wallSolid		= %11000000		; bits 7,6 = 1,1 solid tile to enemy and ladybug (timer blocks and maze walls)
wallTurnstile		= %10000000		; bits 7,6 = 1,0 solid tile to enemy only (turnstiles)

mapTileBlank		= 0			; empty tile

mapTileDot		= 1			; dot tile

mapTileTimerTopLeft	= 2			; timer tiles
mapTileTimerTop		= 4
mapTileTimerTopRight	= 6
mapTileTimerLeft	= 8
mapTileTimerRight	= 10
mapTileTimerBottomLeft	= 12
mapTileTimerBottom	= 14
mapTileTimerBottomRight	= 16

mapTileTopLeft		= 18			; maze tiles
mapTileTopRight		= 19
mapTileBottomLeft	= 20
mapTileBottomRight	= 21
mapTileVertical		= 22
mapTileHorizontal	= 23
mapTileVerticalD	= 24
mapTileVerticalU	= 25
mapTileHorizontalR	= 26
mapTileHorizontalL	= 27



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; maze turnstile tile codes
;-----------------------------------------------------------------------------------------------------------------------------------------------------

mapTileTurnstileCV	= 28			; center vertical
mapTileTurnstileCH	= 29			; center horizontal
mapTileTurnstileD	= 30			; down
mapTileTurnstileU	= 31			; up
mapTileTurnstileR	= 32			; right
mapTileTurnstileL	= 33			; left



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; object tile codes
;-----------------------------------------------------------------------------------------------------------------------------------------------------

objectTileIndex		= 34			; object tile images start at this index

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; letters, heart, skull that change color
;-----------------------------------------------------------------------------------------------------------------------------------------------------

mapTileS		= objectTileIndex + 0	; S
mapTileP		= objectTileIndex + 1	; P
mapTileE		= objectTileIndex + 2	; E
mapTileC		= objectTileIndex + 3	; C
mapTileI		= objectTileIndex + 4	; I
mapTileA		= objectTileIndex + 5	; A
mapTileL		= objectTileIndex + 6	; L
mapTileX		= objectTileIndex + 7	; X
mapTileT		= objectTileIndex + 8	; T
mapTileR		= objectTileIndex + 9	; R
mapTileSkull		= objectTileIndex + 10	; skull
mapTileHeart		= objectTileIndex + 11	; heart

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; letters and heart with fixed color (used on level intro screen)
;-----------------------------------------------------------------------------------------------------------------------------------------------------

mapTileCyanHeart	= objectTileIndex + 12
mapTileYellowE		= objectTileIndex + 13
mapTileYellowX		= objectTileIndex + 14
mapTileYellowT		= objectTileIndex + 15
mapTileYellowR		= objectTileIndex + 16
mapTileYellowA		= objectTileIndex + 17

;-----------------------------------------------------------------------------------------------------------------------------------------------------

mapTileBlankObj		= objectTileIndex + 18



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; extra tiles codes
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
; name registration cursor box tile codes
;-----------------------------------------------------------------------------------------------------------------------------------------------------

registrationTL		= 90			; name registration cursor box
registrationTR		= 91
registrationBL		= 92
registrationBR		= 93
registrationVL		= 94
registrationVR		= 95
registrationTH		= 96
registrationBH		= 97



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; sprite base codes
;-----------------------------------------------------------------------------------------------------------------------------------------------------

spriteVegetables	= 0			; vegetables 0-17
spriteDiamond		= 18			; diamond 18
spritePoints		= 19			; object points 19-30
spriteLadybug		= 31			; ladybug 31-45
spriteEnemy1		= 46			; enemy1 46-60
spriteEnemy2		= 61			; enemy2 61-75
spriteEnemy3		= 76			; enemy3 76-90
spriteEnemy4		= 91			; enemy4 91-105
spriteEnemy5		= 106			; enemy5 106-120
spriteEnemy6		= 121			; enemy6 121-135
spriteEnemy7		= 136			; enemy7 136-150
spriteEnemy8		= 151			; enemy8 151-165
spriteAngel0		= 166			; angel0 166-167
spriteLowerDiamond	= 168			; lower diamond 168-169
spriteAngel1		= 170			; angel1 170-171



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; tile/sprite sizes and bytes
;-----------------------------------------------------------------------------------------------------------------------------------------------------

mapTileWidth		= 6
mapTileHeight		= 8
mapTileBytes		= mapTileHeight * mapTileWidth / 2 

	;---------------------------------------------------------------------------------------------------------------------------------------------

diamondTileWidth	= 8
diamondTileHeight	= 6
diamondTileBytes	= diamondTileHeight * diamondTileWidth / 2 

	;---------------------------------------------------------------------------------------------------------------------------------------------

spriteTileWidth		= 10
spriteTileHeight	= 14
spriteTileBytes		= spriteTileHeight * spriteTileWidth / 2 

	;---------------------------------------------------------------------------------------------------------------------------------------------

objectTileWidth		= 8
objectTileHeight	= 8
objectTileBytes		= objectTileHeight * objectTileWidth / 2 

	;---------------------------------------------------------------------------------------------------------------------------------------------

sprite10x10Width	= 10
sprite10x10Height	= 10
sprite10x10Bytes	= sprite10x10Height * sprite10x10Width / 2 

	;---------------------------------------------------------------------------------------------------------------------------------------------

miniFontWidth		= 4
miniFontHeight		= 8
miniFontBytes		= miniFontHeight * miniFontWidth / 2



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; image base (index) for ladybug, enemies and angel
;-----------------------------------------------------------------------------------------------------------------------------------------------------

imgLadybug		= 0
imgEnemies		= 1
imgAngel		= 9


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; screen constants
;-----------------------------------------------------------------------------------------------------------------------------------------------------

screenWidth		= 69			; screen is 69 bytes wide (138 pixels)
screenHeight		= 26			; screen is 26 rows of 8 lines high (208 raster lines)

						; start address of screen
screenAddr		= &8000 - screenWidth * screenHeight * 8

screenAddrMode1		= &3000			; start address of mode 1 screen for editor.bas
screenSizeMode1		= &5000			; size of mode 1 screen

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; screen character sizes
;-----------------------------------------------------------------------------------------------------------------------------------------------------

column			= 8			; 8 bytes to next column
chrColumn		= 3 * column		; 24 bytes to next chr column
chrRow			= screenWidth * column	; 552 bytes to next chr row



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; ula palette color values
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
; ula palette register index
;-----------------------------------------------------------------------------------------------------------------------------------------------------

pal0			= &00			; palette register index for regular colors
pal1			= &10
pal2			= &20
pal3			= &30
pal4			= &40
pal5			= &50
pal6			= &60
pal7			= &70

palMultiplier0		= &80			; palette register index for flashing colors
palMultiplier1		= &90
palSpecial0		= &a0
palSpecial1		= &b0
palExtra0		= &c0
palExtra1		= &d0
palSkull		= &e0
palObject		= &f0



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; draw chr color index
;-----------------------------------------------------------------------------------------------------------------------------------------------------

colorBlack		= 0
colorRed		= 1
colorGreen		= 2
colorYellow		= 3
colorBlue		= 4
colorMagenta		= 5
colorCyan		= 6
colorWhite		= 7
colorMultiplier0	= 8
colorMultiplier1	= 9
colorSpecial0		= 10
colorSpecial1		= 11
colorExtra0		= 12
colorExtra1		= 13
colorSkull		= 14
colorObject		= 15



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; pixel mask values for chr colors
;-----------------------------------------------------------------------------------------------------------------------------------------------------

pixelsBlack		= &00			; pixel mask color values used for text drawing and screen erase
pixelsRed		= &03
pixelsGreen		= &0c
pixelsYellow		= &0f
pixelsBlue		= &30
pixelsMagenta		= &33
pixelsCyan		= &3c
pixelsWhite		= &3f
pixelsMultiplier0	= &c0
pixelsMultiplier1	= &c3
pixelsSpecial0		= &cc
pixelsSpecial1		= &cf
pixelsExtra0		= &f0
pixelsExtra1		= &f3
pixelsSkull		= &fc
pixelsObject		= &ff



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; os vectors and functions
;-----------------------------------------------------------------------------------------------------------------------------------------------------

irqAcc			= &00fc			; bbc os irq1 interrupt stores accumilator here
irqVector		= &0204			; bbc os irq1 interrupt vector
fx200			= &0258			; bbc os reads the *fx 200 value from here
breakVector		= &0287			; bbc os jumps here on break key reset
resetVector		= &fffc			; bbc os reset vector
bankSelectCopy		= &00f4			; bbc os stores current sideways bank copy here
bankSelect		= &fe30			; bank select register for acorn/other
bankSelectSolidisk	= &fe32			; bank select register for solidisk
bankSelectWatford	= &ff30			; bank select register (&ff30 - &ff3f) for write operations with watford electronics sideways ram

acccon			= &fe34			; access control register
osbyte			= &fff4			; os function
oscli			= &fff7			; os function
osfile			= &ffdd			; os function
oswrch			= &ffee			; os function



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; define important program areas
;-----------------------------------------------------------------------------------------------------------------------------------------------------

pageZero		= &0000			; all variables here
page0100		= &0100			; page 1 functions and stack
page0130		= &0130			; clean reset code
page0200		= &0200			; os vector redirect and misc functions
pagefx200		= fx200			; os fx 200
pageBreak		= breakVector		; os break key intercept
pageNmi			= &0d00			; os nmi
pageCls			= &2b00			; clear screen function for editor.bas
pageConfig		= &7b80			; config / high score table load address
pageHigh		= &8000			; high ram / sideways ram

	;---------------------------------------------------------------------------------------------------------------------------------------------

progReloc		= &0000			; relocation address of program
progLoad		= &2000			; load address of program
progOffset		= progLoad - progReloc	; relocation offset

swramStart		= pageHigh		; high/sideways ram start address
swramEnd		= pageHigh + &3000	; high/sideways ram end address (12K only for compatibility with B+ 64K model)

	;---------------------------------------------------------------------------------------------------------------------------------------------

map1Load		= &7800			; map addresses used by boot.bas to load the 3 maps before running lady bug
map2Load		= &7900
map3Load		= &7a00

	;---------------------------------------------------------------------------------------------------------------------------------------------

canvasBoot		= &f000			; temporary canvas address for !Boot file generation
canvasMapNames		= &f100			; temporary canvas address for _Maps file generation
canvasBonusSettings	= &f200			; temporary canvas address for _Bonus file generation
canvasEditor		= &f300			; temporary canvas address for tile drawing code used in editor.bas
pageEditor		= &2b00			; editor code runs at this address
						; offset from canvas to real editor code address
pageEditorOffset	= canvasEditor - pageEditor
canvasCls		= &f400			; temporary canvas address for cls code used in editor.bas



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; 6522 via 1
;-----------------------------------------------------------------------------------------------------------------------------------------------------

via1PortB		= &fe40			; via port B data
via1PortDdrB		= &fe42			; via port B io control

via1PortDdrA		= &fe43			; via Port A io control

via1T1CounterLo		= &fe44			; via timer 1 counter low
via1T1CounterHi		= &fe45			; via timer 1 counter high
via1T1LatchLo		= &fe46			; via timer 1 latch low
via1T1LatchHi		= &fe47			; via timer 1 latch high

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

crtcAddr		= &fe00			; crtc address
crtcData		= &fe01			; crtc data
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
; misc
;-----------------------------------------------------------------------------------------------------------------------------------------------------

quote			= '"'			; ascii code for quotation



;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print
