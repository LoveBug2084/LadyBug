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

	skip 16					; filler to prevent any possibility of the beeb detecting this as a valid sideways rom after reset



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; vegetables, points and sprites by drawSprite and share a common address table although the sizes are different
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.spriteBin
	skip 0

.sprite10x10Bin
	skip 0

.vegetablesBin
	skip 0

	incbin "img-vegetables.bin"		; load the 10x10 pixel vegetable sprites into memory

.vegetablesBinEnd

.pointsBin
	skip 0

	incbin "img-points.bin"			; load the 10x10 pixel points sprites into memory

.pointsBinEnd
	skip 0

.sprite10x10BinEnd
	skip 0

	sprite10x10 = (sprite10x10BinEnd - sprite10x10Bin) / sprite10x10Bytes
	pointsBaseImg = (pointsBin - spriteBin) / sprite10x10Bytes

.ladybugBin
	skip 0

	incbin "img-ladybug.bin"		; load ladybug sprite set into memory

.enemy1Bin
	skip 0

	incbin "img-enemy1.bin"			; load 1st enemy sprite set into memory

.enemy2Bin
	skip 0

	incbin "img-enemy2.bin"			; load 2nd enemy sprite set into memory

.enemy3Bin
	skip 0

	incbin "img-enemy3.bin"			; load 3rd enemy sprite set into memory

.enemy4Bin
	skip 0

	incbin "img-enemy4.bin"			; load 4th enemy sprite set into memory

.enemy5Bin
	skip 0

	incbin "img-enemy5.bin"			; load 5th enemy sprite set into memory

.enemy6Bin
	skip 0

	incbin "img-enemy6.bin"			; load 6th enemy sprite set into memory

.enemy7Bin
	skip 0

	incbin "img-enemy7.bin"			; load 7th enemy sprite set into memory

.enemy8Bin
	skip 0

	incbin "img-enemy8.bin"			; load 8th enemy sprite set into memory

.angelBin
	skip 0
	
	incbin "img-angel.bin"			; load ladybug angel sprite set into memory

.spriteBinEnd
	skip 0



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; clean reset code
;-----------------------------------------------------------------------------------------------------------------------------------------------------

zpAddr 			= 0
continueBplus		= &d973

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrCleanResetJmp

	equw swrCleanResetB			; unlikely	Electron
	equw swrCleanResetB			; working	B
	equw swrCleanResetBplus			; working	B+
	equw swrCleanResetB			; not yet	Master
	equw swrCleanResetB			; not yet	Compact
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------
; choose correct machine function
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrCleanReset

	lda swrCleanResetJmp, x			; get jump address for machine function
	sta zpAddr
	lda swrCleanResetJmp + 1, x
	sta zpAddr + 1
	jmp (zpAddr)				; run the function

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; model B reset just use the regular simulated power on reset, stack will be intact
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrCleanResetB

	lda #&7f				; disable all via interrupts (simulate power on state)
	sta viaIer
	
	jmp (resetVector)			; reboot the beeb

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; model B+ clear zp, jump into os to clear 0200-7fff leaving the stack intact
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrCleanResetBplus

	lda #opcodeRTI				; disable nmi
	sta page0d00 

	ldx #&ff				; initialize stack
	txs

	inx					; push 0 on stack (fake via interrupt enable flags)
	txa
	pha

.swrCleanResetBplusLoop
	sta page0000, x				; fill 0000-00ff with 0
	inx
	bne swrCleanResetBplusLoop

	lda #hi(page0200)			; os fill 0200-7fff with 0
	sta zpAddr + 1
	txa
	jmp continueBplus



;-----------------------------------------------------------------------------------------------------------------------------------------------------
align &100
.swramLastAddr
;-----------------------------------------------------------------------------------------------------------------------------------------------------



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; loaderMessages				; various messages for ram test etc
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.loaderMessages

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.loaderBuild

	equb 31,9,11				; position cursor
	equs 133,"Lady",131,"Bug ",130,"Build",135
	incbin "ladybug-build-padded.txt"
	equb &ff				; end

.loaderUsingBank

	equb 31,8,13				; position cursor
	equs "Using sideways ram bank 0"
	equb &ff				; end

.loaderUsingWorkspace

	equb 31,10,13				; position cursor
	equs "Using B+ workspace ram"
	equb &ff				; end

.loaderBank

	equs "0123456789ABCDEF"			; hexadecimal bank number

.loaderRamFailed

	equb 31,9,13				; position cursor
	equs 129,"Sideways ram not found"
	equb 31,0,10				; position cursor
	equb 23,1,1,0,0,0,0,0,0,0		; curson on
	equb &ff				; end

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.runLadybug

	equs "/LadyBug", &0d			; oscli run ladybug



;-----------------------------------------------------------------------------------------------------------------------------------------------------

screenCenter	= &7d90
screenCenterY	= &7e71

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.loaderStart

	lda #19					; wait for vsync
	jsr osByte

	ldx #0					; clear center section of display for messages
	lda #' '

.loaderClearCenter

	sta screenCenter, x
	inx
	cpx #200
	bne loaderClearCenter

	sta screenCenterY			; clear the tail of the 'y'
	sta screenCenterY + 1
	
	ldy #lo(loaderBuild - loaderReloc)	; display build number
	jsr loaderPrint - loaderReloc

	sei					; disable interrupts

	lda bankSelectCopy			; save original sideways bank
	sta swrBankOriginal

	lda #0					; get bbc model
	ldx #1
	jsr osByte

	stx machineType				; save machine type

	cpx #2					; if it reports back as a b+ model
	bne loaderNotBplus

	ldy #128				; then test that there really is ram at bank 128 (real b+)
	jsr swrTestByte - loaderReloc
	beq loaderRealBplus			; if ram found in bank 128 then its a real B+
	
	dec machineType				; else its a model b with integra
	bne loaderNotBplus

.loaderRealBplus
						; we got this far so its a real b+, display b+ workspace message
	ldy #lo(loaderUsingWorkspace - loaderReloc)
	jsr loaderPrint - loaderReloc

	jmp loaderCopyData - loaderReloc	; copy data and run ladybug

	;---------------------------------------------------------------------------------------------------------------------------------------------

.loaderNotBplus

	jsr swrTest - loaderReloc		; test for sideways ram banks
	bne loaderFailed			; if none found then jump to failed message

	ldy #lo(loaderUsingBank - loaderReloc)	; else display sideways bank message
	jsr loaderPrint - loaderReloc

	ldy swrBank				; display bank number
	lda loaderBank - loaderReloc, y
	jsr osWrch

	;---------------------------------------------------------------------------------------------------------------------------------------------

.loaderCopyData

	jsr swramCopy - loaderReloc		; copy data to swram

	asl machineType				; convert machine type into index

	ldx #lo(runLadybug - loaderReloc)	; run the main ladybug
	ldy #hi(runLadybug - loaderReloc)
	jmp osCli

	;---------------------------------------------------------------------------------------------------------------------------------------------

.loaderFailed

	lda swrBankOriginal			; restore original sideways bank
	sta bankSelectCopy
	sta bankSelect

	cli					; enable interrupts

	ldy #lo(loaderRamFailed - loaderReloc)	; display failure message
	
	;---------------------------------------------------------------------------------------------------------------------------------------------

.loaderPrint	

	lda loaderMessages - loaderReloc, y	; get byte from messages, y
	cmp #&ff				; if its the end marker (&ff) then exit
	beq loaderPrintExit

	jsr osWrch				; else print it
	
	iny					; and get another
	bne loaderPrint

.loaderPrintExit	

	rts					; done.



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; check system for sideways ram
;-----------------------------------------------------------------------------------------------------------------------------------------------------

swrTestLocation		= page8000 + 8		; memory location for write test

machineType		= page8000 - 3		; storage for machine type
swrBank			= page8000 - 2		; storage for bank number of swram
swrBankOriginal		= page8000 - 1		; storage for original system bank number


;-----------------------------------------------------------------------------------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swrTestByte

	sty swrBank				; store bank number

	sty bankSelectCopy			; select bank
	sty bankSelect
	
	lda swrTestLocation			; invert byte at test location
	eor #&ff
	sta swrTestLocation
	
	ldx #0					; delay loop

.swrTestByteDelay

	dex
	bne swrTestByteDelay
	
	cmp swrTestLocation			; check if ram is present (value matches)

	rts					; exit with equal / not equal result in Z flag



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; swramCopy					copy data from main ram to sideways ram
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.swramCopy

	ldx #hi(swramLastAddr - swramStart)	; number of pages to copy

	ldy #0					; transfer index

.swramTransfer

	lda loaderPage, y			; read byte from loader data
	sta swramStart, y			; write byte to sideways ram

	iny					; repeat until page transferred
	bne swramTransfer
	
	inc swramTransfer + 2 - loaderReloc	; move to next page
	inc swramTransfer + 5 - loaderReloc

	dex					; repeat until all pages transferred
	bne swramTransfer

	rts					; done.



;-----------------------------------------------------------------------------------------------------------------------------------------------------
.loaderEnd
;-----------------------------------------------------------------------------------------------------------------------------------------------------

assert P% <= swramEnd				; sideways ram limit exceeded, check ladybug.lst

loaderPage = page3000				; load address
loaderReloc = swramStart - loaderPage
loaderStartReloc = loaderStart - loaderReloc

	print
	print
	print
