;-----------------------------------------------------------------------------------------------------------------------------------------------------
; clear screen function for editor.bas
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " cls.asm"
	print "----------------------------------------------------"
	print

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; assemble at termporary canvas address
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org canvasCls


;-----------------------------------------------------------------------------------------------------------------------------------------------------
; clear mode 1 memory area &3000-&7fff then change to graphics mode
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.cls

	ldy #0
	tya
	
.clsLoop					; fill all screen pages with 0

	for s, screenAddrMode1, screenAddrMode1 + screenSizeMode1 - 1, &100
	sta s, y
	next

	iny
	beq graphicsMode
	jmp clsLoop - canvasCls + pageCls

.graphicsMode

	ldy #0					; index to oswrch data to setup graphics mode

.graphicsModeLoop

						; output graphics mode data to oswrch
	lda graphicsModeData - canvasCls + pageCls, y
	jsr oswrch

	iny					; until done
	cpy #graphicsModeDataEnd - graphicsModeData
	bne graphicsModeLoop

	rts					; return

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.graphicsModeData

	equb 22,1				; mode 1

	equb 23,1,0,0,0,0,0,0,0,0		; cursor off

	equb 19,1,4,0,0,0			; color 1 blue

	equb 19,2,5,0,0,0			; color 2 magenta

	equb 19,3,2,0,0,0			; color 3 green

.graphicsModeDataEnd
	skip 0					; show this address in listing

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.clsEnd
	skip 0					; show this address in listing

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print
