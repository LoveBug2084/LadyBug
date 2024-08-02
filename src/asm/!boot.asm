;-----------------------------------------------------------------------------------------------------------------------------------------------------
; !Boot
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " !boot.asm"
	print "----------------------------------------------------"
	print

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org canvasBoot				; temporary canvas address for !Boot data

.bootasmStart

	equb "*BASIC",13			; select basic language

	equb 6					; enable vdu

	equb 22,7				; select mode 7
	equb 23,1,0,0,0,0,0,0,0,0		; cursor off
	equb 23,0,1,0,0,0,0,0,0,0		; disable display

	equs "*FX 200 1",13			; disable ESC
	equs "*FX 4 1",13			; disable cursor editing

	equs "*FX 114 1", 13			; disable shadow ram (generates an error on model B but screen is disabled so it does'nt show)
	equs "HIMEM=&7C00", 13			; set himem to non-shadow mode 7 address

	equb 12					; clear screen
	equb 22,7				; select non-shadow mode 7
	equb 23,1,0,0,0,0,0,0,0,0		; cursor off
	equb 23,0,1,0,0,0,0,0,0,0		; disable display

	equs "CHAIN",quote,"CheckFS",quote,13	; CHAIN"CheckFS"



;-----------------------------------------------------------------------------------------------------------------------------------------------------
.bootasmEnd
	skip 0					; show this address in listing
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print
