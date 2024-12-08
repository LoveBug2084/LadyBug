;-----------------------------------------------------------------------------------------------------------------------------------------------------
; bonus settings for menu.bas
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " bonus.asm"
	print "----------------------------------------------------"
	print

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org canvasBonusSettings			; temporary canvas for creating _Bonus file used by menu.bas instruction pages to display
						; the information about the diamond/special/extra bonus screens
	
.bonusBin

	equb bonusExtraLives and 15
	
	equb bonusSpecialScore and 15
	equb bonusSpecialShield and 15
	
	equb bonusDiamondScore
	equb bonusDiamondLevel
	
.bonusBinEnd



;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print
