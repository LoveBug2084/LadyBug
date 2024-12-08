;-----------------------------------------------------------------------------------------------------------------------------------------------------
; Config
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " config.asm"
	print "----------------------------------------------------"
	print

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; default highscore table and settings
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	org pageConfig				

.config
	skip 0					; show this address in listing



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; high score table
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	equb &00,&45,&00			; 45000
	equs "UNIVERSAL",chrHeart,&FF

	equb &00,&40,&00			; 40000
	equs "ASTEROIDS ",&FF

	equb &00,&35,&00			; 35000
	equs "CENTIPEDE ",&FF

	equb &00,&30,&00			; 30000
	equs "GALAXIAN  ",&FF

	equb &00,&25,&00			; 25000
	equs "DEFENDER  ",&FF

	equb &00,&20,&00			; 20000
	equs "ROBOTRON  ",&FF

	equb &00,&15,&00			; 15000
	equs "SCRAMBLE  ",&FF

	equb &00,&10,&00			; 10000
	equs "GAUNTLET  ",&FF




;-----------------------------------------------------------------------------------------------------------------------------------------------------
; settings					default settings
;-----------------------------------------------------------------------------------------------------------------------------------------------------

defaultLadybugLives	= 3
defaultEnemySpeed	= 1
defaultEnemyAttack	= 4
defaultTimerVolume	= 1
defaultSoundEnable	= 2

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	equb defaultLadybugLives		; lady bugs (lives)	1-9
	equb defaultEnemySpeed			; enemy speed		0-5 (0=slower, 1=normal, 2=fast, 3=faster, 4=even faster, 5=extremely fast)
	equb defaultEnemyAttack			; enemy attack		0-9 (0=more random, 4=normal, 9=more aim)
	equb defaultTimerVolume			; timer volume		0-3 (0=off, 1=low, 2=medium, 3=high)
	equb defaultSoundEnable			; sound enable		0-2 (0=off, 1=on without demo, 2=on with demo)

	equb keyX				; right 'X'
	equb keyZ				; left 'Z'
	equb keySlash				; down '/'
	equb keyColon				; up ':'

	equs "XZ/:"				; ascii text for keys

	equb &00				; validation code, no need to calculate it here
						; it is calculated in game and by menu.bas, reset.bas



;-----------------------------------------------------------------------------------------------------------------------------------------------------
.configEnd	
	skip 0					; show this address in listing
;-----------------------------------------------------------------------------------------------------------------------------------------------------
	
	print
	print
	print
