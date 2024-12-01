;-----------------------------------------------------------------------------------------------------------------------------------------------------
; soundtables - sound effects and music
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print " soundtables.asm"
	print "----------------------------------------------------"
	print

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; sound effect type	psg channels	task needed before playing sound effect type
;-----------------------------------------------------------------------------------------------------------------------------------------------------

; 0 music		0 1(2)-		shut down effects 1 2 3 4 and silence psg channels 0 1 2 before playing music
; 1 timer		0 - - -		allowed if effect 0 and 3 are not active
; 2 enemy warning	- - 2 -		allowed if effect 0 is not active
; 3 object		0 1 - -		allowed if effect 0 is not active, shut down effects 1 and 4 before playing sound
; 3 skull		0 1 - -		allowed if effect 0 is not active, shut down effects 1 and 4 before playing sound
; 4 turnstile		- 1 - -		allowed if effect 0 and 3 are not active
; 5 munch		- - - 3		allowed, no conflict with other effects or psg channels

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; game sound effects and music tables
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	sfxTwinkle		= 0
	sfxEndLevel		= 1
	sfxTimerLow		= 2
	sfxTimerMedium		= 3
	sfxTimerHigh		= 4
	sfxTurnstile		= 5
	sfxMunch		= 6
	sfxEnemyWarning		= 7
	sfxObject		= 8
	sfxSkull		= 9
	sfxMusicEntry		= 10
	sfxMusicDeath		= 11
	sfxMusicVegetable	= 12
	sfxMusicLetters		= 13
	sfxMusicExtra		= 14
	sfxMusicSpecial		= 15

	;---------------------------------------------------------------------------------------------------------------------------------------------

.sfxAddrTable

	equw sfxDataTwinkle
	equw sfxDataEndLevel
	equw sfxDataTimerLow
	equw sfxDataTimerMedium
	equw sfxDataTimerHigh
	equw sfxDataTurnstile
	equw sfxDataMunch
	equw sfxDataEnemyWarning
	equw sfxDataObject
	equw sfxDataSkull
	equw sfxDataMusicEntry
	equw sfxDataMusicDeath
	equw sfxDataMusicVegetable
	equw sfxDataMusicLetters
	equw sfxDataMusicExtra
	equw sfxDataMusicSpecial



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; Twinkle
	;---------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataTwinkle

	; sfx type 0, psg channel 0
	
	equb 0

	equb &8b, &06, &90
	equb &48

	equb &8f, &05
	equb &44

	equb &85, &05
	equb &44

	equb &80, &05
	equb &42

	equb &94
	equb &44

	equb &9f
	equb &40



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; EndLevel
	;---------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataEndLevel

	; sfx type 0, psg channels 0,1

	equb 0

	equb &86, &0d, &91
	equb &ab, &06, &b1
	equb &43

	equb &8e, &0b
	equb &a1, &07
	equb &43

	equb &8a, &0a
	equb &af, &07
	equb &43

	equb &80, &0a
	equb &af, &08
	equb &43

	equb &8f, &08
	equb &a0, &0a
	equb &43

	equb &8f, &07
	equb &aa, &0a
	equb &43

	equb &81, &07
	equb &ae, &0b
	equb &43

	equb &8b, &06
	equb &a6, &0d
	equb &43

	equb &9f, &bf
	equb &40
	


	;---------------------------------------------------------------------------------------------------------------------------------------------
	; TimerLow
	;---------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataTimerLow

	; sfx type 1, psg channel 0

	equb 1

	equb &86, &35, &9a
	equb &42

	equb &8a, &23
	equb &42

	equb &9f
	equb &40

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; TimerMedium
	;---------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataTimerMedium

	; sfx type 1, psg channel 0

	equb 1

	equb &86, &35, &95
	equb &42

	equb &8a, &23
	equb &42

	equb &9f
	equb &40

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; TimerHigh
	;---------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataTimerHigh

	; sfx type 1, psg channel 0

	equb 1

	equb &86, &35, &90
	equb &42

	equb &8a, &23
	equb &42

	equb &9f
	equb &40



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; Turnstile
	;---------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataTurnstile

	; sfx type 4, psg channel 1

	equb 4

	equb &aa, &0c, &b1
	equb &43

	equb &a4, &0b
	equb &43

	equb &a7, &09
	equb &43

	equb &a7, &08
	equb &43

	equb &a8, &07
	equb &43

	equb &a5, &06
	equb &43

	equb &bf
	equb &40



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; Munch
	;---------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataMunch

	; sfx type 5, psg channel 3

	equb 5

	equb &e6, &f2
	equb &44

	equb &e4
	equb &42

	equb &ff
	equb &40
	


	;---------------------------------------------------------------------------------------------------------------------------------------------
	; EnemyWarning
	;---------------------------------------------------------------------------------------------------------------------------------------------
	; ** special thanks to HeadHunter for working out the math and regenerating the enemy warning sound for 50Hz **
	;---------------------------------------------------------------------------------------------------------------------------------------------


.sfxDataEnemyWarning							

	; sfx type 2, psg channel 2
	
	equb 2
	
	equb &c6, &35, &d0
	equb &41

	equb &cd, &31
	equb &41

	equb &c7, &2e
	equb &41

	equb &c5, &2b
	equb &41

	equb &c7, &28
	equb &41

	equb &cb, &25
	equb &41

	equb &c3, &23
	equb &41

	equb &cd, &20
	equb &41

	equb &c9, &1e
	equb &41

	equb &c9, &1c
	equb &41

	equb &cb, &25
	equb &41

	equb &c3, &23
	equb &41

	equb &cd, &20
	equb &41

	equb &ca, &1e
	equb &41

	equb &c9, &1c
	equb &41

	equb &ca, &1a
	equb &41

	equb &cd, &18
	equb &41

	equb &c3, &17
	equb &41

	equb &ca, &15
	equb &41

	equb &c2, &14
	equb &41

	equb &cb, &1a
	equb &41

	equb &ce, &18
	equb &41

	equb &c3, &17
	equb &41

	equb &ca, &15
	equb &41

	equb &c3, &14
	equb &41

	equb &cd, &12
	equb &41

	equb &c9, &11
	equb &41

	equb &c6, &10
	equb &41

	equb &c4, &0f
	equb &41

	equb &c4, &0e
	equb &41

	equb &cd, &12
	equb &41

	equb &c9, &11
	equb &41

	equb &c6, &10
	equb &41

	equb &c5, &0f
	equb &41

	equb &c4, &0e
	equb &41

	equb &c5, &0d
	equb &41

	equb &c6, &0c
	equb &41

	equb &c9, &0b
	equb &41

	equb &cd, &0a
	equb &41

	equb &c1, &0a
	equb &41

	equb &c5, &0d
	equb &41

	equb &c7, &0c
	equb &41

	equb &ca, &0b
	equb &41

	equb &cd, &0a
	equb &41

	equb &c2, &0a
	equb &41

	equb &c7, &09
	equb &41

	equb &cc, &08
	equb &41

	equb &c3, &08
	equb &41

	equb &ca, &07
	equb &41

	equb &c2, &07
	equb &41

	equb &c7, &09
	equb &41

	equb &cd, &08
	equb &41

	equb &c3, &08
	equb &41

	equb &ca, &07
	equb &41

	equb &c2, &07
	equb &41

	equb &ca, &06
	equb &41

	equb &c3, &06
	equb &41

	equb &cc, &05
	equb &41

	equb &c6, &05
	equb &41

	equb &c0, &05
	equb &41

	equb &ca, &06
	equb &41

	equb &c3, &06
	equb &41

	equb &cc, &05
	equb &41

	equb &c6, &05
	equb &41

	equb &c0, &05
	equb &41

	equb &cb, &04
	equb &41

	equb &c6, &04
	equb &41

	equb &c1, &04
	equb &41

	equb &cd, &03
	equb &41

	equb &c9, &03
	equb &41

	equb &df
	equb &40



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; Object
	;---------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataObject

	; sfx type 3, psg channels 0,1

	equb 3

	equb &88, &3f, &92
	equb &a6, &35, &b2
	equb &41

	equb &86, &35
	equb &ae, &2c
	equb &41

	equb &8e, &2c
	equb &ac, &25
	equb &41

	equb &8c, &25
	equb &ac, &1f
	equb &41

	equb &8c, &1f
	equb &ab, &1a
	equb &41

	equb &8b, &1a
	equb &a7, &16
	equb &41

	equb &87, &16
	equb &ae, &12
	equb &41

	equb &8e, &12
	equb &ae, &0f
	equb &41

	equb &8e, &0f
	equb &a6, &0d
	equb &41

	equb &86, &0d
	equb &a4, &0b
	equb &41

	equb &84, &0b
	equb &a7, &09
	equb &41

	equb &87, &09
	equb &af, &07
	equb &41

	equb &8f, &07
	equb &ab, &06
	equb &41

	equb &8b, &06
	equb &aa, &05
	equb &41

	equb &8a, &05
	equb &ab, &04
	equb &41

	equb &9f, &bf
	equb &40



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; Skull
	;---------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataSkull

	; sfx type 3, psg channels 0,1

	equb 3

	equb &8d, &10, &90
	equb &a3, &15, &b0
	equb &41

	equb &8d, &11
	equb &a7, &16
	equb &41

	equb &8e, &12
	equb &ad, &17
	equb &41

	equb &80, &14
	equb &a3, &19
	equb &41

	equb &8d, &11
	equb &a7, &16
	equb &41

	equb &8e, &12
	equb &ad, &17
	equb &41

	equb &80, &14
	equb &a3, &19
	equb &41

	equb &83, &15
	equb &ab, &1a
	equb &41

	equb &8e, &12
	equb &ad, &17
	equb &41

	equb &80, &14
	equb &a3, &19
	equb &41

	equb &83, &15
	equb &ab, &1a
	equb &41

	equb &87, &16
	equb &a4, &1c
	equb &41

	equb &80, &14
	equb &a3, &19
	equb &41

	equb &83, &15
	equb &ab, &1a
	equb &41

	equb &87, &16
	equb &a4, &1c
	equb &41

	equb &8d, &17
	equb &af, &1d
	equb &41

	equb &83, &15
	equb &ab, &1a
	equb &41

	equb &87, &16
	equb &a4, &1c
	equb &41

	equb &8d, &17
	equb &af, &1d
	equb &41

	equb &83, &19
	equb &ac, &1f
	equb &41

	equb &87, &16
	equb &a4, &1c
	equb &41

	equb &8d, &17
	equb &af, &1d
	equb &41

	equb &83, &19
	equb &ac, &1f
	equb &41

	equb &8b, &1a
	equb &aa, &21
	equb &41

	equb &8d, &17
	equb &af, &1d
	equb &41

	equb &83, &19
	equb &ac, &1f
	equb &41

	equb &8b, &1a
	equb &aa, &21
	equb &41

	equb &84, &1c
	equb &aa, &23
	equb &41

	equb &83, &19
	equb &ac, &1f
	equb &41

	equb &8b, &1a
	equb &aa, &21
	equb &41

	equb &84, &1c
	equb &aa, &23
	equb &41

	equb &8f, &1d
	equb &ac, &25
	equb &41

	equb &8b, &1a
	equb &aa, &21
	equb &41

	equb &84, &1c
	equb &aa, &23
	equb &41

	equb &8f, &1d
	equb &ac, &25
	equb &41

	equb &8c, &1f
	equb &a0, &28
	equb &41

	equb &84, &1c
	equb &aa, &23
	equb &41

	equb &8f, &1d
	equb &ac, &25
	equb &41

	equb &8c, &1f
	equb &a0, &28
	equb &41

	equb &8a, &21
	equb &a6, &2a
	equb &41

	equb &9f, &bf
	equb &40



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; MusicEntry
	;---------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataMusicEntry

	; sfx type 0, psg channels 0,1,2
	
	equb 0

	equb &8e, &0f, &92
	equb &a0, &14, &b4
	equb &c0, &28, &d4
	equb &50

	equb &df
	equb &42

	equb &8d, &11
	equb &a3, &15
	equb &d4
	equb &44

	equb &df
	equb &42

	equb &8e, &0f
	equb &a0, &14
	equb &d4
	equb &52

	equb &8d, &11
	equb &a3, &15
	equb &46

	equb &8e, &0f
	equb &a0, &14
	equb &4c

	equb &82, &0e
	equb &ad, &11
	equb &44

	equb &df
	equb &42

	equb &86, &0d
	equb &ae, &0f
	equb &d4
	equb &44

	equb &df
	equb &42

	equb &d4
	equb &58

	equb &82, &0e, &91
	equb &ad, &11
	equb &ca, &23
	equb &4a

	equb &9f, &bf
	equb &42

	equb &91, &b4
	equb &44

	equb &df
	equb &42

	equb &8e, &0f
	equb &ae, &12
	equb &d4
	equb &44

	equb &df
	equb &42

	equb &82, &0e
	equb &ad, &11
	equb &d4
	equb &46

	equb &86, &0d
	equb &ae, &0f
	equb &4e

	equb &8e, &0b
	equb &a2, &0e
	equb &58

	equb &df
	equb &42

	equb &d4
	equb &44

	equb &df
	equb &42

	equb &d4
	equb &5a

	equb &9f, &bf, &df
	equb &40
	


	;---------------------------------------------------------------------------------------------------------------------------------------------
	; MusicDeath
	;---------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataMusicDeath

	; sfx type 0, psg channels 0,1

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; 1st part death music
	;---------------------------------------------------------------------------------------------------------------------------------------------

	equb 0

	equb &86, &0d, &90
	equb &ae, &0f, &b0
	equb &44

	equb &9f, &bf
	equb &48

	equb &8e, &0f, &90
	equb &a0, &14, &b0
	equb &44

	equb &9f, &bf
	equb &48

	equb &80, &14, &90
	equb &ad, &17, &b0
	equb &44

	equb &9f, &bf
	equb &48

	equb &8b, &1a, &90
	equb &ac, &1f, &b0
	equb &44

	equb &9f, &bf
	equb &48

	equb &8d, &17, &92
	equb &a4, &1c, &b2
	equb &4c

	equb &83, &15, &91
	equb &ab, &1a, &b1
	equb &4c

	equb &80, &14, &90
	equb &ad, &17, &b0
	equb &4c

	equb &83, &15
	equb &ab, &1a
	equb &44

	equb &8d, &17, &91
	equb &a4, &1c, &b1
	equb &44

	equb &8b, &1a, &92
	equb &ac, &1f, &b2
	equb &44

	equb &84, &1c, &93
	equb &aa, &23, &b3
	equb &44

	equb &8c, &1f, &94
	equb &a0, &28, &b4
	equb &44

	equb &8a, &23, &95
	equb &a6, &2a, &b5
	equb &44

	equb &80, &28, &90
	equb &aa, &2f, &b0
	equb &52

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; 2nd part floating angel music
	;---------------------------------------------------------------------------------------------------------------------------------------------

	equb &8f, &07, &94
	equb &ab, &06, &b4
	equb &48

	equb &88, &07, &92
	equb &a5, &06, &b2
	equb &48

	equb &8f, &07, &91
	equb &ab, &06, &b1
	equb &48

	equb &88, &07, &92
	equb &a5, &06, &b2
	equb &48

	equb &8f, &07, &94
	equb &ab, &06, &b4
	equb &48

	equb &88, &07, &98
	equb &a5, &06, &b8
	equb &48

	equb &8f, &07, &94
	equb &ab, &06, &b4
	equb &48

	equb &88, &07, &92
	equb &a5, &06, &b2
	equb &48

	equb &8f, &07, &91
	equb &ab, &06, &b1
	equb &48

	equb &88, &07, &92
	equb &a5, &06, &b2
	equb &48

	equb &8f, &07, &94
	equb &ab, &06, &b4
	equb &48

	equb &88, &07, &98
	equb &a5, &06, &b8
	equb &48

	equb &8f, &07, &94
	equb &ab, &06, &b4
	equb &48

	equb &88, &07, &92
	equb &a5, &06, &b2
	equb &48

	equb &8f, &07, &91
	equb &ab, &06, &b1
	equb &48

	equb &88, &07, &92
	equb &a5, &06, &b2
	equb &48

	equb &8f, &07, &94
	equb &ab, &06, &b4
	equb &48

	equb &88, &07, &98
	equb &a5, &06, &b8
	equb &48

	equb &9f, &bf
	equb &40



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; MusicVegetable
	;---------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataMusicVegetable

	; sfx type 0, psg channels 0,1

	equb 0

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; 1st part collect vegetable sound effect
	;---------------------------------------------------------------------------------------------------------------------------------------------

	equb &80, &0a, &91
	equb &a7, &09, &b1
	equb &43

	equb &8f, &08
	equb &a7, &08
	equb &43

	equb &8f, &07
	equb &a8, &07
	equb &43

	equb &8b, &06
	equb &a5, &06
	equb &43

	equb &8f, &05
	equb &aa, &05
	equb &43

	equb &8b, &06
	equb &a5, &06
	equb &43

	equb &8f, &07
	equb &a8, &07
	equb &43

	equb &8f, &08
	equb &a7, &08
	equb &43

	equb &80, &0a
	equb &a7, &09
	equb &43

	equb &9f, &bf
	equb &43

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; 2nd part vegetable music
	;---------------------------------------------------------------------------------------------------------------------------------------------

	equb &86, &0d, &90
	equb &aa, &23, &b0
	equb &48

	equb &9f, &bf
	equb &48

	equb &8d, &11, &91
	equb &ab, &1a, &b1
	equb &48

	equb &9f, &bf
	equb &48

	equb &86, &0d, &90
	equb &aa, &23, &b0
	equb &48

	equb &9f, &bf
	equb &48

	equb &8d, &11, &91
	equb &ab, &1a, &b1
	equb &48

	equb &9f, &bf
	equb &48

	equb &8a, &0a, &90
	equb &ad, &11, &b0
	equb &4c

	equb &8e, &0b, &91
	equb &a0, &14, &b1
	equb &44

	equb &86, &0d, &90
	equb &a3, &15, &b0
	equb &4c

	equb &8d, &11
	equb &ab, &1a
	equb &44

	equb &8e, &0f, &91
	equb &a0, &14, &b1
	equb &4c

	equb &82, &0e
	equb &ad, &17
	equb &44

	equb &86, &0d
	equb &a3, &15
	equb &4c

	equb &8e, &0b
	equb &a0, &14
	equb &44

	equb &8a, &0a, &90
	equb &ab, &1a, &b0
	equb &48

	equb &9f, &bf
	equb &48

	equb &87, &09, &90
	equb &ad, &17, &b0
	equb &48

	equb &9f, &bf
	equb &48

	equb &8f, &08, &90
	equb &ad, &11, &b0
	equb &48

	equb &9f, &bf
	equb &58

	equb &8f, &08, &90
	equb &ad, &11, &b0
	equb &44
	
	equb &8f, &07
	equb &a0, &14
	equb &44

	equb &9f, &bf
	equb &48

	equb &8f, &07, &90
	equb &a0, &14, &b0
	equb &44
	
	equb &81, &07
	equb &ad, &17
	equb &44

	equb &9f, &bf
	equb &48

	equb &81, &07, &90
	equb &ad, &17, &b0
	equb &44

	equb &8b, &06
	equb &ab, &1a
	equb &44
	
	equb &9f, &bf
	equb &48
	equb &40



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; MusicLetters
	;---------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataMusicLetters

	; sfx type 0, psg channel 0, 1

	equb 0
	
	equb &8b, &06, &91
	equb &ab, &1a, &b1
	equb &4E

	equb &86, &0d
	equb &ad, &11
	equb &47

	equb &8a, &0a
	equb &a6, &0d
	equb &47

	equb &8f, &08
	equb &aa, &0a
	equb &47

	equb &8b, &06
	equb &a3, &15
	equb &47

	equb &85, &05
	equb &af, &08
	equb &47

	equb &8b, &06
	equb &aa, &0a
	equb &47

	equb &9f, &bf
	equb &40



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; MusicExtra
	;---------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataMusicExtra

	; sfx type 0, psg channel 0, 1

	equb 0

	equb &8a, &0a, &92
	equb &a6, &35, &b4
	equb &54

	equb &bf
	equb &41

	equb &8e, &0b
	equb &b4
	equb &46

	equb &bf
	equb &41

	equb &8a, &0a
	equb &a6, &2a, &b4
	equb &55

	equb &8e, &0b
	equb &47

	equb &86, &0d
	equb &ac, &1f
	equb &46

	equb &9f
	equb &41

	equb &92
	equb &4d

	equb &bf
	equb &41

	equb &82, &0e
	equb &b4
	equb &46

	equb &bf
	equb &41

	equb &8e, &0f
	equb &b4
	equb &5d

	equb &9f
	equb &41

	equb &92
	equb &aa, &2f
	equb &4d

	equb &9f, &bf
	equb &41

	equb &92, &b4
	equb &4d

	equb &bf
	equb &41

	equb &86, &0d
	equb &b4
	equb &46

	equb &bf
	equb &41

	equb &8a, &0a
	equb &b4
	equb &4d

	equb &bf
	equb &41

	equb &8e, &0b
	equb &b4
	equb &47

	equb &aa, &23
	equb &4d

	equb &bf
	equb &41

	equb &b4
	equb &4d

	equb &bf
	equb &41

	equb &b4
	equb &4d

	equb &bf
	equb &41

	equb &b4
	equb &4d

	equb &bf
	equb &41

	equb &b4
	equb &46

	equb &bf
	equb &41

	equb &b4
	equb &4d
	
	equb &bf
	equb &41

	equb &b4
	equb &46

	equb &bf
	equb &41

	equb &b4
	equb &5c

	equb &9f, &bf
	equb &40



	;---------------------------------------------------------------------------------------------------------------------------------------------
	; MusicSpecial
	;---------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataMusicSpecial

	; sfx type 0, psg channel 0, 1

	equb 0
	
	equb &8e, &0f, &92
	equb &a0, &28, &b4
	equb &4e

	equb &86, &0d
	equb &46

	equb &9f, &bf
	equb &41

	equb &92, &b4
	equb &46

	equb &9f, &bf
	equb &41

	equb &92, &b4
	equb &46

	equb &9f, &41

	equb &92
	equb &47

	equb &8e, &0f
	equb &47

	equb &8d, &11
	equb &47

	equb &8e, &0f
	equb &a6, &35
	equb &47

	equb &8d, &11
	equb &47

	equb &83, &15
	equb &46

	equb &9f, &bf
	equb &41

	equb &92, &b4
	equb &46

	equb &9f, &bf
	equb &41

	equb &92, &b4
	equb &5c

	equb &8d, &17
	equb &aa, &2f
	equb &4d

	equb &9f
	equb &41

	equb &92
	equb &46

	equb &9f, &bf
	equb &41

	equb &92, &b4
	equb &46

	equb &9f, &bf
	equb &41

	equb &92, &b4
	equb &46

	equb &9f
	equb &41

	equb &92
	equb &4d

	equb &9f
	equb &41

	equb &92
	equb &47

	equb &8d, &11
	equb &aa, &23
	equb &46

	equb &9f
	equb &41

	equb &92
	equb &47

	equb &8e, &0f
	equb &46

	equb &bf
	equb &41

	equb &8d, &11
	equb &aa, &23
	equb &b4
	equb &46

	equb &9f, &bf
	equb &41

	equb &8d, &11, &92
	equb &aa, &23, &b4
	equb &5e

	equb &9f, &bf
	equb &40



;-----------------------------------------------------------------------------------------------------------------------------------------------------
; end of sfxData
;-----------------------------------------------------------------------------------------------------------------------------------------------------
.sfxDataEnd
	skip 0					; show this address in listing
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print
