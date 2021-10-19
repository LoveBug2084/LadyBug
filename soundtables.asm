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
; sound channels
;-----------------------------------------------------------------------------------------------------------------------------------------------------

; channel		psg channel	special task before playing sound

; 0 music		0 1(2)-		shut down channels 1 2 3 4 and silence psg 0 1 2 before playing music
; 1 timer		0 - - -		allowed if channel 0 and 3 are not active
; 2 enemy warning	- - 2 -		allowed if channel 0 is not active
; 3 object		0 1 - -		allowed if channel 0 is not active, shut down channels 1 and 4 before playing sound
; 3 skull		0 1 - -		allowed if channel 0 is not active, shut down channels 1 and 4 before playing sound
; 4 turnstile		- 1 - -		allowed if channels 0 and 3 are not active
; 5 munch		- - - 3		allowed always, no conflict with other channels or psg channels

;-----------------------------------------------------------------------------------------------------------------------------------------------------
; game sound effects and music tables
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	sfxExtraLife		= 0
	sfxEndLevel		= 1
	sfxTimerLow		= 2
	sfxTimerMedium		= 3
	sfxTimerHigh		= 4
	sfxTurnstile		= 5
	sfxMunch		= 6
	sfxObject		= 7
	sfxEnemyWarning		= 8
	sfxSkull		= 9
	sfxMusicEntry		= 10
	sfxMusicDeath		= 11
	sfxMusicVegetable	= 12
	sfxMusicLetters		= 13
	sfxMusicExtra		= 14
	sfxMusicSpecial		= 15

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.sfxAddrTable

	equw sfxDataExtraLife
	equw sfxDataEndLevel
	equw sfxDataTimerLow
	equw sfxDataTimerMedium
	equw sfxDataTimerHigh
	equw sfxDataTurnstile
	equw sfxDataMunch
	equw sfxDataObject
	equw sfxDataEnemyWarning
	equw sfxDataSkull
	equw sfxDataMusicEntry
	equw sfxDataMusicDeath
	equw sfxDataMusicVegetable
	equw sfxDataMusicLetters
	equw sfxDataMusicExtra
	equw sfxDataMusicSpecial

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataExtraLife

	; sfx channel 0, psg channel 0
	
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

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataEndLevel

	; sfx channel 0, psg channels 0,1

	equb 0

	equb &86
	equb &0d
	equb &91
	equb &ab
	equb &06
	equb &b1
	equb &43

	equb &8e
	equb &0b
	equb &a1
	equb &07
	equb &43

	equb &8a
	equb &0a
	equb &af
	equb &07
	equb &43

	equb &80
	equb &0a
	equb &af
	equb &08
	equb &43

	equb &8f
	equb &08
	equb &a0
	equb &0a
	equb &43

	equb &8f
	equb &07
	equb &aa
	equb &0a
	equb &43

	equb &81
	equb &07
	equb &ae
	equb &0b
	equb &43

	equb &8b
	equb &06
	equb &a6
	equb &0d
	equb &43

	equb &9f
	equb &bf
	equb &40
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataTimerLow

	; sfx channel 1, psg channel 0

	equb 1

	equb &86, &35, &9a
	equb &42

	equb &8a, &23
	equb &42

	equb &9f
	equb &40

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataTimerMedium

	; sfx channel 1, psg channel 0

	equb 1

	equb &86, &35, &94
	equb &42

	equb &8a, &23
	equb &42

	equb &9f
	equb &40

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataTimerHigh

	; sfx channel 1, psg channel 0

	equb 1

	equb &86, &35, &90
	equb &42

	equb &8a, &23
	equb &42

	equb &9f
	equb &40

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataTurnstile

	; sfx channel 4, psg channel 1

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

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataMunch

	; sfx channel 5, psg channel 3

	equb 5

	equb &e6, &f0
	equb &44

	equb &e4
	equb &42

	equb &ff
	equb &40
	
; old modulated sound, no so good on a real beeb
; .sfxDataMunch

	; ; sfx channel 5, psg channel 3

	; equb 5

	; equb &e2, &f0
	; equb &41

	; equb &e6
	; equb &41

	; equb &e2
	; equb &41

	; equb &e6
	; equb &41

	; equb &e4
	; equb &42

	; equb &ff
	; equb &40
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataObject

	; sfx channel 3, psg channels 0,1

	equb 3

	equb &88
	equb &3f
	equb &92
	equb &a6
	equb &35
	equb &b2
	equb &41

	equb &86
	equb &35
	equb &ae
	equb &2c
	equb &41

	equb &8e
	equb &2c
	equb &ac
	equb &25
	equb &41

	equb &8c
	equb &25
	equb &ac
	equb &1f
	equb &41

	equb &8c
	equb &1f
	equb &ab
	equb &1a
	equb &41

	equb &8b
	equb &1a
	equb &a7
	equb &16
	equb &41

	equb &87
	equb &16
	equb &ae
	equb &12
	equb &41

	equb &8e
	equb &12
	equb &ae
	equb &0f
	equb &41

	equb &8e
	equb &0f
	equb &a6
	equb &0d
	equb &41

	equb &86
	equb &0d
	equb &a4
	equb &0b
	equb &41

	equb &84
	equb &0b
	equb &a7
	equb &09
	equb &41

	equb &87
	equb &09
	equb &af
	equb &07
	equb &41

	equb &8f
	equb &07
	equb &ab
	equb &06
	equb &41

	equb &8b
	equb &06
	equb &aa
	equb &05
	equb &41

	equb &8a
	equb &05
	equb &ab
	equb &04
	equb &41

	equb &9f
	equb &bf
	equb &40

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataEnemyWarning

	; sfx channel 2, psg channel 2
	
	equb 2
	
	equb &c6
	equb &35
	equb &d0
	equb &41

	equb &cd
	equb &31
	equb &41

	equb &c7
	equb &2e
	equb &41

	equb &c5
	equb &2b
	equb &41

	equb &c7
	equb &28
	equb &41

	equb &cb
	equb &25
	equb &41

	equb &c3
	equb &23
	equb &41

	equb &cd
	equb &20
	equb &41

	equb &c9
	equb &1e
	equb &41

	equb &c9
	equb &1c
	equb &41

	equb &cb
	equb &25
	equb &41

	equb &c3
	equb &23
	equb &41

	equb &cd
	equb &20
	equb &41

	equb &ca
	equb &1e
	equb &41

	equb &c9
	equb &1c
	equb &41

	equb &ca
	equb &1a
	equb &41

	equb &cd
	equb &18
	equb &41

	equb &c3
	equb &17
	equb &41

	equb &ca
	equb &15
	equb &41

	equb &c2
	equb &14
	equb &41

	equb &cb
	equb &1a
	equb &41

	equb &ce
	equb &18
	equb &41

	equb &c3
	equb &17
	equb &41

	equb &ca
	equb &15
	equb &41

	equb &c3
	equb &14
	equb &41

	equb &cd
	equb &12
	equb &41

	equb &c9
	equb &11
	equb &41

	equb &c6
	equb &10
	equb &41

	equb &c4
	equb &0f
	equb &41

	equb &c4
	equb &0e
	equb &41

	equb &cd
	equb &12
	equb &41

	equb &c9
	equb &11
	equb &41

	equb &c6
	equb &10
	equb &41

	equb &c5
	equb &0f
	equb &41

	equb &c4
	equb &0e
	equb &41

	equb &c5
	equb &0d
	equb &41

	equb &c6
	equb &0c
	equb &41

	equb &c9
	equb &0b
	equb &41

	equb &cd
	equb &0a
	equb &41

	equb &c1
	equb &0a
	equb &41

	equb &c5
	equb &0d
	equb &41

	equb &c7
	equb &0c
	equb &41

	equb &ca
	equb &0b
	equb &41

	equb &cd
	equb &0a
	equb &41

	equb &c2
	equb &0a
	equb &41

	equb &c7
	equb &09
	equb &41

	equb &cc
	equb &08
	equb &41

	equb &c3
	equb &08
	equb &41

	equb &ca
	equb &07
	equb &41

	equb &c2
	equb &07
	equb &41

	equb &c7
	equb &09
	equb &41

	equb &cd
	equb &08
	equb &41

	equb &c3
	equb &08
	equb &41

	equb &ca
	equb &07
	equb &41

	equb &c2
	equb &07
	equb &41

	equb &ca
	equb &06
	equb &41

	equb &c3
	equb &06
	equb &41

	equb &cc
	equb &05
	equb &41

	equb &c6
	equb &05
	equb &41

	equb &c0
	equb &05
	equb &41

	equb &ca
	equb &06
	equb &41

	equb &c3
	equb &06
	equb &41

	equb &cc
	equb &05
	equb &41

	equb &c6
	equb &05
	equb &41

	equb &c0
	equb &05
	equb &41

	equb &cb
	equb &04
	equb &41

	equb &c6
	equb &04
	equb &41

	equb &c1
	equb &04
	equb &41

	equb &cd
	equb &03
	equb &41

	equb &c9
	equb &03
	equb &41

	equb &df
	equb &40

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataSkull

	; sfx channel 3, psg channels 0,1

	equb 3

	equb &8d
	equb &10
	equb &90
	equb &a3
	equb &15
	equb &b0
	equb &41

	equb &8d
	equb &11
	equb &a7
	equb &16
	equb &41

	equb &8e
	equb &12
	equb &ad
	equb &17
	equb &41

	equb &80
	equb &14
	equb &a3
	equb &19
	equb &41

	equb &8d
	equb &11
	equb &a7
	equb &16
	equb &41

	equb &8e
	equb &12
	equb &ad
	equb &17
	equb &41

	equb &80
	equb &14
	equb &a3
	equb &19
	equb &41

	equb &83
	equb &15
	equb &ab
	equb &1a
	equb &41

	equb &8e
	equb &12
	equb &ad
	equb &17
	equb &41

	equb &80
	equb &14
	equb &a3
	equb &19
	equb &41

	equb &83
	equb &15
	equb &ab
	equb &1a
	equb &41

	equb &87
	equb &16
	equb &a4
	equb &1c
	equb &41

	equb &80
	equb &14
	equb &a3
	equb &19
	equb &41

	equb &83
	equb &15
	equb &ab
	equb &1a
	equb &41

	equb &87
	equb &16
	equb &a4
	equb &1c
	equb &41

	equb &8d
	equb &17
	equb &af
	equb &1d
	equb &41

	equb &83
	equb &15
	equb &ab
	equb &1a
	equb &41

	equb &87
	equb &16
	equb &a4
	equb &1c
	equb &41

	equb &8d
	equb &17
	equb &af
	equb &1d
	equb &41

	equb &83
	equb &19
	equb &ac
	equb &1f
	equb &41

	equb &87
	equb &16
	equb &a4
	equb &1c
	equb &41

	equb &8d
	equb &17
	equb &af
	equb &1d
	equb &41

	equb &83
	equb &19
	equb &ac
	equb &1f
	equb &41

	equb &8b
	equb &1a
	equb &aa
	equb &21
	equb &41

	equb &8d
	equb &17
	equb &af
	equb &1d
	equb &41

	equb &83
	equb &19
	equb &ac
	equb &1f
	equb &41

	equb &8b
	equb &1a
	equb &aa
	equb &21
	equb &41

	equb &84
	equb &1c
	equb &aa
	equb &23
	equb &41

	equb &83
	equb &19
	equb &ac
	equb &1f
	equb &41

	equb &8b
	equb &1a
	equb &aa
	equb &21
	equb &41

	equb &84
	equb &1c
	equb &aa
	equb &23
	equb &41

	equb &8f
	equb &1d
	equb &ac
	equb &25
	equb &41

	equb &8b
	equb &1a
	equb &aa
	equb &21
	equb &41

	equb &84
	equb &1c
	equb &aa
	equb &23
	equb &41

	equb &8f
	equb &1d
	equb &ac
	equb &25
	equb &41

	equb &8c
	equb &1f
	equb &a0
	equb &28
	equb &41

	equb &84
	equb &1c
	equb &aa
	equb &23
	equb &41

	equb &8f
	equb &1d
	equb &ac
	equb &25
	equb &41

	equb &8c
	equb &1f
	equb &a0
	equb &28
	equb &41

	equb &8a
	equb &21
	equb &a6
	equb &2a
	equb &41

	equb &9f
	equb &bf
	equb &40

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataMusicEntry

	; sfx channel 0, psg channels 0,1,2
	
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

	equb &9f
	equb &bf
	equb &42

	equb &91
	equb &b4
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

	equb &9f
	equb &bf
	equb &df
	equb &40
	
;-----------------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataMusicDeath

	; sfx channel 0, psg channels 0,1

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; 1st part death music
	;---------------------------------------------------------------------------------------------------------------------------------------------

	equb 0

	equb &86
	equb &0d
	equb &90
	equb &ae
	equb &0f
	equb &b0
	equb &44

	equb &9f
	equb &bf
	equb &48

	equb &8e
	equb &0f
	equb &90
	equb &a0
	equb &14
	equb &b0
	equb &44

	equb &9f
	equb &bf
	equb &48

	equb &80
	equb &14
	equb &90
	equb &ad
	equb &17
	equb &b0
	equb &44

	equb &9f
	equb &bf
	equb &48

	equb &8b
	equb &1a
	equb &90
	equb &ac
	equb &1f
	equb &b0
	equb &44

	equb &9f
	equb &bf
	equb &48

	equb &8d
	equb &17
	equb &92
	equb &a4
	equb &1c
	equb &b2
	equb &4c

	equb &83
	equb &15
	equb &91
	equb &ab
	equb &1a
	equb &b1
	equb &4c

	equb &80
	equb &14
	equb &90
	equb &ad
	equb &17
	equb &b0
	equb &4c

	equb &83
	equb &15
	equb &ab
	equb &1a
	equb &44

	equb &8d
	equb &17
	equb &91
	equb &a4
	equb &1c
	equb &b1
	equb &44

	equb &8b
	equb &1a
	equb &92
	equb &ac
	equb &1f
	equb &b2
	equb &44

	equb &84
	equb &1c
	equb &93
	equb &aa
	equb &23
	equb &b3
	equb &44

	equb &8c
	equb &1f
	equb &94
	equb &a0
	equb &28
	equb &b4
	equb &44

	equb &8a
	equb &23
	equb &95
	equb &a6
	equb &2a
	equb &b5
	equb &44

	equb &80
	equb &28
	equb &90
	equb &aa
	equb &2f
	equb &b0
	equb &52

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; 2nd part angel music
	;---------------------------------------------------------------------------------------------------------------------------------------------

	equb &8f
	equb &07
	equb &94
	equb &ab
	equb &06
	equb &b4
	equb &48

	equb &88
	equb &07
	equb &92
	equb &a5
	equb &06
	equb &b2
	equb &48

	equb &8f
	equb &07
	equb &91
	equb &ab
	equb &06
	equb &b1
	equb &48

	equb &88
	equb &07
	equb &92
	equb &a5
	equb &06
	equb &b2
	equb &48

	equb &8f
	equb &07
	equb &94
	equb &ab
	equb &06
	equb &b4
	equb &48

	equb &88
	equb &07
	equb &98
	equb &a5
	equb &06
	equb &b8
	equb &48

	equb &8f
	equb &07
	equb &94
	equb &ab
	equb &06
	equb &b4
	equb &48

	equb &88
	equb &07
	equb &92
	equb &a5
	equb &06
	equb &b2
	equb &48

	equb &8f
	equb &07
	equb &91
	equb &ab
	equb &06
	equb &b1
	equb &48

	equb &88
	equb &07
	equb &92
	equb &a5
	equb &06
	equb &b2
	equb &48

	equb &8f
	equb &07
	equb &94
	equb &ab
	equb &06
	equb &b4
	equb &48

	equb &88
	equb &07
	equb &98
	equb &a5
	equb &06
	equb &b8
	equb &48

	equb &8f
	equb &07
	equb &94
	equb &ab
	equb &06
	equb &b4
	equb &48

	equb &88
	equb &07
	equb &92
	equb &a5
	equb &06
	equb &b2
	equb &48

	equb &8f
	equb &07
	equb &91
	equb &ab
	equb &06
	equb &b1
	equb &48

	equb &88
	equb &07
	equb &92
	equb &a5
	equb &06
	equb &b2
	equb &48

	equb &8f
	equb &07
	equb &94
	equb &ab
	equb &06
	equb &b4
	equb &48

	equb &88
	equb &07
	equb &98
	equb &a5
	equb &06
	equb &b8
	equb &48

	equb &9f
	equb &bf
	equb &40

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataMusicVegetable

	; sfx channel 0, psg channels 0,1

	equb 0

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; 1st part collect vegetable sound effect
	;---------------------------------------------------------------------------------------------------------------------------------------------

	equb &80
	equb &0a
	equb &91
	equb &a7
	equb &09
	equb &b1
	equb &43

	equb &8f
	equb &08
	equb &a7
	equb &08
	equb &43

	equb &8f
	equb &07
	equb &a8
	equb &07
	equb &43

	equb &8b
	equb &06
	equb &a5
	equb &06
	equb &43

	equb &8f
	equb &05
	equb &aa
	equb &05
	equb &43

	equb &8b
	equb &06
	equb &a5
	equb &06
	equb &43

	equb &8f
	equb &07
	equb &a8
	equb &07
	equb &43

	equb &8f
	equb &08
	equb &a7
	equb &08
	equb &43

	equb &80
	equb &0a
	equb &a7
	equb &09
	equb &43

	equb &9f
	equb &bf
	equb &43

	;---------------------------------------------------------------------------------------------------------------------------------------------
	; 2nd part vegetable music
	;---------------------------------------------------------------------------------------------------------------------------------------------

	equb &86
	equb &0d
	equb &90
	equb &aa
	equb &23
	equb &b0
	equb &48

	equb &9f
	equb &bf
	equb &48

	equb &8d
	equb &11
	equb &91
	equb &ab
	equb &1a
	equb &b1
	equb &48

	equb &9f
	equb &bf
	equb &48

	equb &86
	equb &0d
	equb &90
	equb &aa
	equb &23
	equb &b0
	equb &48

	equb &9f
	equb &bf
	equb &48

	equb &8d
	equb &11
	equb &91
	equb &ab
	equb &1a
	equb &b1
	equb &48

	equb &9f
	equb &bf
	equb &48

	equb &8a
	equb &0a
	equb &90
	equb &ad
	equb &11
	equb &b0
	equb &4c

	equb &8e
	equb &0b
	equb &91
	equb &a0
	equb &14
	equb &b1
	equb &44

	equb &86
	equb &0d
	equb &90
	equb &a3
	equb &15
	equb &b0
	equb &4c

	equb &8d
	equb &11
	equb &ab
	equb &1a
	equb &44

	equb &8e
	equb &0f
	equb &91
	equb &a0
	equb &14
	equb &b1
	equb &4c

	equb &82
	equb &0e
	equb &ad
	equb &17
	equb &44

	equb &86
	equb &0d
	equb &a3
	equb &15
	equb &4c

	equb &8e
	equb &0b
	equb &a0
	equb &14
	equb &44

	equb &8a
	equb &0a
	equb &90
	equb &ab
	equb &1a
	equb &b0
	equb &48

	equb &9f
	equb &bf
	equb &48

	equb &87
	equb &09
	equb &90
	equb &ad
	equb &17
	equb &b0
	equb &48

	equb &9f
	equb &bf
	equb &48

	equb &8f
	equb &08
	equb &90
	equb &ad
	equb &11
	equb &b0
	equb &48

	equb &9f
	equb &bf
	equb &58

	equb &8f
	equb &08
	equb &90
	equb &ad
	equb &11
	equb &b0
	equb &44
	
	equb &8f
	equb &07
	equb &a0
	equb &14
	equb &44

	equb &9f
	equb &bf
	equb &48

	equb &8f
	equb &07
	equb &90
	equb &a0
	equb &14
	equb &b0
	equb &44
	
	equb &81
	equb &07
	equb &ad
	equb &17
	equb &44

	equb &9f
	equb &bf
	equb &48

	equb &81
	equb &07
	equb &90
	equb &ad
	equb &17
	equb &b0
	equb &44

	equb &8b
	equb &06
	equb &ab
	equb &1a
	equb &44
	
	equb &9f
	equb &bf
	equb &48
	equb &40

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataMusicLetters

	; sfx channel 0, psg channel 0, 1

	equb 0
	
	equb &8b
	equb &06
	equb &91
	equb &ab
	equb &1a
	equb &b1
	equb &4E

	equb &86
	equb &0d
	equb &ad
	equb &11
	equb &47

	equb &8a
	equb &0a
	equb &a6
	equb &0d
	equb &47

	equb &8f
	equb &08
	equb &aa
	equb &0a
	equb &47

	equb &8b
	equb &06
	equb &a3
	equb &15
	equb &47

	equb &85
	equb &05
	equb &af
	equb &08
	equb &47

	equb &8b
	equb &06
	equb &aa
	equb &0a
	equb &47

	equb &9f
	equb &bf
	equb &40

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataMusicExtra

	; sfx channel 0, psg channel 0, 1

	equb 0

	equb &8a
	equb &0a
	equb &92
	equb &a6
	equb &35
	equb &b4
	equb &54

	equb &bf
	equb &41

	equb &8e
	equb &0b
	equb &b4
	equb &46

	equb &bf
	equb &41

	equb &8a
	equb &0a
	equb &a6
	equb &2a
	equb &b4
	equb &55

	equb &8e
	equb &0b
	equb &47

	equb &86
	equb &0d
	equb &ac
	equb &1f
	equb &46

	equb &9f
	equb &41

	equb &92
	equb &4d

	equb &bf
	equb &41

	equb &82
	equb &0e
	equb &b4
	equb &46

	equb &bf
	equb &41

	equb &8e
	equb &0f
	equb &b4
	equb &5d

	equb &9f
	equb &41

	equb &92
	equb &aa
	equb &2f
	equb &4d

	equb &9f
	equb &bf
	equb &41

	equb &92
	equb &b4
	equb &4d

	equb &bf
	equb &41

	equb &86
	equb &0d
	equb &b4
	equb &46

	equb &bf
	equb &41

	equb &8a
	equb &0a
	equb &b4
	equb &4d

	equb &bf
	equb &41

	equb &8e
	equb &0b
	equb &b4
	equb &47

	equb &aa
	equb &23
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

	equb &9f
	equb &bf
	equb &40

;-----------------------------------------------------------------------------------------------------------------------------------------------------

.sfxDataMusicSpecial

	; sfx channel 0, psg channel 0, 1

	equb 0
	
	equb &8e
	equb &0f
	equb &92
	equb &a0
	equb &28
	equb &b4
	equb &4e

	equb &86
	equb &0d
	equb &46

	equb &9f
	equb &bf
	equb &41

	equb &92
	equb &b4
	equb &46

	equb &9f
	equb &bf
	equb &41

	equb &92
	equb &b4
	equb &46

	equb &9f
	equb &41

	equb &92
	equb &47

	equb &8e
	equb &0f
	equb &47

	equb &8d
	equb &11
	equb &47

	equb &8e
	equb &0f
	equb &a6
	equb &35
	equb &47

	equb &8d
	equb &11
	equb &47

	equb &83
	equb &15
	equb &46

	equb &9f
	equb &bf
	equb &41

	equb &92
	equb &b4
	equb &46

	equb &9f
	equb &bf
	equb &41

	equb &92
	equb &b4
	equb &5c

	equb &8d
	equb &17
	equb &aa
	equb &2f
	equb &4d

	equb &9f
	equb &41

	equb &92
	equb &46

	equb &9f
	equb &bf
	equb &41

	equb &92
	equb &b4
	equb &46

	equb &9f
	equb &bf
	equb &41

	equb &92
	equb &b4
	equb &46

	equb &9f
	equb &41

	equb &92
	equb &4d

	equb &9f
	equb &41

	equb &92
	equb &47

	equb &8d
	equb &11
	equb &aa
	equb &23
	equb &46

	equb &9f
	equb &41

	equb &92
	equb &47

	equb &8e
	equb &0f
	equb &46

	equb &bf
	equb &41

	equb &8d
	equb &11
	equb &aa
	equb &23
	equb &b4
	equb &46

	equb &9f
	equb &bf
	equb &41

	equb &8d
	equb &11
	equb &92
	equb &aa
	equb &23
	equb &b4
	equb &5e

	equb &9f
	equb &bf
	equb &40

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print
