master reset after ?&fe4e=127

.e364
	lda #&40		; disable nmi
	sta &d00
	sei			; disable irq
	lda #&53		; unknown
	sta &fe8e
	jsr &e590		; page in bank &0f
.e372
	jmp &8020		; run code in bank &0f
	
.e590
	lda #&0f
	sta &f4
	sta &fe39
	rts
	

.8020
	lda #&fe
	trb &fe34
	stz &dfdd
	trb &0366
	cld
	ldx #&ff
	txs
	stx &fe63
	lda #&cf
	sta &fe42
	ldy #&20
	ldx #&0a
	jsr &98e4
.803e
	jsr 9729
.8041
	lda #&0c
	tsb &fe34
	lda &fe4e
	asl a
	pha
	beq &8054
.8054
	tay
.8055
	tya
	stz &01
	stz &00
.805a
	sta (&00), y
	iny
	bne &805a
	inc &01
	lda #&40
	sta &0d00
	ldx &01
	cpx #&e0
	bne &805a
.806c
	lda#&04
	trb &fe34
	bne &8055
.8073
	lda #&11
	sta &df04
	lda #&e8
	
	
	
	
	
.9729
	ldx #&0b
	ldy #&02
	jmp &98e4
	
	
	
	
	
.98e4
	php
	sei
	jsr 9906
.98e9
	lda #&41
	sta &fe40
	lda #&ff
	sta &fe43
	lda #&4a
	sta &fe40
	sty &fe4f
	bra &98cc
	
.98cc
	lda #&42
	sta &fe40
	lda #&02
	sta &fe40
	stz &fe43
	plp
	tya
	rts
	







.9906
	lda #&02
	sta &fe40
	lda #&82
	sta &fe40
	lda #&ff
	sta &fe43
	stx &fe4f
	lda #&c2
	sta &fe40
	lda #&42
	sta &fe40
	rts

	
	
	
	
