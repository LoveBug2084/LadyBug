;-----------------------------------------------------------------------------------------------------------------------------------------------------
; enemy speed table
;-----------------------------------------------------------------------------------------------------------------------------------------------------

				; enemies always move 1 step per frame plus this additional fraction

						; enemy speed option 0 (easy)
	equb 0.00 * 256				; 1.00 level 1-6
	equb 0.02 * 256				; 1.02 level 7-11
	equb 0.04 * 256				; 1.04 level 12-17
	equb 0.06 * 256				; 1.06 level 18-99
	
						; enemy speed option 1 (normal)
	equb 0.00 * 256				; 1.00 level 1-6
	equb 0.05 * 256				; 1.05 level 7-11
	equb 0.10 * 256				; 1.10 level 12-17
	equb 0.15 * 256				; 1.15 level 18-99
	
						; enemy speed option 2 (hard)
	equb 0.00 * 256				; 1.00 level 1-6
	equb 0.07 * 256				; 1.07 level 7-11
	equb 0.14 * 256				; 1.14 level 12-17
	equb 0.21 * 256				; 1.21 level 18-99
	
						; enemy speed option 3 (insane)
	equb 0.05 * 256				; 1.05 level 1-6
	equb 0.13 * 256				; 1.13 level 7-11
	equb 0.21 * 256				; 1.21 level 12-17
	equb 0.29 * 256				; 1.29 level 18-99

