;-----------------------------------------------------------------------------------------------------------------------------------------------------
; show memory usage
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print "screen ram    ", ~screenAddr, "- ", ~pageHigh - 1, ", ", pageHigh - screenAddr, "bytes"
	print "----------------------------------------------------"
	print

	print "----------------------------------------------------"
	print "main ram      ", ~pageZero, "- ", ~screenAddr - 1, ", ", screenAddr - pageZero, "bytes"
	print "----------------------------------------------------"
	print "used          ", ~pageZero, "- ", ~programEnd - 1, ", ", programEnd - pageZero, "bytes"
	print "free          ", ~programEnd, "- ", ~screenAddr - 1, ", ", screenAddr - programEnd, "bytes"
	print "unused        ", pageBreak - pagefx200End + pagefx200 - page0200End + page0100 - pageZeroEnd, "bytes"
	print "----------------------------------------------------"
	print

	print "----------------------------------------------------"
	print "high ram      ", ~swramStart, "- ", ~swramEnd - 1, ", ", swramEnd - swramStart, "bytes"
	print "----------------------------------------------------"
	print "used          ", ~swramStart, "- ", ~swramLastAddr - 1, ", ", swramLastAddr - swramStart, " bytes"
	print "free          ", ~swramLastAddr, "- ", ~swramEnd - 1, ", ", swramEnd - swramLastAddr, "bytes"
	print "----------------------------------------------------"
	print

	print "----------------------------------------------------"
	print "main ram details"
	print "----------------------------------------------------"
	print "variables     ", ~pageZero, "- ", ~pageZeroEnd - 1, ", ", pageZeroEnd - pageZero, "bytes"
	print "unused        ", ~pageZeroEnd, "- ", ~page0100 - 1, ", ", page0100 - pageZeroEnd, "bytes"
	print "----------------------------------------------------"
	print "used          ", ~page0100, "- ", ~page0100End - 1, ", ", page0100End - page0100, "bytes"
	print "stack         ", ~page0100End, "- ", ~page0200 - 1, ", ", page0200 - page0100End, "bytes"
	print "----------------------------------------------------"
	print "used          ", ~page0200, "- ", ~page0200End - 1, ", ", page0200End - page0200, "bytes" 
	print "unused        ", ~page0200End, "- ", ~pagefx200 - 1, ", ", pagefx200 - page0200End, "bytes"
	print "----------------------------------------------------"
	print "used          ", ~pagefx200, "- ", ~pagefx200End - 1, ", ", pagefx200End - pagefx200, "bytes" 
	print "unused        ", ~pagefx200End, "- ", ~pageBreak - 1, ", ", pageBreak - pagefx200End, "bytes"
	print "----------------------------------------------------"
	print "used          ", ~pageBreak, "- ", ~programEnd - 1, ", ", programEnd - pageBreak, "bytes"
	print "free          ", ~programEnd, "- ", ~screenAddr - 1, ", ", screenAddr - programEnd, "bytes"
	print "----------------------------------------------------"
	print
	print "----------------------------------------------------"
	print "destroyed on run"
	print "----------------------------------------------------"
	print "bootstrap     ", ~bootstrap, "- ", ~bootstrapEnd - 1, ", ", bootstrapEnd - bootstrap, "bytes"
	print "----------------------------------------------------"

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print
