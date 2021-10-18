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
; show memory usage
;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print "----------------------------------------------------"
	print "sideways ram  ", ~swramStart, "- ", ~swramEnd - 1, ", ", swramEnd - swramStart, "bytes"
	print "----------------------------------------------------"
	print "used          ", ~swramStart, "- ", ~loaderEnd - 1, ", ", loaderEnd - swramStart, " bytes"
	print "free          ", ~loaderEnd, "- ", ~swramEnd - 1, ", ", swramEnd - loaderEnd, "bytes"
	print "----------------------------------------------------"
	print
	print "----------------------------------------------------"
	print "screen ram    ", ~screenAddr, "- &7fff .", &8000 - screenAddr, "bytes"
	print "----------------------------------------------------"
	print
	print "----------------------------------------------------"
	print "main ram      ", ~page0000, "- ", ~screenAddr - 1, ", ", screenAddr - page0000, "bytes"
	print "----------------------------------------------------"
	print "used          ", ~page0000, "- ", ~programEnd - 1, ", ", programEnd - page0000, "bytes"
	print "unused        ", page0100 - page0000End + page0287 - page0258End + page0258 - page0200End + page0d00 - page0287End, "bytes"
	print "free          ", screenAddr - page0000 - (page0000End - page0000 + page0200End - page0100 + page0258End - page0258 + page0287End - page0287 + programEnd - page0d00), "bytes"
	print "----------------------------------------------------"
	print
	print "----------------------------------------------------"
	print "main ram details"
	print "----------------------------------------------------"
	print "variables     ", ~page0000, "- ", ~page0000End - 1, ", ", page0000End - page0000, "bytes"
	print "free          ", ~page0000End, "- ", ~page0100 - 1, ", ", page0100 - page0000End, "bytes"
	print "----------------------------------------------------"
	print "used          ", ~page0100, "- ", ~page0100End - 1, ", ", page0100End - page0100, "bytes"
	print "stack         ", ~page0100End, "- ", ~page0200 - 1, ", ", page0200 - page0100End, "bytes"
	print "----------------------------------------------------"
	print "used          ", ~page0200, "- ", ~page0200End - 1, ", ", page0200End - page0200, "bytes" 
	print "free          ", ~page0200End, "- ", ~page0258 - 1, ", ", page0258 - page0200End, "bytes"
	print "----------------------------------------------------"
	print "used          ", ~page0258, "- ", ~page0258End - 1, ", ", page0258End - page0258, "bytes" 
	print "free          ", ~page0258End, "- ", ~page0287 - 1, ", ", page0287 - page0258End, "bytes"
	print "----------------------------------------------------"
	print "used          ", ~page0287, "- ", ~page0287End - 1, ", ", page0287End - page0287, "bytes"
	print "free          ", ~page0287End, "- ", ~page0d00 - 1, ", ", page0d00 - page0287End, "bytes"
	print "----------------------------------------------------"
	print "used          ", ~page0d00, "- ", ~programEnd - 1, ", ", programEnd - page0d00, "bytes"
	print "free          ", ~programEnd, "- ", ~screenAddr - 1, ", ", screenAddr - programEnd, "bytes"
	print "----------------------------------------------------"
	print
	print "----------------------------------------------------"
	print "bootstrap     ", ~bootstrap, "- ", ~bootstrapEnd - 1, ", ", bootstrapEnd - bootstrap, "bytes"
	print "----------------------------------------------------"

;-----------------------------------------------------------------------------------------------------------------------------------------------------

	print
	print
	print
