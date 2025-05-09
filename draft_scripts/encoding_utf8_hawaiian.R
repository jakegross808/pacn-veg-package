# Encoding UTF-8 vs. CP1252
hawaiian <- "ʻŌlaʻa" # Typed this in using Hawaiian keyboard
Encoding(hawaiian) # Encoding() shows what encoding is currently associated with object.

iconv(x = hawaiian, from = "UTF-8", to = "UTF-8") # iconv() converts between encodings
# UTF-8 to UTF-8 just shows no problems when assuming object is UTF-8

#  However, if you assume a UTF-8 object is in CP1252 (Windows legacy) encoding, and try
# to convert from CP1252 to UTF-8, then issues will arise!
bad_encoding <- iconv(x = hawaiian, from = "CP1252", to = "UTF-8")
bad_encoding

does_not_work <- iconv(x = bad_encoding, from = "UTF-8", to = "Latin1")
does_not_work

