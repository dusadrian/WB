
minus <- rawToChar(as.raw(as.integer(c(226, 128, 147))))

ghilimele <- c("\xfc\xbe\x8d\x83\xa0\xbc", "\xfc\xbe\x8d\x83\xa4\xbc", "\xfc\xbe\x8c\xb3\xa4\xbc",
rawToChar(as.raw(packBits(as.integer(c(0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,0,1,1,1,0,0,1))))),
rawToChar(as.raw(packBits(as.integer(c(0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,1,1,0,1,1,1,0,0,1))))),
rawToChar(as.raw(packBits(as.integer(c(0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,1,1,1,1,0,0,1))))))
ghilimele <- paste(ghilimele, collapse = "|")

dots <- rawToChar(as.raw(packBits(as.integer(c(0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,1,1,0,0,1,0,1)))))



apostrof <- c("`", 
rawToChar(as.raw(packBits(as.integer(c(0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,1,0,1,1,0,0,1))))),
rawToChar(as.raw(packBits(as.integer(c(0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,1,1,0,0,1,1,0,0,1))))),
rawToChar(as.raw(packBits(as.integer(c(0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1))))),
"\x82", "\x92", "\x91")
apostrof <- paste(apostrof, collapse = "|")



imic <- rawToChar(as.raw(packBits(as.integer(c(1,1,0,0,0,0,1,1,0,1,1,1,0,1,0,1)))))
imic <- paste(c(imic, "\xee"), collapse = "|")

imare <- rawToChar(as.raw(packBits(as.integer(c(1,1,0,0,0,0,1,1,0,1,1,1,0,0,0,1)))))
imare <- paste(c(imare, "\xce"), collapse = "|")

amic <- c("\xe2",
rawToChar(as.raw(packBits(as.integer(c(0,0,1,0,0,0,1,1,1,1,0,0,0,0,0,1))))),
rawToChar(as.raw(packBits(as.integer(c(1,1,0,0,0,0,1,1,0,1,0,0,0,1,0,1))))))
amic <- paste(amic, collapse = "|")

amare <- c("\xc2",
rawToChar(as.raw(packBits(as.integer(c(0,0,1,0,0,0,1,1,0,1,0,0,0,0,0,1))))),
rawToChar(as.raw(packBits(as.integer(c(1,1,0,0,0,0,1,1,0,1,0,0,0,0,0,1))))))
amare <- paste(amare, collapse = "|")

smic <- paste(c(
rawToChar(as.raw(packBits(as.integer(c(0,0,0,1,0,0,1,1,1,0,0,1,1,0,0,1))))),
rawToChar(as.raw(packBits(as.integer(c(1,0,1,0,0,0,1,1,1,1,1,1,1,0,0,1)))))),
collapse = "|")

smare <- paste(c(
rawToChar(as.raw(packBits(as.integer(c(0,0,0,1,0,0,1,1,0,0,0,1,1,0,0,1))))),
rawToChar(as.raw(packBits(as.integer(c(1,0,1,0,0,0,1,1,0,1,1,1,1,0,0,1)))))),
collapse = "|")

tmic <- paste(c(
rawToChar(as.raw(packBits(as.integer(c(0,0,0,1,0,0,1,1,1,1,0,1,1,0,0,1))))),
rawToChar(as.raw(packBits(as.integer(c(1,0,1,0,0,0,1,1,1,1,0,0,0,1,0,1)))))),
collapse = "|")

tmare <- paste(c(
rawToChar(as.raw(packBits(as.integer(c(0,0,0,1,0,0,1,1,0,1,0,1,1,0,0,1))))),
rawToChar(as.raw(packBits(as.integer(c(1,0,1,0,0,0,1,1,0,1,0,0,0,1,0,1)))))),
collapse = "|")
