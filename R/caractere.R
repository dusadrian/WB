caractere <- function(x) {
    
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
    imic <- paste(c(imic, "\xee", "<U\\+00EE>"), collapse = "|")
    
    imare <- rawToChar(as.raw(packBits(as.integer(c(1,1,0,0,0,0,1,1,0,1,1,1,0,0,0,1)))))
    imare <- paste(c(imare, "\xce", "<U\\+00CE>"), collapse = "|")
    
    
    amic <- c("\xe2", "<U\\+00E2>",
    rawToChar(as.raw(packBits(as.integer(c(0,0,1,0,0,0,1,1,1,1,0,0,0,0,0,1))))),
    rawToChar(as.raw(packBits(as.integer(c(1,1,0,0,0,0,1,1,0,1,0,0,0,1,0,1))))))
    amic <- paste(amic, collapse = "|")
    
    amare <- c("\xc2", "<U\\+00C2>",
    rawToChar(as.raw(packBits(as.integer(c(0,0,1,0,0,0,1,1,0,1,0,0,0,0,0,1))))),
    rawToChar(as.raw(packBits(as.integer(c(1,1,0,0,0,0,1,1,0,1,0,0,0,0,0,1))))))
    amare <- paste(amare, collapse = "|")
    
    # U+015F este s turcesc
    smic <- paste(c("\x219", "<U\\+0219>", "\x15f", "<U\\+015F>",
    rawToChar(as.raw(packBits(as.integer(c(0,0,0,1,0,0,1,1,1,0,0,1,1,0,0,1))))),
    rawToChar(as.raw(packBits(as.integer(c(1,0,1,0,0,0,1,1,1,1,1,1,1,0,0,1)))))),
    collapse = "|")
    
    # U+015E este S turcesc
    smare <- paste(c("\x218", "<U\\+0218>", "\x15e", "<U\\+015E>",
    rawToChar(as.raw(packBits(as.integer(c(0,0,0,1,0,0,1,1,0,0,0,1,1,0,0,1))))),
    rawToChar(as.raw(packBits(as.integer(c(1,0,1,0,0,0,1,1,0,1,1,1,1,0,0,1)))))),
    collapse = "|")
    
    
    tmic <- paste(c("\x21b", "<U\\+021B>",
    rawToChar(as.raw(packBits(as.integer(c(0,0,0,1,0,0,1,1,1,1,0,1,1,0,0,1))))),
    rawToChar(as.raw(packBits(as.integer(c(1,0,1,0,0,0,1,1,1,1,0,0,0,1,0,1)))))),
    collapse = "|")
    
    tmare <- paste(c("\x21a", "<U\\+021A>",
    rawToChar(as.raw(packBits(as.integer(c(0,0,0,1,0,0,1,1,0,1,0,1,1,0,0,1))))),
    rawToChar(as.raw(packBits(as.integer(c(1,0,1,0,0,0,1,1,0,1,0,0,0,1,0,1)))))),
    collapse = "|")
    
    x <- gsub("\t", " ", x) # tab-uri
    x <- gsub(paste(c(minus, "\x96"), collapse = "|"), "-", x)
    x <- gsub(ghilimele, "\"", x)
    x <- gsub(paste(c(dots, "\xfc\xbe\x8d\x93\xa0\xbc"), collapse = "|"), "...", x)
    x <- gsub(imic, "i", x)
    x <- gsub(imare, "I", x)
    x <- gsub(amic, "a", x)
    x <- gsub(amare, "A", x)
    x <- gsub(smic, "s", x)
    x <- gsub(smare, "S", x)
    x <- gsub(tmic, "t", x)
    x <- gsub(tmare, "T", x)
    x <- gsub(apostrof, "'", x)
    
    return(enc2utf8(x))
    
}
