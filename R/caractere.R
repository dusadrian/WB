caractere <- function(x, replace = c()) {
    
    toreplace <- list()
    toreplace$tab <- "\t"

    # integer raw vector, obtained with e.g.: as.integer(charToRaw("~"))
    minus <- rawToChar(as.raw(as.integer(c(226, 128, 147))))
    toreplace$minus <- paste(c(minus, "\x96"), collapse = "|")
    
    ghilimele <- c("\xfc\xbe\x8d\x83\xa0\xbc", "\xfc\xbe\x8d\x83\xa4\xbc", "\xfc\xbe\x8c\xb3\xa4\xbc",
    rawToChar(as.raw(packBits(as.integer(c(0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,0,1,1,1,0,0,1))))),
    rawToChar(as.raw(packBits(as.integer(c(0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,1,1,0,1,1,1,0,0,1))))),
    rawToChar(as.raw(packBits(as.integer(c(0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,1,1,1,1,0,0,1))))))
    toreplace$ghilimele <- paste(ghilimele, collapse = "|")
    
    dots <- rawToChar(as.raw(packBits(as.integer(c(0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,1,1,0,0,1,0,1)))))
    toreplace$dots <- paste(c(dots, "\xfc\xbe\x8d\x93\xa0\xbc"), collapse = "|")
    
    apostrof <- c("`", 
    rawToChar(as.raw(packBits(as.integer(c(0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,1,0,1,1,0,0,1))))),
    rawToChar(as.raw(packBits(as.integer(c(0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,1,1,0,0,1,1,0,0,1))))),
    rawToChar(as.raw(packBits(as.integer(c(0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1))))),
    "\x82", "\x92", "\x91")
    toreplace$apostrof <- paste(apostrof, collapse = "|")
    
    
    
    imic <- rawToChar(as.raw(packBits(as.integer(c(1,1,0,0,0,0,1,1,0,1,1,1,0,1,0,1)))))
    toreplace$imic <- paste(c(imic, "\xee", "<U\\+00EE>"), collapse = "|")
    
    imare <- rawToChar(as.raw(packBits(as.integer(c(1,1,0,0,0,0,1,1,0,1,1,1,0,0,0,1)))))
    toreplace$imare <- paste(c(imare, "\xce", "<U\\+00CE>"), collapse = "|")
    
    
    amic <- c("\xe2", "<U\\+00E2>",
    rawToChar(as.raw(packBits(as.integer(c(0,0,1,0,0,0,1,1,1,1,0,0,0,0,0,1))))),
    rawToChar(as.raw(packBits(as.integer(c(1,1,0,0,0,0,1,1,0,1,0,0,0,1,0,1))))))
    toreplace$amic <- paste(amic, collapse = "|")
    
    amare <- c("\xc2", "<U\\+00C2>",
    rawToChar(as.raw(packBits(as.integer(c(0,0,1,0,0,0,1,1,0,1,0,0,0,0,0,1))))),
    rawToChar(as.raw(packBits(as.integer(c(1,1,0,0,0,0,1,1,0,1,0,0,0,0,0,1))))))
    toreplace$amare <- paste(amare, collapse = "|")
    
    # U+015F este s turcesc
    toreplace$smic <- paste(c("\x219", "<U\\+0219>", "\x15f", "<U\\+015F>",
    rawToChar(as.raw(packBits(as.integer(c(0,0,0,1,0,0,1,1,1,0,0,1,1,0,0,1))))),
    rawToChar(as.raw(packBits(as.integer(c(1,0,1,0,0,0,1,1,1,1,1,1,1,0,0,1)))))),
    collapse = "|")
    
    # U+015E este S turcesc
    toreplace$smare <- paste(c("\x218", "<U\\+0218>", "\x15e", "<U\\+015E>",
    rawToChar(as.raw(packBits(as.integer(c(0,0,0,1,0,0,1,1,0,0,0,1,1,0,0,1))))),
    rawToChar(as.raw(packBits(as.integer(c(1,0,1,0,0,0,1,1,0,1,1,1,1,0,0,1)))))),
    collapse = "|")
    
    
    toreplace$tmic <- paste(c("\x21b", "<U\\+021B>",
    rawToChar(as.raw(packBits(as.integer(c(0,0,0,1,0,0,1,1,1,1,0,1,1,0,0,1))))),
    rawToChar(as.raw(packBits(as.integer(c(1,0,1,0,0,0,1,1,1,1,0,0,0,1,0,1)))))),
    collapse = "|")
    
    toreplace$tmare <- paste(c("\x21a", "<U\\+021A>",
    rawToChar(as.raw(packBits(as.integer(c(0,0,0,1,0,0,1,1,0,1,0,1,1,0,0,1))))),
    rawToChar(as.raw(packBits(as.integer(c(1,0,1,0,0,0,1,1,0,1,0,0,0,1,0,1)))))),
    collapse = "|")


    if (length(replace) == 0) {
        replace <- c(
            tab = " ",
            minus = "-",
            ghilimele = "\"",
            dots = "...",
            imic = "i",
            imare = "I",
            amic = "a",
            amare = "A",
            smic = "s",
            smare = "S",
            tmic = "t",
            tmare = "T",
            apostrof = "'"
        )
    }

    nms <- names(replace)
    for (i in seq(length(nms))) {
        x <- gsub(toreplace[[nms[i]]], replace[nms[i]], x)
    }
    
    return(enc2utf8(x))
    
}
