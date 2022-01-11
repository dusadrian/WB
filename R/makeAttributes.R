`makeAttributes` <- function(x, cb, diacritice, type) {
    
    labels <- cb[["labels"]]
    if (is.null(labels)) {
        labels <- cb[["values"]]
    }

    if (any(is.element(x, -7)) && all(!is.element(labels, -7))) {
        labels <- c(labels, c("Nu se aplică" = -7))
    }
    
    na_values <- cb[["na_values"]]

    if (!is.null(cb$type) && is.element(cb$type, c("input", "textarea"))) {
        x <- as.character(x)
    }
    
    if (admisc::possibleNumeric(x)) {
        if (!is.numeric(x)) {
            x <- admisc::asNumeric(x)
        }

        if (!is.integer(x) && all(na.omit(x) < 2^31 - 1)) {
            if (admisc::wholeNumeric(x)) {
                x <- as.integer(x)
            }
        }

        notna <- which(!is.na(x))
        if (toupper(type) == "STATA" && any(x[notna] < 0)) {
            x[notna][x[notna] == -1] <- haven::tagged_na('a')
            x[notna][x[notna] == -7] <- haven::tagged_na('b')
            x[notna][x[notna] == -9] <- haven::tagged_na('c')
        }
    }

    if (is.character(x)) {

        if (!is.null(labels)) {
            lnms <- names(labels)
            labels <- as.character(labels)
        }

        if (!is.null(na_values)) {
            na_values <- as.character(na_values)
        }
    }
    else {
        if (!is.null(labels)) {
            lnms <- names(labels)
            labels <- as.integer(labels)
        }
    }
    
    if (toupper(type) == "SPSS") {
        label <- cb[["label"]]
        if (!is.null(label) & !diacritice) {
            label <- caractere(label)
        }
        

        if (!is.null(labels) & !diacritice) {
            names(labels) <- caractere(lnms)
        }

        x <- haven::labelled_spss(x, label = label, labels = labels, na_values = na_values)
    }
    else if (toupper(type) == "STATA") {
        
        if (is.numeric(x)) {
            if (!is.null(labels)) {
                labels[labels == -1] <- haven::tagged_na('a')
                labels[labels == -7] <- haven::tagged_na('b')
                labels[labels == -9] <- haven::tagged_na('c')
                names(labels) <- lnms

                x <- haven::labelled(x, label = cb[["label"]], labels = labels)
            }
        }
        
        attr(x, "label") <- cb[["label"]]
    }

    return(x)
}
