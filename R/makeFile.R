`makeFile` <- function(
    instrument = NULL, type = NULL, partial = FALSE, destination = "~",
    host = "127.0.0.1", port = NULL, dbname = "", user = "", password = "") {

    if (is.null(type)) {
        cat("\n")
        stop(simpleError("Nu s-a specificat tipul de fisier care trebuie creat.\n\n"))
    }
    else {
        if (!is.element(toupper(type), c("SPSS", "STATA"))) {
            cat("\n")
            stop(simpleError("Tipul de fisier nu poate fi decat SPSS ori Stata.\n\n"))
        }
    }

    
    if (is.null(port)) {
        con <- DBI::dbConnect(
            RMariaDB::MariaDB(),
            host = as.character(host),
            dbname = dbname,
            user = user,
            password = password
        )
    }
    else {
        con <- DBI::dbConnect(
            RMariaDB::MariaDB(),
            host = as.character(host),
            port = as.character(port),
            dbname = dbname,
            user = user,
            password = password
        )
    }

    data <- DBI::dbGetQuery(con, 
        sprintf(
            "SELECT answer_id, name, value FROM answer_values
            LEFT JOIN answers ON answers.id = answer_values.answer_id
            WHERE %s instrument_id = %s",
            ifelse(partial, "", "partial = 0 AND"), instrument
        )
    )

    instruments <- DBI::dbGetQuery(con, "SELECT id, folder FROM instruments")

    DBI::dbDisconnect(con)

    if (nrow(data) == 0) {
        cat("\n")
        stop(simpleError("Nu sunt date pentru acest instrument.\n\n"))
    }
    
    dataDscr <- codebook[[as.character(instrument)]]$dataDscr

    if (is.null(dataDscr)) {
        cat("\n")
        stop(simpleError("Instrument inexistent.\n\n"))
    }

    dataDscr <- dataDscr[!(grepl("ldsss", names(dataDscr)) & grepl("file", names(dataDscr)))]

    answer_id <- sort(unique(data$answer_id))
    
    nms <- names(dataDscr)
    mdata <- matrix(NA, ncol = length(nms), nrow = length(answer_id))
    colnames(mdata) <- nms


    for (i in seq(length(answer_id))) {
        sdata <- data[data$answer_id == answer_id[i], ]
        columns <- match(sdata$name, nms)
        mdata[i, columns] <- sdata$value
    }

    mdata <- gsub("&amp;quot;", "", mdata)
    mdata <- gsub("&#039;", "", mdata)
    mdata <- gsub("&quot;", "", mdata)
    mdata <- gsub(",,-,,", "-", mdata)
    mdata[grepl("amp;amp;amp;", mdata)] <- "-"
    mdata[is.element(mdata, "#")] <- "-"
    mdata[is.element(mdata, -77)] <- -7
    mdata <- as.data.frame(mdata)

    if (toupper(type) == "SPSS") {
        mdata[] <- lapply(mdata, caractere)
    }

    for (i in seq(ncol(mdata))) {
        # print(nms[i])
        cb <- dataDscr[[nms[i]]]
        x <- mdata[, i]

        if (!is.null(cb$type) && cb$type == "string") {
            x <- as.character(x)
        }
        else if (admisc::possibleNumeric(x)) {

            x <- admisc::asNumeric(x)

            if (admisc::wholeNumeric(x)) {
                x <- as.integer(x)
            }

            if (toupper(type) == "STATA" && any(x < 0)) {
                x[x == -1] <- haven::tagged_na('a')
                x[x == -7] <- haven::tagged_na('b')
                x[x == -9] <- haven::tagged_na('c')
            }
        }

        labels <- cb[["labels"]]
        if (is.null(labels)) {
            labels <- cb[["values"]]
        }
        
        missing <- cb[["missing"]]
        if (is.null(missing)) {
            missing <- cb[["na_values"]]
        }

        if (is.character(x)) {

            if (!is.null(labels)) {
                lnms <- names(labels)
                labels <- as.character(labels)
            }

            if (!is.null(missing)) {
                missing <- as.character(missing)
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
            if (!is.null(label)) {
                label <- caractere(label)
            }
            

            if (!is.null(labels)) {
                names(labels) <- caractere(lnms)
            }

            x <- haven::labelled_spss(x, label = label, labels = labels, na_values = missing)
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

        mdata[, i] <- x
    }
    
    mdata[] <- lapply(mdata, function(x) {
        attrx <- attributes(x)
        if (is.factor(x)) {
            
            attrx$class <- setdiff(attrx$class, "factor")
            x <- as.character(x)
        }
        
        if (is.character(x)) {
            x <- strtrim(x, 244)
        }

        attributes(x) <- attrx

        return(x)
    })

    filename <- paste(instruments$folder[instruments$id == instrument],
        ifelse(toupper(type) == "SPSS", "sav", "dta") , sep = ".")

    if (toupper(type) == "SPSS") {
        haven::write_sav(mdata, path = file.path(destination, filename))
    }
    else if (toupper(type) == "STATA") {
        haven::write_dta(mdata, path = file.path(destination, filename))
    }
}
