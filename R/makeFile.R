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

    if (nrow(data) == 0) {
        cat("\n")
        stop(simpleError("Nu sunt date pentru acest instrument.\n\n"))
    }

    instruments <- DBI::dbGetQuery(con, "SELECT id, folder FROM instruments")

    DBI::dbDisconnect(con)


    
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

    if (toupper(type) == "SPSS") {
        mdata <- caractere(mdata)
    }

    mdata[is.element(mdata, -77)] <- -7

    mdata <- as.data.frame(mdata)

    for (i in seq(ncol(mdata))) {
        # print(nms[i])
        cb <- dataDscr[[nms[i]]]
        x <- mdata[, i]

        if (admisc::possibleNumeric(x)) {
            x <- as.integer(admisc::asNumeric(x))

            if (toupper(type) == "STATA") {
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
            if (!is.null(labels)) {
                labels[labels == -1] <- haven::tagged_na('a')
                labels[labels == -7] <- haven::tagged_na('b')
                labels[labels == -9] <- haven::tagged_na('c')
                names(labels) <- lnms

                if (is.numeric(x)) {
                    x <- haven::labelled(x, label = cb[["label"]], labels = labels)
                }
            }
        }

        mdata[, i] <- x
    }

    filename <- paste(instruments$folder[instruments$id == instrument],
        ifelse(toupper(type) == "SPSS", "sav", "dta") , sep = ".")

    if (toupper(type) == "SPSS") {
        haven::write_sav(mdata, path = file.path(destination, filename))
    }
    else if (toupper(type) == "STATA") {
        haven::write_dta(mdata, path = file.path(destination, filename))
    }
}
