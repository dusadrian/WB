# LC_ALL=C.UTF-8 Rscript -e  "WB::makeFile(instrument = 1, type = 'SPSS', partial = FALSE, destination = '~', host = '127.0.0.1', port = NULL, dbname = 'c1_fir_db', user = 'c1_fir', password = '_A79qxeYzMQCy')"

`makeFile` <- function(
    instrument = NULL, type = NULL, partial = FALSE, destination = "~",
    empty = TRUE, diacritice = FALSE, host = "127.0.0.1", port = NULL,
    dbname = "", user = "", password = "") {

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

            # host = '127.0.0.1',
            # dbname = 'c1_fir_db',
            # user = 'c1_fir',
            # password = '_A79qxeYzMQCy'
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

    chestionare <- DBI::dbGetQuery(con, "SELECT * FROM answers")
    chestionare$id <- as.numeric(chestionare$id) # este integer64 si nu merge match() de ex.

    judete <- DBI::dbGetQuery(con, "SELECT * FROM counties")
    judete$id <- as.numeric(judete$id)

    
    # sql <- "SELECT answer_id, name, value FROM answer_values LEFT JOIN answers ON answers.id = answer_values.answer_id"

    sql <- paste(
        "SELECT answer_id, name, value FROM answer_values",
        "LEFT JOIN answers ON answers.id = answer_values.answer_id"
    )

    if (!is.null(instrument)) {
        sql <- paste(sql, "WHERE instrument_id =", instrument)
    }

    data <- DBI::dbGetQuery(con, sql)

    # instruments <- DBI::dbGetQuery(con, "SELECT id, folder FROM instruments")

    DBI::dbDisconnect(con)

    if (nrow(data) == 0) {
        cat("\n")
        stop(simpleError("Nu sunt date pentru acest instrument.\n\n"))
    }


    nms <- names(dataDscr)
    mdata <- matrix(NA, ncol = length(nms), nrow = nrow(chestionare))
    colnames(mdata) <- nms


    for (i in seq(length(chestionare$id))) {
        sdata <- data[data$answer_id == chestionare$id[i], , drop = FALSE]
        columns <- match(sdata$name, nms)
        if (any(is.na(columns))) {
            cat("\n")
            stop(simpleError(paste("Variabile lipsa:", paste(sdata$name[is.na(columns)], collapse = ", "), "\n\n")))
        }
        mdata[i, columns] <- sdata$value
    }

    mdata <- gsub("&amp;quot;", "", mdata)
    mdata <- gsub("&#039;", "", mdata)
    mdata <- gsub("&quot;", "", mdata)
    mdata <- gsub(",,-,,", "-", mdata)
    mdata[grepl("amp;amp;amp;", mdata)] <- "-"
    mdata[is.element(mdata, "#")] <- "-"
    mdata[is.element(mdata, -77)] <- -7
    mdata <- as.data.frame(mdata, stringsAsFactors = FALSE)

    mdata$adr1 <- judete$name[match(chestionare$county_id, judete$id)]

    if (toupper(type) == "SPSS" & !diacritice) {
        mdata[] <- lapply(mdata, caractere)
    }

    mdata[] <- lapply(mdata, function(x) {
        if (admisc::possibleNumeric(x)) {
            x <- admisc::asNumeric(x)
            if (admisc::wholeNumeric(x)) {
                x <- as.integer(x)
            }
        }
        return(x)
    })

    mdata <- cbind(
        partial = chestionare$partial,
        codg = mdata$adr2uat * 1000 + mdata$cfo,
        codjud = chestionare$county_id,
        mdata
    )


    nmsp <- c(
        "sex", "dn", "age", "nat", "sciv", "nive", "ocup", 
        "kids", "momp", "codm", "momnp", "momprez", "dadp", "codd", "dadnp", 
        "dadprez", "fmon", "acasa1", "acasa2", "acasa0", "mainresp", 
        "mainralt", "mom18", "preg18", "acte1", "acte2", "abnsp", "kidmgr", 
        "rein", "plas", "talk", "pac", "ingr", "ttl", "diz", "certif", 
        "vld", "grad", "grav", "diztip", "subn", "obz", "med", "vaccin", 
        "polisp", "risc3e2", "risc3e3", "risc3e4", "risc3e5", "risc3e6", 
        "risc3e7", "risc3e9", "risc3e10", "risc3e11", "risc3e12", "risc3e14", 
        "risc3e15", "preg", "mdsar", "mdct", "plan", "cit", "scris", 
        "schf", "scho", "noschf", "abn", "schnvr", "ces", "crsh", "abs", 
        "corj", "mmic", "repet", "sanc", "alc", "drg", "tut"
    )

    persoane <- vector(mode = "list", length = length(nmsp))

    for (i in seq(length(persoane))) {
        nms <- names(mdata)
        nms <- gsub(nmsp[i], "", nms)
        nms <- gsub("_|[0-9]+$", "", nms)
        persoane[[i]] <- which(nms == "")
    }

    maxpers <- max(chestionare$nrpersons)
    todelete <- unlist(lapply(persoane, function(x) {
        if (maxpers < length(x)) {
            x <- x[seq(maxpers + 1, 20)]
        }
        return(x)
    }))

    mdata <- mdata[, -todelete, drop = FALSE]

    # trebuie refacut index-ul persoanelor, pentru ca am sters coloane din mdata
    for (i in seq(length(persoane))) {
        nms <- names(mdata)
        nms <- gsub(nmsp[i], "", nms)
        nms <- gsub("_|[0-9]+$", "", nms)
        persoane[[i]] <- which(nms == "")[seq(maxpers)]
    }


    varpers <- c(
        "partial",
        "adr1", # judet
        "codjud", # chestionare$county_id
        "adr2", # comuna
        "adr2uat", # siruta comunei
        "adr3", # satul
        "adr3uat", # siruta satului
        "codg",
        "cpers",
        "codpers"
    )

    
    pers <- as.data.frame(matrix(nrow = 0, ncol = length(nmsp) + length(varpers)))
    names(pers) <- c(varpers, nmsp)

    cpers <- seq(length(persoane[[1]]))
    for (i in cpers) {
        temp <- mdata[, unlist(lapply(persoane, "[[", i))]
        names(temp) <- nmsp

        temp <- cbind(
            stringsAsFactors = FALSE,
            partial = chestionare$partial,
            adr1 = mdata$adr1,
            codjud = chestionare$county_id,
            adr2 = mdata$adr2,
            adr2uat = mdata$adr2uat,
            adr3 = mdata$adr3,
            adr3uat = mdata$adr3uat,
            codg = mdata$codg,
            cpers = cpers[i],
            codpers = mdata$codg * 100 + cpers[i],
            temp
        )

        pers <- rbind(pers, temp[chestionare$nrpersons >= i, , drop = FALSE])
    }

    mdata <- mdata[, -unlist(persoane)]

    dataDscr$codg = list(label = "Cod gospodarie")
    dataDscr$cpers = list(label = "Cod persoana")
    dataDscr$codpers = list(label = "Cod unic persoana")
    dataDscr$codjud = list(label = "Cod judet")
    dataDscr$partial = list(
        label = "Status chestionar: partial",
        labels = c(
            Da = 1,
            Nu = 0
        )
    )

    pers <- pers[order(pers$codg), , drop = FALSE]

    
    nms <- names(dataDscr)
    sep <- rep("", length(nmsp))
    sep[grepl("[0-9]+$", nmsp)] <- "_"
    vb <- c(varpers, paste0(nmsp, sep, 1))
    
    for (i in seq(ncol(pers))) {
        pers[, i] <- makeAttributes(
            pers[, i],
            dataDscr[[vb[i]]],
            diacritice = diacritice,
            type = type
        )
    }

    nms <- names(mdata)
    for (i in seq(ncol(mdata))) {
        mdata[, i] <- makeAttributes(
            mdata[, i],
            dataDscr[[nms[i]]],
            diacritice = diacritice,
            type = type
        )
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



    pers[] <- lapply(pers, function(x) {
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

    filename_gosp <- paste(
        "gosp",
        ifelse(toupper(type) == "SPSS", "sav", "dta"),
        sep = "."
    )

    filename_pers <- paste(
        "pers",
        ifelse(toupper(type) == "SPSS", "sav", "dta"),
        sep = "."
    )

    if (toupper(type) == "SPSS") {
        haven::write_sav(mdata, path = file.path(destination, filename_gosp))
        haven::write_sav(pers, path = file.path(destination, filename_pers))
    }
    else if (toupper(type) == "STATA") {
        haven::write_dta(mdata, path = file.path(destination, filename_gosp))
        haven::write_dta(pers, path = file.path(destination, filename_pers))
    }
}
