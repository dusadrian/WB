pocufiles <- function(datacsv, sheet = NULL, template = "", dirname = "") {

    if (identical(template, "")) {
        stop("Lipseste calea catre fisierul template.")
    }

    if (identical(dirname, "")) {
        stop("Lipseste calea catre directorul in care sa se creeze datele.")
    }

    if (is.null(sheet) || !is.atomic(sheet) || !is.numeric(sheet) || length(sheet) != 1) {
        stop("Argumentul sheet trebuie sa fie un numar")
    }

    pocu <- readxl::read_xlsx(datacsv, sheet = sheet)
    nms <- names(pocu)
    options(scipen = 15)
    
    # integer raw vector, obtained with e.g.: as.integer(charToRaw("~"))
    smare <- rawToChar(as.raw(as.integer(c(200, 152))))
    tmare <- rawToChar(as.raw(as.integer(c(200, 154))))

    pocu$C27 <- caractere(pocu$C27, c(amare = "A", smare = smare, tmare = tmare)) # judet
    pocu$C32 <- caractere(pocu$C32, c(amare = "A", smare = smare, tmare = tmare)) # judet
    pocu$C28 <- caractere(pocu$C28, c(smare = smare, tmare = tmare))
    pocu$C33 <- caractere(pocu$C33, c(smare = smare, tmare = tmare))

    
    pocu$C16 <- format(pocu$C16, format = "%d.%m.%Y")
    pocu$C17 <- format(pocu$C17, format = "%d.%m.%Y")
    pocu$C40 <- format(pocu$C40, format = "%d.%m.%Y")
    pocu$D128 <- format(pocu$D128, format = "%d.%m.%Y")
    pocu$C140 <- format(pocu$C140, format = "%d.%m.%Y")


    if (!dir.exists(dirname)) {
        dir.create(dirname)
    }

    if (!dir.exists(file.path(dirname, "formulare_validate"))) {
        dir.create(file.path(dirname, "formulare_validate"))
    }


    options(java.parameters = "-Xmx4g")

    for (i in 1:10) { # seq(nrow(pocu))
        filename <- file.path(dirname, "formulare_validate", paste(pocu$ID[i], tools::file_ext(template), sep = "."))
        file.copy(template, filename)

        wb <- XLConnect::loadWorkbook(filename, create = TRUE)
        dateblue <- XLConnect::getOrCreateCellStyle(wb, name = "dateblue")
        datebluebox <- XLConnect::getOrCreateCellStyle(wb, name = "datebluebox")
        number <- XLConnect::getOrCreateCellStyle(wb, name = "number")
        textblue <- XLConnect::getOrCreateCellStyle(wb, name = "textblue")
        Xbox <- XLConnect::getOrCreateCellStyle(wb, name = "Xbox")

        Xboxes <- c(
            "E31", "C36", "C38", "C57", "C63", "E65", "C67", "E69", "C71", "E73",
            "C75", "E75", "C77", "E77", "C79", "E79", "C81", "E81", "C83", "E83",
            "C85", "C91", "E93", "C97", "C99", "C101", "E101", "C103", "C105",
            "E105", "C107", "E107", "C109", "E109", "C111", "C113", "E113", "C115",
            "C117", "E117", "C119", "C121", "E121", "C123", "C125", "E125", "C220",
            "C224", "C228", "C232", "C236", "C240", "C244", "C248", "C252", "C256",
            "C260", "C264", "C268", "C272", "C276", "C280", "C284", "C288", "C292",
            "C296", "C300", "C304", "C308", "C312", "C316", "C320", "C324", "C328",
            "C332", "C336", "C340", "C344", "C348", "C352", "C356", "C360", "C364",
            "C368", "C372", "C376", "C380", "C384", "C388", "C392", "C396", "C400",
            "C404", "C408", "C412", "C416", "C420", "C424", "C428", "C432", "C436",
            "C440", "C444", "C448", "C452", "C456", "C460", "C464", "C468", "C472",
            "C476", "C480", "C484", "C488", "C492", "C496", "C500", "C504", "C508",
            "C512", "C516"
        )

        e31x <- TRUE

        for (j in seq(2, ncol(pocu))) {
            if (is.na(pocu[i, j]) || !identical(pocu[i, j], "")) {
                nmj <- nms[j]
                col <- match(substring(nmj, 1, 1), LETTERS)
                row <- as.numeric(substring(nmj, 2))
                valid <- TRUE
                if (nmj == "E31") {
                    e31x <- pocu[i, nmj] == "X"
                }

                if (e31x) {
                    valid <- !is.element(nmj, c("C32", "C33", "C34"))
                }

                if (valid) {
                    XLConnect::writeWorksheet(
                        wb,
                        pocu[i, j],
                        sheet = 1,
                        startRow = row,
                        startCol = col,
                        header = FALSE
                    )
                }

                if (is.element(nmj, c("C16", "C17", "C40", "C140"))) {
                    XLConnect::setCellStyle(wb, sheet = 1, row = row, col = col, cellstyle = dateblue)
                }
                else if ( nmj == "D128") {
                    XLConnect::setCellStyle(wb, sheet = 1, row = row, col = col, cellstyle = datebluebox)
                }
                else if (nmj == "C41") { #CNP
                    XLConnect::setCellStyle(wb, sheet = 1, row = row, col = col, cellstyle = number)
                }
                else if (is.element(nmj, Xboxes)) {
                    XLConnect::setCellStyle(wb, sheet = 1, row = row, col = col, cellstyle = Xbox)
                }
                else {
                    XLConnect::setCellStyle(wb, sheet = 1, row = row, col = col, cellstyle = textblue)
                }
            }
        }

        XLConnect::setForceFormulaRecalculation(wb, sheet = 1, TRUE)
        XLConnect::saveWorkbook(wb)

        rm(wb)
        XLConnect::xlcFreeMemory()
    }
}
