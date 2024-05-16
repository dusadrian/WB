# setwd("~/Lucru/Institutii/Banca Mondiala/2020/WB Child/p3wb/")
# WB::makeVars("Child.xlsx", sheet = "ALT", "page/registrulALT/10_variabile_alt.sql")


`makeSQL` <- function(excel, sheet = 1, tabela = NULL, dir = NULL) {

    if (is.null(tabela)) {
        admisc::stopError("Lipseste numele tabelei SQL.")
    }

    if (!is.atomic(tabela) || !is.character(tabela)) {
        admisc::stopError("Argumentul tabela trebuie sa fie un string.")
    }

    if (!is.null(dir)) {
        if (!is.atomic(dir) || !is.character(dir)) {
            admisc::stopError("Argumentul dir trebuie sa fie un string.")
        }
    }

    on.exit(suppressWarnings(sink()))

    if (identical(sheet, "")) {
        sheet <- 1
    }

    aa <- readxl::read_excel(excel, sheet = sheet)
    aa <- aa[, c("id", "type")]
    aa$id <- admisc::trimstr(aa$id)
    aa$id <- tolower(aa$id)
    maxlen <- max(nchar(aa$id))
    extra <- sapply(aa$id, function(x) {
        paste(rep(" ", maxlen - nchar(x)), collapse = "")
    })


    comanda <- c(
        paste("CREATE TABLE", tabela, "("),
        "    id", paste(rep(" ", maxlen - 2)), "PRIMARY KEY UNIQUE,",
        paste0("    ", aa$id, extra, " TEXT", c(rep(",", nrow(aa) - 1), "")),
        ");"
    )

    if (!endsWith(tolower(tabela), ".sql")) {
        tabela <- paste(tabela, "sql", sep = ".")
    }

    sink(
        ifelse(is.null(dir), tabela, file.path(dir, tabela))
    )

    cat(paste(comanda, collapse = "\n"))
}
