
# htmlTranslate(
#     excel = "Downloads/01_cpis.xlsx", # sau "Downloads" daca toate fisierele sursa sunt acolo
#     template = "Documents/GitHub/wb-uzbek/src/templates",
#     destination = "Documents/GitHub/wb-uzbek/src/pages/instruments"
# )

# htmlTranslate("Downloads/01_cpis.xlsx")

htmlTranslate <- function(
    excel = "",
    sheet = "translations",
    languages = c(""),
    template = "",
    destination = "",
    ...
) {
    dots <- list(...)
    min <- ifelse(is.null(dots$min), 1, dots$min)
    max <- dots$max

    if (identical(template, "")) {
        # hardcoded for the Uzbek app
        template <- "Documents/GitHub/wb-uzbek/src/templates"
    }

    if (identical(destination, "")) {
        # hardcoded for the Uzbek app
        destination <- "Documents/GitHub/wb-uzbek/src/pages/instruments"
    }

    # TODO
    # "inputclass block"
    # "inputclass block w-full"
    # "inputclass block w-32" etc

    classes <- c(
        "inputclass"  = "border-gray-400 bg-white border text-gray-900 text-sm rounded focus:ring-blue-500 focus:border-blue-500 disabled:bg-gray-100 read-only:bg-gray-100 p-2.5",
        "valueclass"  = "border-gray-400 w-4 h-4 text-blue-600 focus:ring-blue-500 disabled:bg-gray-200",
        "cbclass"     = "border-gray-400 w-4 h-4 text-blue-600 focus:ring-blue-500 disabled:bg-gray-100 rounded",
        "labelclass"  = "ms-2 text-sm text-gray-900",
        "div1class"   = "col-span-1",
        "div2class"   = "col-span-1 flex gap-4",
        "div3class"   = "grid grid-cols-2 gap-4 items-start",
        "div31class"  = "grid grid-cols-3 gap-4 items-center",
        "div4class"   = "col-span-1 flex items-center",
        "div5class"   = "col-span-1 flex flex-col gap-2",
        "div6class"   = "col-span-1 flex gap-4 flex-col",
        "div7class"   = "col-span-1 flex gap-4 items-center",
        "div8class"   = "col-span-1 flex gap-4 items-center",
        "div9class"   = "flex items-center justify-center gap-2",
        "centerflex1" = "flex items-center",
        "centerflex2" = "flex items-center gap-2",
        "centerflex3" = "flex items-center gap-3",
        "centerflex4" = "flex items-center gap-4",
        "selectclass" = "border-gray-400 border text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block disabled:bg-gray-100 p-2.5 w-full",
        "smallclass"  = "my-2 block italic text-red-500",
        "h1class"     = "text-4xl text-gray-700 pl-4 tracking-wide mb-4",
        "h2class"     = "text-2xl font-bold text-gray-700 mb-4",
        "h3class"     = "text-xl font-semibold bg-gray-100 px-2 py-1 text-gray-700"
    )

    if (file_test("-f", excel)) {
        files <- basename(excel)
        dir <- suppressWarnings(normalizePath(dirname(excel), winslash = "/"))
    } else if (file_test("-d", excel)) {
        files <- list.files(excel)
        dir <- excel
    } else {
        admisc::stopError("Excel is not a file or directory")
    }

    for (file in files) {
        if (grepl("\\.xlsx", file)) {
            file <- gsub("\\.xlsx", "", file)
        }


        translations <- as.data.frame(
            suppressMessages(read_xlsx(
                file.path(dir, paste0(file, ".xlsx")),
                sheet = sheet
            ))
        )

        lang <- grepl("lang_", names(translations))
        langpos <- NULL

        if (identical(languages, c(""))) {
            # hardcoded for the Uzbek app
            languages <- c("en", "ru", "uz")

            if (any(lang)) {
                languages <- unique(gsub("lang_", "", names(translations)[lang]))
                langpos <- which(lang)
            }
        }


        if (is.null(langpos)) {
            if (!all(is.element(languages, names(translations)))) {
                print(languages)
                admisc::stopError("Language column(s) not found in the translations sheet")
            }

            langpos <- match(languages, names(translations))
        }

        translations <- translations[
            !is.na(translations$code),
            seq(max(which(
                is.element(names(translations), languages) | lang
            )))
        ]

        valsheet <- as.data.frame(
            read_xlsx(
                file.path(dir, paste0(file, ".xlsx")),
                sheet = "values"
            )
        )

        check <- is.na(valsheet$variable) | is.na(valsheet$value)
        valsheet <- valsheet[!check, ]

        if (file_test("-f", template)) {
            file <- basename(template)
            template <- suppressWarnings(normalizePath(dirname(template), winslash = "/"))
        } else if (!file_test("-d", template)) {
            admisc::stopError("Template is not a file or directory")
        }

        if (!file_test("-f", file.path(template, paste0(file, ".html")))) {
            admisc::stopError(sprintf("There is no file %s.html in the template directory", file))
        }

        translated <- vector(mode = "list", length = length(languages))
        names(translated) <- languages
        for (l in languages) {
            translated[[l]] <- readLines(file.path(template, paste0(file, ".html")))
        }

        nms <- names(classes)

        variable <- c()
        position <- c()

        for (i in seq(min, ifelse(is.null(max), length(translated[[1]]), max))) {
            for (j in seq_along(nms)) {
                translated[[1]][i] <- gsub(nms[j], classes[nms[j]], translated[[1]][i])
                if (length(languages) > 1) {
                    for (l in seq(2, length(languages))) {
                        translated[[l]][i] <- translated[[1]][i]
                    }
                }
            }

            bb <- betweenBrackets(translated[[1]][i], "{", regexp = "[[:alnum:]|_|-]*")

            if (length(bb) > 0) {
                vals <- grepl("-", bb)

                if (any(vals)) {
                    for (j in seq_along(bb[vals])) {
                        bbvals <- unlist(strsplit(bb[vals][j], "-"))

                        if (length(bb) == 2 & is.element(bb[2], translations$code)) {
                            pos <- which(translations$code == bb[2])
                        }
                        else if (length(bb) == 1) {
                            pos <- which(
                                translations$code == betweenBrackets(
                                    translated[[1]][i + 1],
                                    "{",
                                    regexp = "[[:alnum:]|_|-]*"
                                )[1]
                            )
                        }

                        if (length(pos) > 0) {
                            variable <- c(variable, bbvals[1])
                            position <- c(position, pos)
                        }

                        values <- valsheet$value[tolower(valsheet$variable) == tolower(bbvals[1])]
                        if (length(values) > 0) {
                            translated[[1]][i] <- gsub(
                                paste("\\{\\{", bb[vals][j], "\\}\\}", sep = ""),
                                values[as.numeric(bbvals[2])],
                                translated[[1]][i]
                            )

                            if (length(languages) > 1) {
                                for (l in seq(2, length(languages))) {
                                    translated[[l]][i] <- translated[[1]][i]
                                }
                            }
                        }
                    }
                }

                bb <- bb[!vals]
                if (length(bb) > 0) {
                    for (j in seq_along(bb)) {
                        for (l in seq_along(languages)) {
                            tc <- admisc::tryCatchWEM(
                                translated[[l]][i] <- gsub(
                                    paste("\\{\\{", bb[j], "\\}\\}", sep = ""),
                                    translations[translations$code == bb[j], langpos[l]],
                                    translated[[l]][i]
                                )
                            )

                            if (!is.null(tc$error)) {
                                print(file)
                                print(i)
                                print(j)
                                print(bb)
                                admisc::stopError("stop")
                            }
                        }
                    }
                }
            }
        }

        if (!file_test("-d", destination)) {
            admisc::stopError("Destination should be a directory")
        }

        for (l in seq_along(languages)) {
            writeLines(translated[[l]], file.path(destination, paste0(file, "_", languages[l], ".html")))
        }
        # writeLines(en, file.path(destination, paste0(file, "_en.html")))
        # writeLines(ru, file.path(destination, paste0(file, "_ru.html")))
        # writeLines(uz, file.path(destination, paste0(file, "_uz.html")))
    }

    # return(invisible(data.frame(variable, position)))
    # return(invisible(list(en = en, ru = ru, uz = uz)))
}
