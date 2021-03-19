# setwd("~/Lucru/Institutii/Banca Mondiala/2020/WB Child/p3wb/")
# WB::makeVars("Child.xlsx", sheet = "ALT", "page/registrulALT/10_variabile_alt.js")


`makeVars` <- function(excel, sheet = "", js, newstyle = TRUE, sat = FALSE, headers = TRUE, node = FALSE, ...) {
    
    on.exit(suppressWarnings(sink()))
    
    if (identical(sheet, "")) {
        sheet <- 1
    }
    
    aa <- readxl::read_excel(excel, sheet = sheet)
    aa$id <- admisc::trimstr(aa$id)
    aa$id <- tolower(aa$id)
    aa$active <- tolower(aa$active)
    aa$auto[is.na(aa$auto)] <- 0
    aa$auto[aa$auto != 0] <- 1
    aa$hidden[is.na(aa$hidden)] <- 0
    aa$hidden[aa$hidden != 0] <- 1
    # aa$active[aa$auto == 1] <- NA


    if (newstyle) {
        # cat(",\n")
        # cat("    newstyles: {\n")
        # bb <- paste("instrument.questions.", aa$id, ".value", sep = "")
        bb <- paste("instrument.questions.", aa$id, ".value", sep = "")
        for (i in seq(nrow(aa))) {
            # cat(paste("        '", aa$id[i], "': function() {\n            return(", sep = ""))
            if (aa$active[i] == "" | is.na(aa$active[i])) {
                aa$active[i] <- "true"
            }
            else {
                aa$active[i] <- gsub("\\&", "&&", aa$active[i])
                aa$active[i] <- gsub("\\|", "||", aa$active[i])
                aa$active[i] <- gsub("\\=", "==", aa$active[i])
                aa$active[i] <- gsub("\\!\\=\\=", "!=", aa$active[i])
                aa$active[i] <- gsub(">==", ">=", aa$active[i])
                aa$active[i] <- gsub("<==", "<=", aa$active[i])
                aa$active[i] <- gsub("\\|\\|\\|\\|", "||", aa$active[i])
                aa$active[i] <- gsub("\\&\\&\\&\\&", "&&", aa$active[i])
                aa$active[i] <- gsub("====", "==", aa$active[i])
                aa$active[i] <- admisc::replaceText(aa$active[i], aa$id, bb)
            }
            # cat(paste(tolower(aa$active[i]), ")\n        },\n", sep = ""))
        }
        # cat("    }")
    }

    
    if (is.element("section", names(aa))) {
        pos <- which(!is.na(aa$section))
        snames <- aa$section[pos]
        pos[1] <- 1
        for (i in seq(length(pos))) {
            if (i < length(pos)) {
                aa$section[seq(pos[i], pos[i + 1] - 1)] <- i
            }
            else {
                aa$section[seq(pos[i], nrow(aa))] <- i
            }
        }
    }
    else {
        aa$section <- 1
    }


    sink(js)
    
    if (node) {
        cat("module.exports = {\n")
        cat("    questions: {\n")
    }
    else {
        cat("var questions = {\n")
    }
    
    for (i in seq(nrow(aa))) {
        cat(paste("        '", aa$id[i], "': {\n", sep = ""))
        cat(paste("            'id': '", aa$id[i], "',\n", sep = ""))
        
        cat(paste("            'section': ", aa$section[i], ",\n", sep = ""))

        if (newstyle) {
            cat(paste("            'type': '", aa$type[i], "',\n", sep = ""))
        }
        else {
            itype <- ""
            if (is.element(aa$type[i], c("number", "double"))) {
                itype <- aa$type[i] 
                aa$type[i] <- "input"
            }
            cat(paste("            'type': '", aa$type[i], "',\n", sep = ""))
            cat(paste("            'itype': '", itype, "',\n", sep = ""))
        }
        cat(paste("            'value': ", ifelse(aa$type[i] == "checkbox", "0", ifelse(aa$active[i] == "true" | is.na(aa$active[i]), "-9", "-7")), ",\n", sep = ""))

        if (!newstyle) {
            aa$active[i] <- gsub("false|true", NA, tolower(aa$active[i]))
        }

        if (aa$auto[i] == 1) {
            aa$active[i] <- ifelse(newstyle, "true", NA)
        }

        disabled <- 1
        if (aa$auto[i] == 0 & (aa$active[i] == "" | is.na(aa$active[i]))) disabled <- 0
        
        cat(paste("            'disabled': ", disabled, ",\n", sep = ""))
        cat(paste("            'hidden': ", ifelse(aa$hidden[i], "true", "false"), ",\n", sep = ""))
        cat(paste("            'order': ", i - 1, ",\n", sep = ""))

        if (newstyle) {
            aa$active[is.na(aa$active)] <- "true"
            cat(paste("            'active': function() {return(", aa$active[i], ")},\n", sep = ""))
        }
        else {
            cat(paste("            'active': '", ifelse(is.na(aa$active[i]), "", aa$active[i]), "',\n", sep = ""))
        }
        cat(paste("            'error': ''", sep = ""))

        if (aa$type[i] == "radio") {
            cat(paste(",\n            'checked': 0\n"))
        }
        else {
            cat("\n")
        }

        cat("        },\n")
    }

    if (node) {
        cat("    },\n")
        cat("    questionsOrder: [\n        ")
    }
    else {
        cat("    };\n\n")
        cat("var questionsOrder = [\n        ")
    }

    order <- seq(nrow(aa))
    if (is.element("order", names(aa))) {
        if (!identical(as.integer(sort(unique(aa$order))), order)) {
            sink()
            message("Numerele de ordine nu sunt in regula.")
            return(invisible(NULL))
        }
        order <- aa$order
    }

    cat(paste(strwrap(paste("'", aa$id, "'", sep = "", collapse = ", "), width = 110), collapse = "\n        "))

    cat("\n    ]")

    if (sat) {
        cat(",\n")
        bb <- aa$id[grepl("_sat", aa$id)]
        if (length(bb) > 0) {
            cat("    sate: [\n")
            cat(paste("'", bb, "'", sep = "", collapse = ", "))
            cat("\n    ]")
        }
    }

    if (headers) {
        if (node) {
            cat(",\n")
            cat("    exportHeader:[\n")
        }
        else {
            cat(";\n\n")
            cat("var exportHeader = [\n")
        }
        cat(paste("        {'id': '", aa$id, "', 'title': '", toupper(aa$id), "'},", sep = "", collapse = "\n"))
        cat("\n    ]")
    }

    if (node) {
        cat("\n}")
    }



    # sink()
}
