# setwd("~/Lucru/Institutii/Banca Mondiala/2020/WB Child/p3wb/")
# WB::makeVars("Child.ods", sheet = "ALT", "page/registrulALT/10_variabile_alt.js")


`makeVars` <- function(ods, sheet = "", js, newstyle = FALSE, sat = FALSE) {
    if (identical(sheet, "")) {
        sheet <- 1
    }
    
    aa <- readODS::read_ods(ods, sheet = sheet)
    aa$id <- admisc::trimstr(aa$id)
    aa$id <- tolower(aa$id)
    aa$active <- tolower(aa$active)
    aa$auto[is.na(aa$auto)] <- 0
    aa$auto[aa$auto != 0] <- 1
    aa$hidden[is.na(aa$hidden)] <- 0
    aa$hidden[aa$hidden != 0] <- 1
    aa$active[aa$auto == 1] <- NA


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
                aa$active[i] <- gsub("\\|\\|\\|\\|", "||", aa$active[i])
                aa$active[i] <- gsub("\\&\\&\\&\\&", "&&", aa$active[i])
                aa$active[i] <- gsub("====", "==", aa$active[i])
                aa$active[i] <- admisc::replaceText(aa$active[i], aa$id, bb)
            }
            # cat(paste(tolower(aa$active[i]), ")\n        },\n", sep = ""))
        }
        # cat("    }")
    }


    sink(js)
    cat("module.exports = {\n")
    cat("    questions: {\n")
    for (i in seq(nrow(aa))) {
        cat(paste("        '", aa$id[i], "': {\n", sep = ""))
        cat(paste("            'id': '", aa$id[i], "',\n", sep = ""))
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
        cat(paste("            'value': ", ifelse(aa$type[i] == "checkbox", "'0'", ifelse(aa$active[i] == "" | is.na(aa$active[i]), "'-9'", "'-7'")), ",\n", sep = ""))

        if (!newstyle) {
            aa$active[i] <- gsub("false|true", NA, tolower(aa$active[i]))
        }

        if (aa$auto[i] == 1) {
            aa$active[i] <- ifelse(newstyle, "true", NA)
        }

        disabled <- 1
        if (aa$auto[i] == 0 & (aa$active[i] == "" | is.na(aa$active[i]))) disabled <- 0
        
        cat(paste("            'disabled': ", disabled, ",\n", sep = ""))
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
    cat("    },\n")
    cat("    questionsOrder: [\n")
    order <- seq(nrow(aa))
    if (is.element("order", names(aa))) {
        if (!identical(as.integer(sort(unique(aa$order))), order)) {
            sink()
            message("Numerele de ordine nu sunt in regula.")
            return(invisible(NULL))
        }
        order <- aa$order
    }
    cat(paste("'", aa$id[order], "'", sep = "", collapse = ", "))

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
    cat("\n}")
    sink()
}

