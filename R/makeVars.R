# setwd("~/Lucru/Institutii/Banca Mondiala/2020/WB Child/p3wb/")
# WB::makeVars("Child.xlsx", sheet = "ALT", "page/registrulALT/10_variabile_alt.js")


`makeVars` <- function(excel, sheet = "", js, ...) {

    on.exit(suppressWarnings(sink()))

    if (identical(sheet, "")) {
        sheet <- 1
    }

    aa <- readxl::read_excel(excel, sheet = sheet)
    aa$id <- admisc::trimstr(aa$id)
    aa$id <- tolower(aa$id)
    aa$active <- admisc::trimstr(tolower(aa$active))
    aa$auto[is.na(aa$auto)] <- 0
    aa$auto[aa$auto != 0] <- 1
    aa$hidden[is.na(aa$hidden)] <- 0
    aa$hidden[aa$hidden != 0] <- 1


    bb <- paste("Number(instrument.questions.", aa$id, ".value)", sep = "")
    for (i in seq(nrow(aa))) {
        # if (aa$active[i] == "" | is.na(aa$active[i]) | aa$auto[i] == 1) {
        #     aa$active[i] <- "true"
        # }
        # else {
        if (aa$active[i] != "true") {
            aa$active[i] <- gsub("\\&", "&&", aa$active[i])
            aa$active[i] <- gsub("\\&\\&\\&\\&", "&&", aa$active[i])
            aa$active[i] <- gsub("\\|", "||", aa$active[i])
            aa$active[i] <- gsub("\\|\\|\\|\\|", "||", aa$active[i])
            aa$active[i] <- gsub("\\=", "==", aa$active[i])
            aa$active[i] <- gsub("====", "==", aa$active[i])
            aa$active[i] <- gsub("\\!\\=\\=", "!=", aa$active[i])
            aa$active[i] <- gsub(">==", ">=", aa$active[i])
            aa$active[i] <- gsub("<==", "<=", aa$active[i])
            aa$active[i] <- admisc::replaceText(aa$active[i], aa$id, bb)
        }
    }


    sink(js)

    cat("import instrument from '../../libraries/instrument';\n")
    cat("import { QuestionObjectType } from '../../libraries/in terfaces';\n")

    cat("\n\n")
    cat("export const questions: QuestionObjectType = {\n")

    for (i in seq(nrow(aa))) {
        cat(paste("    '", aa$id[i], "': {\n", sep = ""))
        cat(sprintf("        name: '%s',\n", aa$id[i]))
        cat(paste("        type: '", aa$type[i], "',\n", sep = ""))
        cat(paste(
            "        value: ",
            ifelse(
                aa$type[i] == "checkbox",
                "'0'",
                ifelse(aa$active[i] == "true", "'-9'", "'-7'")
            ),
            ",\n",
            sep = ""
        ))

        cat(paste(
            "        disabled: ",
            ifelse(
                aa$active[i] != "true", "true", "false"
            ),
            ",\n",
            sep = ""
        ))

        cat(paste(
            "        hidden: ",
            ifelse(aa$hidden[i], "true", "false"),
            ",\n",
            sep = ""
        ))

        cat(paste(
            "        readonly: ",
            ifelse(aa$auto[i], "true", "false"),
            ",\n",
            sep = ""
        ))


        cat(paste("        order: ", i - 1, ",\n", sep = ""))
        cat(paste("        active: function() {return(", aa$active[i], ")},\n", sep = ""))
        cat(paste("        skip: false", sep = ""))

        if (aa$type[i] == "checkbox") {
            cat(",\n         checked: 0")
        }

        cat("\n    },\n")
    }

    cat("};\n\n")

    cat("export const questionsOrder: Array<string> = [\n    ")

    order <- seq(nrow(aa))

    cat(paste(
        strwrap(
            paste("'", aa$id, "'", sep = "", collapse = ", "),
            width = 110
        ),
        collapse = "\n    "
    ))

    cat("\n];\n\n")

    cat("export const exportHeader: Array<{ id: string; title: string }> = [\n")

    cat(paste(
        "    {'id': '",
        aa$id,
        "', 'title': '",
        toupper(aa$id),
        "'},",
        sep = "",
        collapse = "\n"
    ))

    cat("\n];")

}
