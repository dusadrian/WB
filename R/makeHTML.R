
# # setwd("~/Lucru/Institutii/Banca Mondiala/2021/static/")

# treatExcel <- function(excel, type = "") {
#     excel$table[is.na(excel$table)] <- 0
#     todelete <- c()

#     if (type == "LP") {
#         # la tabelul 16 introduc m1_q3a_ck
#         linie <- which(excel$table == 16)[1]
#         temp <- data.frame(order = 1, question = "m1_q3", id = "m1_q3a_ck", type = "checkbox",
#                             active = NA, auto = NA, hidden = 0, table = 16, section = NA, Comentarii = NA)
#         excel <- rbind(excel[seq(1, linie - 1), ], temp, excel[seq(linie, nrow(excel)), ])

#         # la tabelul 17 m1_q5a_a_ck
#         linie <- which(excel$table == 17)[1]
#         temp <- data.frame(order = 1, question = "m1_q5", id = "m1_q5a_a_ck", type = "checkbox",
#                             active = NA, auto = NA, hidden = 0, table = 17, section = NA, Comentarii = NA)
#         excel <- rbind(excel[seq(1, linie - 1), ], temp, excel[seq(linie, nrow(excel)), ])

#         # la tabelul 28 m1_s4_5b_1_c
#         linie <- which(excel$table == 28)[1]
#         temp <- data.frame(order = 1, question = "m1_s1_13", id = "m1_s4_5b_1_c", type = "checkbox",
#                             active = NA, auto = NA, hidden = 0, table = 28, section = NA, Comentarii = NA)
#         excel <- rbind(excel[seq(1, linie - 1), ], temp, excel[seq(linie, nrow(excel)), ])

#         # la tabelul 30 m1_s4_9b_1_c
#         linie <- which(excel$table == 30)[1]
#         temp <- data.frame(order = 1, question = "m1_s4_9b", id = "m1_s4_9b_1_c", type = "checkbox",
#                             active = NA, auto = NA, hidden = 0, table = 30, section = NA, Comentarii = NA)
#         excel <- rbind(excel[seq(1, linie - 1), ], temp, excel[seq(linie, nrow(excel)), ])

#         todelete <- which(is.element(excel$id, c("m2_s4_5e_care")))

#     }
#     else if (type == "CZ") {
#         # la tabelul 21 introduc m1_s2_4_n, dupa prima linie
#         linie <- which(excel$table == 21)[1]
#         temp <- data.frame(order = 1, question = "m1_s2_4", id = "m1_s2_4_n", type = "number",
#                             active = '', auto = NA, hidden = 1, table = 21, section = NA, Comentarii = NA)
#         excel <- rbind(excel[seq(1, linie), ], temp, excel[seq(linie + 1, nrow(excel)), ])
#         codeBook$dataDscr$m1_s2_4_n <<- codeBook$dataDscr$m1_s2_5a_n


#         # mai introduc si m1_s2_5b_n
#         temp <- data.frame(order = 1, question = "m1_s2_4", id = "m1_s2_5b_n", type = "number",
#                             active = '', auto = NA, hidden = 1, table = 21, section = NA, Comentarii = NA)
#         excel <- rbind(excel[seq(1, linie + 6), ], temp, excel[seq(linie + 7, nrow(excel)), ])
#         codeBook$dataDscr$m1_s2_5b_n <<- codeBook$dataDscr$m1_s2_5a_n


#         # mai introduc si m1_s2_5b_s
#         temp <- data.frame(order = 1, question = "m1_s2_4", id = "m1_s2_5b_s", type = "number",
#                             active = '', auto = NA, hidden = 1, table = 21, section = NA, Comentarii = NA)
#         excel <- rbind(excel[seq(1, linie + 7), ], temp, excel[seq(linie + 8, nrow(excel)), ])
#         codeBook$dataDscr$m1_s2_5b_s <<- codeBook$dataDscr$m1_s2_5a_s


#         # la tabelul 15 introduc m1_q3a_ck
#         linie <- which(excel$table == 15)[1]
#         temp <- data.frame(order = 1, question = "m1_q3", id = "m1_q3a_ck", type = "checkbox",
#                             active = '', auto = NA, hidden = 1, table = 15, section = NA, Comentarii = NA)
#         excel <- rbind(excel[seq(1, linie - 1), ], temp, excel[seq(linie, nrow(excel)), ])
#         codeBook$dataDscr$m1_q3a_ck <<- codeBook$dataDscr$m1_q3b_ck


#         # la tabelul 16 introduc m1_q5a_a_ck
#         linie <- which(excel$table == 16)[1]
#         temp <- data.frame(order = 1, question = "m1_q5", id = "m1_q5a_a_ck", type = "checkbox",
#                             active = '', auto = NA, hidden = 1, table = 16, section = NA, Comentarii = NA)
#         excel <- rbind(excel[seq(1, linie - 1), ], temp, excel[seq(linie, nrow(excel)), ])
#         codeBook$dataDscr$m1_q5a_a_ck <<- codeBook$dataDscr$m1_q5b_a_ck


#         todelete <- which(is.element(excel$id, c("m4_s7_2_a5_ck", "m4_s7_2_a5_a_care")))
#         codeBook$dataDscr$m4_s7_2_a5_ck <<- NULL
#         codeBook$dataDscr$m4_s7_2_a5_a_care <<- NULL






#     }
#     else if (type == "CRSP_CCZ") {
#         # la tabelul 16 introduc m1q3_ack
#         tabel <- 16
#         linie <- which(excel$table == tabel)[1]
#         temp <- data.frame(order = 1, question = "m1q3", id = "m1q3_ack", type = "checkbox",
#                             active = '', auto = NA, hidden = 1, table = tabel, section = NA, Comentarii = NA)
#         excel <- rbind(excel[seq(1, linie - 1), ], temp, excel[seq(linie, nrow(excel)), ])
#         codeBook$dataDscr$m1q3_ack <<- codeBook$dataDscr$m1q3_bck

#         # la tabelul 17 introduc m1q5_aack
#         tabel <- 17
#         linie <- which(excel$table == tabel)[1]
#         temp <- data.frame(order = 1, question = "m1q5", id = "m1q5_aack", type = "checkbox",
#                             active = '', auto = NA, hidden = 1, table = tabel, section = NA, Comentarii = NA)
#         excel <- rbind(excel[seq(1, linie - 1), ], temp, excel[seq(linie, nrow(excel)), ])
#         codeBook$dataDscr$m1q5_aack <<- codeBook$dataDscr$m1q5_back

#         # la tabelul 21 introduc mai multe
#         tabel <- 21
#         linie <- which(excel$table == 21)[5]
#         temp <- data.frame(order = 1:2, question = rep("m1s2_2", 2), id = c("m1s2_2bn", "m1s2_2bs"),
#                             type = rep("number", 2), active = rep('', 2), auto = rep(NA, 2),
#                             hidden = rep(1, 2), table = rep(tabel, 2), section = rep(NA, 2),
#                             Comentarii = rep(NA, 2))
#         excel <- rbind(excel[seq(1, linie - 1), ], temp, excel[seq(linie, nrow(excel)), ])
#         codeBook$dataDscr$m1s2_2bn <<- codeBook$dataDscr$m1s2_2an
#         codeBook$dataDscr$m1s2_2bs <<- codeBook$dataDscr$m1s2_2as

#         linie <- linie + 3
#         temp <- data.frame(order = 1:2, question = rep("m1s2_2", 2), id = c("m1s2_2cn", "m1s2_2cs"),
#                             type = rep("number", 2), active = rep('', 2), auto = rep(NA, 2),
#                             hidden = rep(1, 2), table = rep(tabel, 2), section = rep(NA, 2),
#                             Comentarii = rep(NA, 2))
#         excel <- rbind(excel[seq(1, linie - 1), ], temp, excel[seq(linie, nrow(excel)), ])
#         codeBook$dataDscr$m1s2_2cn <<- codeBook$dataDscr$m1s2_2an
#         codeBook$dataDscr$m1s2_2cs <<- codeBook$dataDscr$m1s2_2as

#         linie <- linie + 4
#         temp <- data.frame(order = 1, question = "m1s2_2", id = "m1s2_2ds", type = "number",
#                             active = '', auto = NA, hidden = 1, table = tabel, section = NA, Comentarii = NA)
#         excel <- rbind(excel[seq(1, linie - 1), ], temp, excel[seq(linie, nrow(excel)), ])
#         codeBook$dataDscr$m1s2_2ds <<- codeBook$dataDscr$m1s2_2as

#         linie <- linie + 2
#         temp <- data.frame(order = 1:2, question = rep("m1s2_2", 2), id = c("m1s2_2en", "m1s2_2es"),
#                             type = rep("number", 2), active = rep('', 2), auto = rep(NA, 2),
#                             hidden = rep(1, 2), table = rep(tabel, 2), section = rep(NA, 2),
#                             Comentarii = rep(NA, 2))
#         excel <- rbind(excel[seq(1, linie - 1), ], temp, excel[seq(linie, nrow(excel)), ])
#         codeBook$dataDscr$m1s2_2en <<- codeBook$dataDscr$m1s2_2an
#         codeBook$dataDscr$m1s2_2es <<- codeBook$dataDscr$m1s2_2as

#         linie <- linie + 3
#         temp <- data.frame(order = 1:2, question = rep("m1s2_2", 2), id = c("m1s2_2fn", "m1s2_2fs"),
#                             type = rep("number", 2), active = rep('', 2), auto = rep(NA, 2),
#                             hidden = rep(1, 2), table = rep(tabel, 2), section = rep(NA, 2),
#                             Comentarii = rep(NA, 2))
#         excel <- rbind(excel[seq(1, linie - 1), ], temp, excel[seq(linie, nrow(excel)), ])
#         codeBook$dataDscr$m1s2_2fn <<- codeBook$dataDscr$m1s2_2an
#         codeBook$dataDscr$m1s2_2fs <<- codeBook$dataDscr$m1s2_2as

#         linie <- linie + 3
#         temp <- data.frame(order = 1:2, question = rep("m1s2_2", 2), id = c("m1s2_2gn", "m1s2_2gs"),
#                             type = rep("number", 2), active = rep('', 2), auto = rep(NA, 2),
#                             hidden = rep(1, 2), table = rep(tabel, 2), section = rep(NA, 2),
#                             Comentarii = rep(NA, 2))
#         excel <- rbind(excel[seq(1, linie - 1), ], temp, excel[seq(linie, nrow(excel)), ])
#         codeBook$dataDscr$m1s2_2gn <<- codeBook$dataDscr$m1s2_2an
#         codeBook$dataDscr$m1s2_2gs <<- codeBook$dataDscr$m1s2_2as



#         # la tabelul 27 introduc m1s4_5a2c
#         tabel <- 27
#         linie <- which(excel$table == tabel)[1]
#         temp <- data.frame(order = 1, question = "m1s4_5b", id = "m1s4_5b1c", type = "checkbox",
#                             active = '', auto = NA, hidden = 1, table = tabel, section = NA, Comentarii = NA)
#         excel <- rbind(excel[seq(1, linie - 1), ], temp, excel[seq(linie, nrow(excel)), ])
#         codeBook$dataDscr$m1s4_5b1c <<- codeBook$dataDscr$m1s4_5b2c

#         # la tabelul 28 introduc m1s4_8a2c
#         tabel <- 28
#         linie <- which(excel$table == tabel)[1]
#         temp <- data.frame(order = 1, question = "m1s4_8b", id = "m1s4_8a2c", type = "checkbox",
#                             active = '', auto = NA, hidden = 1, table = tabel, section = NA, Comentarii = NA)
#         excel <- rbind(excel[seq(1, linie - 1), ], temp, excel[seq(linie, nrow(excel)), ])
#         codeBook$dataDscr$m1s4_8a2c <<- codeBook$dataDscr$m1s4_8b2c

#         todelete <- which(is.element(excel$id, c("m2s4_2dacare", "m2s4_3iacare")) | grepl("file", excel$id))
#         codeBook$dataDscr$m2s4_2dacare <<- NULL
#         codeBook$dataDscr$m2s4_3iacare <<- NULL

#     }

#     if (length(todelete) > 0) {
#         excel <- excel[-todelete, ]
#     }
#     excel$order <- seq(nrow(excel))
#     return(excel)
# }


# makeHTML <- function(excel, sheet = "", codeBook, html) {

#     header <- c(
#         '<!DOCTYPE html>',
#         '<html>',
#         '',
#         '',
#         '<head>',
#         '    <meta charset="UTF-8">',
#         '    <title>Chestionar ANP</title>',
#         '    <meta http-equiv="Content-Security-Policy" content="script-src \'self\' \'unsafe-inline\';" />',
#         '    <link rel="stylesheet" href="./bootstrap.min.css">',
#         '    <link rel="stylesheet" href="main-style.css">',
#         '    <link rel="stylesheet" href="widths.css">',
#         '    <style scoped>',
#         '        tr.border_bottom td {',
#         '            border-bottom: 1px solid #999999;',
#         '        }',
#         '    </style>',
#         '</head>',
#         '',
#         '<body>',
#         '    <div class="container">',
#         '        <form>',
#         '            <!--- SECTIUNE _1_--->',
#         '')

#         footer <- c(
#         '        </form>',
#         '    </div>',
#         '</body>',
#         '</html>'
#     )

#     donexit <- function() {
#         # cat(paste(footer, collapse = "\n"))
#         suppressWarnings(sink())
#     }

#     # on.exit(donexit())
#     on.exit(suppressWarnings(sink()))

#     if (identical(sheet, "")) {
#         sheet <- 1
#     }

#     if (is.character(excel)) {
#         excel <- as.data.frame(readxl::read_excel(excel, sheet = sheet))
#     }

#     excel$table[is.na(excel$table)] <- 0
#     excel$hidden[is.na(excel$hidden)] <- 0
#     if (is.character(sheet)) {
#         excel <- treatExcel(excel, type = sheet)
#     }


#     excel$id <- admisc::trimstr(excel$id)
#     excel$id <- tolower(excel$id)


#     sectiuni <- grepl("Standard", codeBook$sections)
#     codeBook$sections[!sectiuni] <- toupper(codeBook$sections[!sectiuni])

#     cb <- codeBook$dataDscr
#     # cb <- cb[match(names(cb), excel$id)]





#     tbl <- excel[excel$table != 0, ]
#     tbls <- unique(tbl$table)


#     clist <- unlist(lapply(cb, function(x) return(x$question)))
#     tlist <- vector(mode = "list", length = length(tbls))
#     names(tlist) <- tbls

#     for (i in seq(length(tbls))) {
#         tlist[[i]] <- tbl$id[tbl$table == tbls[i]]
#     }

#     qlist <- vector(mode = "list", length = length(codeBook$questions))
#     names(qlist) <- names(codeBook$questions)

#     for (i in seq(length(qlist))) {
#         qlist[[i]] <- names(clist)[clist == names(qlist)[i]]
#     }

#     # if (!identical(excel$id, names(cb))) {
#     #     cat("\n")
#     #     stop(simpleError("ID-urile variabilelor din Excel nu se potrivesc cu cele din codeBook.\n\n"))
#     # }


#     i <- 1

#     obj <- c()

#     sectiune <- 0

#     while(i <= nrow(excel)) {
#         secprinted <- FALSE
#         sectiune <- sectiune + !is.null(cb[[excel$id[i]]]$section)

#         if (!is.null(cb[[excel$id[i]]]$module)) {
#             if (sectiune > 1) {
#                 secprinted <- TRUE
#                 obj <- c(obj, sprintf('            <!--- SECTIUNE _%s_--->', sectiune))
#             }

#             obj <- c(obj, sprintf('            <h5 class="my-5 bg-dark text-light py-2 pl-3">%s</h5>\n', codeBook$modules[cb[[i]]$module]))
#         }

#         if (!is.null(cb[[excel$id[i]]]$section)) {
#             section <- codeBook$sections[cb[[excel$id[i]]]$section]
#             # if (grepl("Standard", section)) {

#                 if (!secprinted & sectiune > 1) {
#                     obj <- c(obj, sprintf('            <!--- SECTIUNE _%s_--->', sectiune))
#                 }
#                 obj <- c(obj, sprintf('            <h5 class="my-5 bg-blue text-light py-2 pl-3">%s</h5>\n', section))

#             # }
#             # else {
#             #     obj <- c(obj, sprintf('            <h5 class="my-5 py-2 pl-3 "><span style="border-bottom: 2px solid #445569;"><span class="text-light" style="background-color:#445569; padding: 2px">%s.</span> %s</span></h5>\n', cb[[i]]$section, section))
#             # }
#         }



#         # if (i < 15) {
#         #     cat(paste(obj, collapse = "\n"))
#         #     cat("\n-----------------------------------------------------\n\n")
#         # }

#         if (excel$table[i] != 0) {
#             tbl <- as.character(excel$table[i])
#             vars <- tlist[[tbl]]
#             # if (is.element(tbl, names(codeBook$tables))) {
#                 # print(tbl)
#                 # if (tbl == '12') {
#                 #     return(is.element(vars, names(cb)))
#                 #     return(list(codeBook$tables[tbl], cb[vars], excel[which(is.element(excel$id, vars)), ]))
#                 #     print(makeTable(codeBook$tables[tbl], cb[vars], excel[which(is.element(excel$id, vars)), ]))
#                 # }
#                 obj <- c(obj, makeTable(codeBook$tables[tbl], cb[vars], excel[which(is.element(excel$id, vars)), ]))
#             # }

#             i <- i + length(vars)
#         }
#         else {
#             # print(excel$id[i])
#             # print(cb[[i]]$labels)
#             if (excel$hidden[i] != 0) {
#                 obj <- c(obj, sprintf('            <input type="hidden" class="form-control" id="%s" name="%s">', excel$id[i], excel$id[i]))
#             }
#             else {
#                 values <- cb[[excel$id[i]]]$values
#                 values <- values[values >= 0]

#                 widths <- c(7, 5)
#                 if (excel$type[i] == "textarea") {
#                     widths <- c(12, 12)
#                 }

#                 # print(i)
#                 # print(excel$id[i])
#                 # print(codeBook$questions[[cb[[excel$id[i]]]$question]])

#                 obj <- c(obj,
#                         makeQdiv(
#                             widths,
#                             excel$id[i],
#                             paste(codeBook$questions[[cb[[excel$id[i]]]$question]], collapse = " "),
#                             excel$type[i],
#                             values,
#                             !is.na(excel$auto[i])
#                         ))
#             }

#             i <- i + 1
#         }
#     }

#     sink(html)
#     cat(paste(header, collapse = "\n"))
#     cat(paste(obj, collapse = "\n"))
#     # cat("<p><br /><br /><br /><br /><br /></p>")
#     cat(paste(footer, collapse = "\n"))
#     sink()

# }


# makeTable <- function(tbl, cb, excel) {
#     # tbl is the definition from the object "tables"
#     # cb is the relevant subset from the object codeBook$dataDscr
#     # excel contains the specific lines from the Excel
#     tblname <- names(tbl)
#     tbl <- tbl[[1]]
#     # print(tblname)

#     if (!admisc::wholeNumeric(nrow(excel) / tbl$colvars)) {
#         cat("\n")
#         stop(simpleError(sprintf("Tabel cu variabile insuficiente: %s.\n\n", tblname)))
#     }

#     rows <- tbl$rows
#     qtmp <- c()

#     if (identical(rows, "questions")) {
#         rowscontent <- unlist(lapply(cb, function(x) {
#             return(sprintf("<strong>%s</strong>", paste(codeBook$questions[[x$question]], collapse = " ")))
#         }))
#     }
#     else {
#         if (identical(rows, "options")) {
#             rowscontent <- unlist(lapply(cb, function(x) {
#                 return(paste(x$option, collapse = " "))
#             }))
#         }

#         if (!tbl$hasq) {
#             qnms <- unlist(unique(lapply(cb[seq(tbl$colvars)], function(x) {
#                 return(x$question)
#             })))

#             if (length(qnms) > 0) {
#                 questions <- unique(lapply(cb[seq(tbl$colvars)], function(x) {
#                     if (!is.null(x$question)) {
#                         return(paste(codeBook$questions[[x$question]], collapse = " "))
#                     }
#                 }))

#                 names(questions) <- qnms

#                 instructions <- c()
#                 for (i in seq(tbl$colvars)) {
#                     if (!is.null(cb[[i]]$instruction)) {
#                         instructions <- c(instructions, paste(codeBook$instructions[[cb[[i]]$instruction]], collapse = " ") )
#                         names(instructions)[length(instructions)] <- cb[[i]]$question
#                     }
#                 }

#                 if (any(duplicated(names(instructions)))) {
#                     instructions <- instructions[-which(duplicated(names(instructions)))]
#                 }


#                 for (i in seq(length(questions))) {
#                     qi <- ''
#                     if (is.element(qnms[i], names(instructions))) {
#                         qi <- sprintf('\n                <br>\n                <span style="color:red">%s</span>', instructions[qnms[i]])
#                     }
#                     qtmp <- c(qtmp, sprintf('            <p class="mt-4"><strong>%s</strong>%s</p>', questions[[i]], qi))
#                 }
#                 qtmp <- c(qtmp, "\n")
#             }
#         }
#     }


#     vars <- names(cb)

#     td <- '                    <td%s>%s</td>'
#     tmp <- c()

#     # if (tblname == '41') {
#     #     print(qnms)
#     #     print(instructions)
#     #     cat(paste(tmp, collapse = '\n'))
#     # }

#     i <- 1
#     firstw <- TRUE

#     while (i <= length(cb)) {

#         wdth <- ''
#         tmprow <- '                <tr>'

#         if (excel$hidden[i] & grepl("uat", excel$id[i])) {
#             tmprow <- c(tmprow, sprintf('            <input type="hidden" class="form-control" id="%s" name="%s">\n', excel$id[i], excel$id[i]))
#             i <- i + 1
#         }
#         else {

#             # check if columns from the table header have class="w- something
#             widths <- sum(grepl("=\"w-", tbl$header)) == 0 & firstw & tbl$hasq
#             w <- 1
#             wdth <- ''
#             if (!is.null(rows)) {
#                 if (widths) {
#                     wdth <- sprintf(' class="w-%s"', tbl$widths[w])
#                     w <- w + 1
#                 }
#                 tmprow <- c(tmprow, sprintf(td, ifelse(widths, wdth, ''), rowscontent[i]))
#             }

#             for (j in seq(tbl$colvars)) {
#                 # cat(sprintf("i: %s; j: %s\n", i, j))
#                 values <- cb[[i]]$values
#                 values <- values[values >= 0]

#                 if (excel$type[i] == "checkbox") {
#                     specific <- ifelse(excel$hidden[i] == 1, '', sprintf('<input type="checkbox" id="%s" name="%s">', vars[i], vars[i]))

#                     class <- ' class="%stext-center"'
#                     wdth <- ''
#                     if (widths) {
#                         wdth <- sprintf('w-%s ', tbl$widths[w])
#                         w <- w + 1
#                     }

#                     tmprow <- c(tmprow, sprintf(td, sprintf(class, wdth), specific))
#                 }
#                 else if (excel$type[i] == "radio") {
#                     for (val in values) {
#                         if (widths) {
#                             wdth <- sprintf('w-%s ', tbl$widths[w])
#                             w <- w + 1
#                         }

#                         tmprow <- c(tmprow,
#                             sprintf('                    <td%s>', sprintf(' class="%stext-center"', ifelse(widths, wdth, ''))),
#                                     '                        <div class="custom-control custom-radio">',
#                             sprintf('                            <input type="radio" id="%s_%s" name="%s" value="%s" class="custom-control-input">', vars[i], val, vars[i], val),
#                             sprintf('                            <label class="custom-control-label" for="%s_%s"></label>', vars[i], val),
#                                     '                        </div>',
#                                     '                    </td>'
#                         )
#                     }
#                 }
#                 else if (is.element(excel$type[i], c("input", "number", "double"))) {
#                     specific <- ifelse(excel$hidden[i] == 1, '', sprintf('<input type="text" class="form-control" id="%s" name="%s">', vars[i], vars[i]))

#                     if (widths) {
#                         wdth <- sprintf(' class="w-%s"', tbl$widths[w])
#                         w <- w + 1
#                     }
#                     tmprow <- c(tmprow, sprintf(td, ifelse(widths, wdth, ''), specific))
#                 }
#                 else if (is.element(excel$type[i], c("textarea"))) {
#                     specific <- ifelse(excel$hidden[i] == 1, '', sprintf('<textarea class="form-control" id="%s" name="%s"></textarea>', vars[i], vars[i]))

#                     if (widths) {
#                         wdth <- sprintf(' class="w-%s"', tbl$widths[w])
#                         w <- w + 1
#                     }
#                     tmprow <- c(tmprow, sprintf(td, ifelse(widths, wdth, ''), specific))
#                 }
#                 else if (excel$type[i] == "select") {
#                     specific <- ifelse(excel$hidden[i] == 1, '', sprintf('<select class="form-control" id="%s" name="%s"></select>', vars[i], vars[i]))
#                     if (widths) {
#                         wdth <- sprintf(' class="w-%s"', tbl$widths[w])
#                         w <- w + 1
#                     }
#                     tmprow <- c(tmprow, sprintf(td, ifelse(widths, wdth, ''), specific))
#                 }

#                 i <- i + 1
#             }

#             if (is.element("rowscol", names(tbl)) & length(tmprow) > 2) {
#                 tmptr <- tmprow[2]
#                 tmprow[2] <- tmprow[3]
#                 tmprow[3] <- tmptr
#             }

#             tmp <- c(tmp, tmprow,
#                 '                </tr>'
#             )
#         }

#         firstw <- FALSE
#     }

#     return(c(qtmp, '            <table class="table table-hover mt-4">', tbl$header, tmp, '            </table>', '\n'))
# }


# makeQdiv <- function(width, id, text, type, values, auto) {


#     div <- c(
#         '            <div class="form-row mt-4">',
#         sprintf('                <div class="col-%s">', width[1])
#     )

#     if (is.element(type, c("input", "number", "numeric", "double"))) {
#         # print(list(id, text))
#         div <- c(div,
#             sprintf('                    <label for="%s" class="col-form-label"><strong>%s</strong></label>', id, text),
#             '                </div>',
#             sprintf('                <div class="col-%s">', width[2]),
#             sprintf('                    <input type="text" %sclass="form-control" id="%s" name="%s">', ifelse(auto, 'readonly ', ''), id, id)
#         )
#     }
#     else if (type == "textarea") {
#         div <- c(div,
#             sprintf('                    <label for="%s" class="col-form-label"><strong>%s</strong></label>', id, text),
#             '                </div>',
#             sprintf('                <div class="col-%s">', width[2]),
#             sprintf('                    <textarea %sclass="form-control" id="%s" name="%s"></textarea>', ifelse(auto, 'readonly ', ''), id, id)
#         )
#     }
#     else if (type == "select") {
#         div <- c(div,
#             sprintf('                    <label for="%s" class="col-form-label"><strong>%s</strong></label>', id, text),
#             '                </div>',
#             sprintf('                <div class="col-%s">', width[2]),
#             sprintf('                    <select %sclass="form-control" id="%s" name="%s"></select>', ifelse(auto, 'readonly ', ''), id, id)
#         )
#     }
#     else if (type == "radio") {
#         div <- c(div,
#             sprintf('                    <p><strong>%s</strong></p>', text),
#             '                </div>',
#             sprintf('                <div class="col-%s">', width[2])
#         )

#         for (i in seq(length(values))) {
#             div <- c(div,
#                 '                    <div class="custom-control custom-radio custom-control-inline">',
#                 sprintf('                        <input type="radio" id="%s" name="%s" class="custom-control-input" value="%s">', paste(id, values[i], sep = "_"), id, values[i]),
#                 sprintf('                        <label class="custom-control-label" for="%s">%s</label>', paste(id, values[i], sep = "_"), names(values)[i]),
#                 '                    </div>'
#             )
#         }

#     }

#     div <- c(div,
#         '                </div>',
#         '            </div>',
#         '',
#         ''
#     )

#     return(div)
# }














# #
