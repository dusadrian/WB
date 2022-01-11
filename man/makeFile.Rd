\name{makeFile}

\alias{makeFile}

\title{Descarca datele in formate de analiza}

\description{
Produce un fisier SPSS ori Stata pentru analiza statistica a datelor.
}

\usage{
makeFile(
    instrument = NULL, type = NULL, partial = FALSE, destination = "~",
    empty = TRUE, diacritice = FALSE, host = "127.0.0.1", port = NULL,
    dbname = "", user = "", password = "")
}

\arguments{
  \item{instrument}{ID-ul instrumentului}
  \item{type}{SPSS sau Stata}
  \item{partial}{Descarca doar chestionarele complete sau (si) pe cele partiale}
  \item{destination}{Directorul destinatie unde sa fie salvat fisierul}
  \item{empty}{Elimina coloanele complet goale}
  \item{diacritice}{Pastreaza (TRUE) / Elimina (FALSE) diacriticele din SPSS}
  \item{host}{Adresa IP a serveriului MySQL}
  \item{port}{Portul, daca e nevoie}
  \item{dbname}{Numele bazei de date}
  \item{user}{Numele utilizatorului de MySQL}
  \item{password}{Parola de MySQL a utilizatorului}
}


\author{
Adrian Dusa
}

\keyword{functions}
