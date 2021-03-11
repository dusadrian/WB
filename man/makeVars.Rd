\name{makeVars}

\alias{makeVars}

\title{Creaza fisierul .js pentru variabile}

\description{
Fisierul js este un modul exportat care contine componentele questions, questionsOrder si activators.
}

\usage{
makeVars(excel, sheet = "", js, newstyle = TRUE, sat = FALSE, headers = TRUE, ...)
}

\arguments{
  \item{excel}{Calea catre fisierul .ods de unde se citesc informatiile}
  \item{sheet}{Numele foii care sa fie citita}
  \item{js}{Calea catre fisierul .js unde se scriu informatiile}
  \item{newstyle}{Pentru activatori}
  \item{sat}{Adauga si o componenta cu toate variabilele care contin "sat" in nume}
  \item{headers}{Componenta de exportHeader}
  \item{...}{Alte argumente}
}


\author{
Adrian Dusa
}

\keyword{functions}
