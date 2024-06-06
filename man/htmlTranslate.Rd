\name{htmlTranslate}

\alias{htmlTranslate}

\title{Creaza fisiere .html traduse}

\description{
Creaza fisiere .html cu toate elementele traduse si toate valorile inlocuite
}

\usage{
htmlTranslate(
    excel = "",
    sheet = "translations",
    languages = c(""),
    template = "",
    destination = "",
    ...
)
}

\arguments{
  \item{excel}{Calea catre fisierul .ods de unde se citesc informatiile}
  \item{sheet}{Numele foii care sa fie citita}
  \item{languages}{Numele coloanelor care contin limbile in care se traduc variabilele}
  \item{template}{Calea catre fisierul template}
  \item{destination}{Calea catre directorul unde se scriu fisierele traduse}
  \item{...}{Alte argumente}
}


\author{
Adrian Dusa
}

\keyword{functions}
