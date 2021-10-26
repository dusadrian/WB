hideFiles <- function(gitdir = NULL, project = NULL) {

    if (is.null(project)) {
        stop("Numele proiectului lipseste.")
    }
    
    dir <- file.path(
        ifelse(is.null(gitdir), "C:/Users/adrian/Documents/GitHub", gitdir),
        project, "build", "output", "win-unpacked"
    )

    files <- setdiff(
        list.files(dir),
        c("WorldBankTool.exe", "resources", "LICENSES.chromium.html", "LICENSE.electron.txt")
    )
    
    for (i in seq(length(files))) {
        system(paste("attrib +h", file.path(dir, files[i])))
    }

    system(paste("attrib +h", file.path(dir, "resources", "assets")))
    system(paste("attrib +h", file.path(dir, "resources", "app.asar")))
}
