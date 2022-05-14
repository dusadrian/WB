hideFiles <- function(gitdir = NULL, project = NULL) {

    if (Sys.info()[["sysname"]] != "Windows") {
        stop("Numai pentru Windows...")
    }

    if (is.null(project)) {
        stop("Numele proiectului lipseste.")
    }

    if (is.null(gitdir)) {
        gitdir <- "C:/Users/adrian/Documents/GitHub"
    }

    if (!file.exists(file.path(gitdir, project, "build"))) {
        stop("Directorul de build nu exista.")
    }

    if (!file.exists(file.path(gitdir, project, "build", "output"))) {
        stop("Nu am gasit niciun build, directorul output nu exista.")
    }

    if (!file.exists(file.path(gitdir, project, "build", "output", "win-unpacked"))) {
        stop("Directorul win-unpacked nu exista.")
    }

    win32 <- file.exists(file.path(gitdir, project, "build", "output", "win-ia32-unpacked"))

    dir <- file.path(
        gitdir, project, "build", "output", "win-unpacked"
    )
    
    files <- setdiff(
        list.files(dir),
        c(
            "WorldBankTool.exe",
            "WorldBankToolMain.exe",
            "StatConverter.exe",
            "resources",
            "LICENSES.chromium.html",
            "LICENSE.electron.txt"
        )
    )

    
    for (i in seq(length(files))) {
        system(paste("attrib +h", file.path(dir, files[i])))
    }

    if (file.exists(file.path(dir, "resources", "assets"))) {
        system(paste("attrib +h", file.path(dir, "resources", "assets")))
    }

    if (project != "StatConverter") {
        system(paste("attrib +h", file.path(dir, "resources", "app.asar")))
    }
    
    if (file.exists(file.path(dir, "resources", "page"))) {
        system(paste("attrib +h", file.path(dir, "resources", "page")))
    }
    
    if (file.exists(file.path(dir, "resources", "R_Portable"))) {
        system(paste("attrib +h", file.path(dir, "resources", "R_Portable")))
        system(paste("attrib +h", file.path(dir, "resources", "startServer.R")))
        system(paste("attrib +h", file.path(dir, "resources", "elevate.exe")))
    }

    if (win32) {
        dir <- file.path(
            gitdir, project, "build", "output", "win-ia32-unpacked"
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
        if (file.exists(file.path(dir, "resources", "page"))) {
            system(paste("attrib +h", file.path(dir, "resources", "page")))
        }
    }
}
