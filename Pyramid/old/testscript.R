
# Author: triffe
###############################################################################
# for quick conversion
#install.packages("Rd2roxygen")
#library(Rd2roxygen)
#parse_and_save("/home/triffe/git/Pyramid/Pyramid/man/ESextr.Rd",
#        file = "/home/triffe/git/Pyramid/Pyramid/old/ESextr")
#parse_and_save("/home/triffe/git/Pyramid/Pyramid/man/PTpop.Rd",
#        file = "/home/triffe/git/Pyramid/Pyramid/old/PTpop")
#parse_and_save("/home/triffe/git/Pyramid/Pyramid/man/MyersI.Rd",
#        file = "/home/triffe/git/Pyramid/Pyramid/old/MyersI")
#parse_and_save("/home/triffe/git/Pyramid/Pyramid/man/Pyramid-package.Rd",
#        file = "/home/triffe/git/Pyramid/Pyramid/old/Pyramid-package")
# this script is for test package building a light testing
5*65000
devtools::build("/home/triffe/git/Pyramid/Pyramid","/home/triffe/git/Pyramid/Pyramid")

BuildPackage <- function(RepoPath = "/home/triffe/git/Pyramid", PackageName = "Pyramid",
        MajorInc = 2, origin.date = "2010-01-01"){
    # first update documentation
    devtools::document(file.path(RepoPath,PackageName))
    
    # now update DESCRIPTION
    # get time difference from origin
    increment <-  as.numeric(
            Sys.time() - 
                    as.POSIXct(as.Date(origin.date), 
                            origin = ISOdatetime(1960,1,1,0,0,0),tz="PST")
    )
    
    # determine new version number:
    pkg.vs    <- paste0("Version: ",MajorInc, ".", as.character(round(increment / 365.25, digits = 4)))
    
    # update version in DESCRIPTION file:
    DESC      <- readLines(file.path(RepoPath, PackageName,"DESCRIPTION"))
    Version.i <- grep(DESC, pattern = "Version:")
    DESC[Version.i] <- pkg.vs
    writeLines(DESC, file.path(RepoPath, PackageName,"DESCRIPTION"))

    # build
    devtools::build(pkg = file.path(RepoPath, PackageName), path = RepoPath)
}
BuildPackage(RepoPath = "/home/triffe/git/Pyramid", PackageName = "Pyramid", MajorInc = 2)
