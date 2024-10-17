.onAttach <- function(libname, pkgname){

#  require(utils)

  desc <- utils::packageDescription("SSRA")
  d1 <- desc$Version
  nk <- paste0(rep(" ", 20 - nchar(d1)))

  packageStartupMessage("|----------------------------------------------|\n",
                          paste0("| ", desc$Package, " ", d1," (",desc$Date,")"), nk, "       |\n" ,
                        "|      Sakai Sequential Relation Analysis      |\n" ,
                        "|----------------------------------------------|\n",
                        "SSRA is a BETA software. Please report any bugs.")

}

version <- function(pkg = "SSRA") {

  lib <- dirname(system.file(package = pkg))
  desc <- utils::packageDescription(pkg)

  return(paste(desc$Package, desc$Version, desc$Date,lib))

}
