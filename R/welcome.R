.onAttach <- function(lib, pkg){
  packageStartupMessage("Package 'medfateutils' [ver. ",
                        utils::packageDescription("medfateutils",
                                                  fields="Version"),"]",
                        appendLF = TRUE)
}
