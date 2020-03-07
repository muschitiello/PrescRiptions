#' Add settings file to project
#'
#' @description  Creates a sample settings file, by default (leading dashes for
#'   formatting, whitespace in the actual file):
#'
#' year: [year] \cr
#' month: [month] \cr
#' jarfolder: "D:/Documents/DriversCertificates" \cr
#' personeFisiche: FALSE  \cr
#' stkv: \cr
#' -- server: "mydwpr02.intra.infocamere.it" \cr
#' -- port: "3306" \cr
#' -- schema: "stkv" \cr
#' -- user: "[your_user]" \cr
#' -- pw: "[your_password]" \cr
#' aSoci: \cr
#' -- server: "mydwpr01.intra.infocamere.it" \cr
#' -- port: "3306" \cr
#' -- schema: "infm" \cr
#' -- user: "[your_user]" \cr
#' -- pw: "[your_password]" \cr
#' oracle: \cr
#' -- server: "oraricl.intra.infocamere.it" \cr
#' -- schema: "batchri.WORLD" \cr
#' -- user: "[your_user]" \cr
#' -- pw: "[your_password]" \cr
#' dvsu: \cr
#' -- server: "mydwpr01.intra.infocamere.it" \cr
#' -- port: "3306" \cr
#' -- schema: "dvsu" \cr
#' -- user: "[your_user]" \cr
#' -- pw: "[your_password]" \cr
#'
#' @param dir character. Directory in which to write settings files
#' @param filename character. Name of settings file to write
#' @param gitignore logical. Should this file be added to the .gitignore?
#' @param fields list. Manually specify the structure of the settings file
#'
#' @import yaml
#' @export

addSettings <- function(dir = ".", filename = "pesi.yml", gitignore = TRUE, fields=NULL){
  
  file <- file.path(dir, filename)
  
  # Basic settings file
  sample_yml <- file.path(path.package("icAnagraficaPesi"), "sample_pesi.yml")
  yaml_list <- readChar(sample_yml, file.info(sample_yml)$size)
  
  # Overwrite the basic with a new one if it is provided
  if(!is.null(fields)){
    yaml_list <- as.yaml(fields)
  }
  
  examplefile <- paste0(file, ".example")
  
  if(all(file.exists(file, examplefile))){
    # If files already exists, don't write them
    missing <- message(sprintf("Settings files %s already exist. Not overwriting them.",
                               paste0(c(file, examplefile), collapse = ", ")
    )
    )
  } else if(xor(file.exists(examplefile),file.exists(file))){
    # If one file exists and not the other, copy it
    if(file.exists(examplefile) && !file.exists(file))  {
      message(sprintf("Example file %s exists, settings file doesn't. Creating settings file %s",
                      examplefile, file))
      file.copy(examplefile, file)
    } else {
      message(sprintf("Every settings file should have an example file. Creating example file %s",
                      examplefile))
      file.copy(file, examplefile)
    }
    
  } else {
    # If neither exist, create them
    message(sprintf("Neither settings nor example file exist. Creating %s and %s", file, examplefile))
    writeLines(yaml_list, file)
    writeLines(yaml_list, examplefile)
  }
  
  
  # Add to .gitignore
  if(gitignore){
    if(file.exists(".gitignore")){
      no_newline <- FALSE
      parsed_gitignore <- withCallingHandlers(readLines(".gitignore"),
                                              warning = function(w){
                                                if(grepl("incomplete final line found on", w$message)){
                                                  no_newline <<- TRUE
                                                }
                                              })
      
      if(!(normalizePath(file) %in% normalizePath(parsed_gitignore, mustWork = FALSE))){
        ignore_connection <- file(".gitignore", "a")
        on.exit(close(ignore_connection))
        writeLines(paste0("\n"[no_newline], file), ignore_connection)
        message(sprintf("Filename %s added to .gitignore", file))
      } else {
        message(sprintf("Filename %s already in .gitignore, not re-added", file))
      }
      
    }
  }
  
  message(sprintf("[SUCCESS] Settings files prepared"))
}
