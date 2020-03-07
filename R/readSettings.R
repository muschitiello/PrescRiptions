##' readSettings
##'
##' A settings file allows a user of the package to make changes to the parameters of
##' a script without changing the script itself.#'
##'
##' @param yamlF character. Path of the settings file.
##'
##' @import data.table
##' @import feather
##' @importFrom stringr str_pad
##' @import yaml
##'
##' @export

readSettings <- function(yamlF = NULL){

  if(!file.exists(yamlF)){
    stop(sprintf(paste("The settings file, %1$s, doesn't exist. Maybe you should",
    "create one. If there's a %1$s.example file, copy that to %1$s. If not, you",
    "can create a sample one using Addsettings()"), file))
  }

  raw_yaml <- yaml::yaml.load_file(yamlF)

  raw_yaml

}
