#' load_config
#'
#' Load basic configuration variables, such as file paths, to be used with
#' various functions in chonyepicdata
#'
#' @param yml_path A character vector path to the configuration file. Default is config/config.yml
#' @param useglobal A boolean, default to FALSE. Set to TRUE if you want to automatically load configuration variables into the global environment. Otherwise these will be returned as a list.
#'
#' @return Returns nothing if useglobal=TRUE. Otherwise returns a list containing configuration variables.
#' These can run using \code{list2env(config, globalenv())}
#' @export
load_config <- function(yml_path = 'inst/config/config.yml',
                        useglobal = FALSE) {

     if(useglobal){
          list2env(read_yaml(yml_path), globalenv())
     }
     else{
          return(yaml::read_yaml(yml_path))
     }
}
