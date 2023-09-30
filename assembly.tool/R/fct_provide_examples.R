################################################################################
# Description: Provides examples of control and run files
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
#
# Issues:
#
################################################################################

#' Provide an example of the use of the assembly tool
#' 
#' @param folder Folder to copy the example into. Defaults to current working
#' directory.
#' 
#' @return The path of the newly created folder (invisibly)
#' 
#' @export
provide_assembly_tool_example = function(folder = NA){
  if(is.na(folder)){
    folder = getwd()
  }
  
  stopifnot(is.character(folder))
  stopifnot(dir.exists(folder))
  
  # to location
  to_dir = file.path(normalizePath(folder), "demonstration_example")
  
  if(!dir.exists(to_dir)){
    dir.create(to_dir)
  }
  
  # from location
  example_folder = system.file("extdata", "demonstration_example", package = "assembly.tool")
  
  # copy
  file.copy(
    list.files(example_folder, full.names = TRUE), 
    to_dir, 
    recursive = TRUE
  )
  
  # conclude
  return(invisible(to_dir))
}
