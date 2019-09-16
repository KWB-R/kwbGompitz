  error_code <- .runModuleInDirectory(

  # Check the output for an error message  
  filename <- kwb.utils::getAttribute(error_code, "stdout")
  
  file_stdout <- kwb.utils::safePath(target.dir, filename)
  
  lines_stdout <- readLines(file_stdout)
  
  # "No valid data rows - Gompred stops processing"
  if (length(grep("Gompred stops processing", lines_stdout))) {
    
    log_dir <- dirname(file_stdout)
    
    if (grepl("win", getOperatingSystemType())) {
      log_dir <- kwb.utils::windowsPath(log_dir)
    } 
    
    msg_stopped <- "Gompred stopped processing!"
    
    message(sprintf(
      "\n\n%s See '%s' in '%s':\n", msg_stopped, basename(file_stdout), log_dir
    ))
    
    writeLines(lines_stdout)
    
    stop_(msg_stopped, " See the output above for possible reasons.")
  }
  error_code <- kwb.utils::runInDirectory(
  if (show.error && error_code != 0) {
    stderr <- kwb.utils::getAttribute(error_code, "stderr")
    
      paste0("There was an error (error_code: ", error_code, ") given below. ",
    open_file_if_not_on_unix(file = input.file)

  # Return error code having files containing stdout and stderr as attributes 
  error_code
}

# open_file_if_not_on_unix -----------------------------------------------------
open_file_if_not_on_unix <- function(file)
{
  if (! grepl("^unix", getOperatingSystemType())) {
    
    kwb.utils::hsOpenWindowsExplorer(file)
  }
.runModule <- function (
  error_code <- system2(command, args, stdout = files[1], stderr = files[2])
  structure(error_code, stdout = files[1], stderr = files[2])