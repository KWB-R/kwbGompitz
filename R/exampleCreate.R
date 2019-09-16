# Info -------------------------------------------------------------------------
#
# Provide different versions of example files against which the results
# of the kwb.rsproto functions can be tested.
#
# In the following we refer to the software package "gompitz2.08.zip" provided
# by Yves Le Gat. It contains the following sub-folders and files:
#
# [bin]
#   - gompcal
#   - gompred
#
# [example]
#   - calibr.txt
#   - iff.txt
#   - obs.txt
#   - param.txt
#   - predict[0-3].txt
#   - rehab[1-3].txt
#   - ST[0-3].txt
#
# [source]
#   - gompcal.c
#   - gompred.c
#
# [win32]
#   - gompcal.exe
#   - gompred.exe
#
# Version <unix>: Files contained in [example]. These files are most probably
#   the result of running the Unix executable files in [bin] on a Unix system.
#
# Version <win32>: Files created with the Windows executable files contained in
#   [win32].
#
# Version <win32_kwb>: Files created with the Windows executable files that were
#   recompiled at KWB from the source files in [source].
#

# The <unix> files are already contained in kwbGompitz, e.g.
# kwbGompitz::exampleFile("calibr.txt")

# The <win32> and <win32_kwb> files can be created using the functions in
# kwbGompitz

# createExampleFiles -----------------------------------------------------------

#' Create Example Files
#' 
#' Create Example Files Using Different Windows Executables. Run different
#' executables of gompcal and  gompred (as provided in subfolders 
#' "bin_<version>" with version in ["win32", "win32_kwb", "unix_home"]) on the 
#' example input file (obs.txt) and store the  results in folders 
#' "example_<version>".
#' 
#' @export
#' 
createExampleFiles <- function()
{
  #kwb.utils::assignPackageObjects("kwbGompitz")

  # Prepare (and open) temporary folder and copy strategy files into it

  testdir <- tempGompitzDir()
  files <- c("obs.txt", paste0("ST", 0:2, ".txt"))
  example_files_unix <- exampleFile(files, VERSION = "unix")
  file.copy(example_files_unix, testdir)
  #kwb.utils::hsOpenWindowsExplorer(testdir)

  os_type <- getOperatingSystemType()
  
  versions <- if (grepl("^unix", os_type)) {
    ifelse(os_type == "unix64", "unix64", "unix_home")
  } else {
    c("win32", "win32_kwb")
  }

  # Reduce to versions for which a folder "bin_<version>" is available
  bin_available <- dir(dirname(dirname(example_files_unix[1])), "^bin_")
  versions <- versions[paste0("bin_", versions) %in% bin_available]
  
  # Create all output files for the (different versions of) executable(s)
  for (version in versions) {

    message("\nCreating example output files for version ", version, "...\n")

    # Provide path to input file
    file <- kwb.utils::safePath(testdir, "obs.txt")

    # Run gompcal.exe
    runGompcalInDirectory(testdir, file, VERSION = version)

    # Now there are files calibr.txt and param.txt in the directory testdir
    
    # Run gompred.exe
    for (strategy in 0:2) {
      
      runGompredInDirectory(
        testdir, file, strategy = strategy, VERSION = version
      )
    }

    # Copy all files to "example_win32" or "example_win32_kwb" in the
    # installation folder of kwbGompitz
    file.copy(
      from = dir(testdir, "*.txt", full.names = TRUE),
      to = get_or_create_target_dir(version)
    )
  }

  # Remove files in testdir
  unlink(file.path(testdir, "*.*"))
}

# get_or_create_target_dir -----------------------------------------------------

#' Create target directory if required and return the path to it
#' 
#' @param version one of \code{c("unix", "unix_home", "win32", "win32_kwb")}
#' 
get_or_create_target_dir <- function(version)
{
  target_base <- system.file("extdata", package = "kwbGompitz")
  target_dir <- file.path(target_base, paste0("example_", version))
  
  kwb.utils::createDirectory(target_dir)
}

# createExampleFilesSmall ------------------------------------------------------

#' Create smaller versions of the example files
#' 
#' @param parts vector of integer determining the parts of the files to be
#' created: 2 = first half, 3 = first third, ..., 10 = first 10 percent, etc.
#' @param version one of c("unix", "unix_home", "win32", "win32_kwb")
#' @param types vector of file types to be created. See the default assignment
#' for possible items
#' @param targetdir full path to the target directory in which to put the files.
#' By default the files go into the package's example directory related to 
#' the selected \code{version}.
#' 
#' @examples
#' \dontrun{kwbGompitz:::createExampleFilesSmall(c(2, 4, 10))}
#' 
#' @export
#' 
createExampleFilesSmall <- function
(
  parts = c(2, 4, 8, 16), 
  version = c("unix", "unix_home", "win32", "win32_kwb")[
    ifelse(kwb.utils::.OStype() == "unix", 1, 3)
  ],
  types = c("obs", "predict0", "predict1", "predict2", "iff"),
  targetdir = NULL
)
{
  (filenames <- structure(paste0(types, ".txt"), names = types))
  (filepaths <- kwbGompitz::exampleFile(filenames, VERSION = version))

  # Read all file contents into a list of vectors of character
  contents <- lapply(filepaths, readLines)
  
  # How many header lines are to be skipped from each file of given type?
  (n_skip <- c(obs = 8, predict0 = 3, predict1 = 4, predict2 = 4, iff = 1))
  
  # Split each content into header and body and provide vectors of pipe IDs
  sections <- lapply(kwb.utils::toNamedList(types), function(type) {
    content <- contents[[type]]
    i_header <- seq_len(n_skip[[type]])
    list(
      header = content[i_header],
      body = content[- i_header],
      pipe_ids = if (type == "iff") {
        substr(content, 1, 8)
      } else {
        sapply(strsplit(content, ";"), "[", 2)
      }
    )
  })

  # If no target directory is given use the package directory
  (targetdir <- kwb.utils::defaultIfNULL(targetdir, dirname(filepaths[[1]])))
  
  cat("Writing example files to", targetdir, "...\n")

  # Define a helper function that writes a text file giving log messages
  write_file <- function(x, file) {
    cat("Writing", basename(file), "... ")
    writeLines(x, file)
    cat("ok.\n")
  }
  
  # For each part into which the files are to be divided and each file...
  for (part in parts) {

    included_ids <- NULL
    
    for (type in types) {

      # Provide the vector of pipe IDs available in the file of given type
      pipe_ids <- sections[[type]]$pipe_ids
      
      if (type == "obs") {
        
        # Find the row "i" representing the end of the first part of the file.
        # Correct i to the last line of the lines representing the same pipe
        (i <- round(1 / part * length(sections[[type]]$body)))
        (i <- max(which(pipe_ids[i] == pipe_ids)))
        indices <- seq_len(i)
        included_ids <- unique(pipe_ids[indices])

      } else {
        
        stopifnot(! is.null(included_ids))
        indices <- which(pipe_ids %in% included_ids)
      }

      # Write the small file containing only the indexed (body) lines
      write_file(
        x = c(sections[[type]]$header, sections[[type]]$body[indices]), 
        file = file.path(targetdir, sprintf("%s_partial_%02d.txt", type, part))
      )
    }
  }
}
