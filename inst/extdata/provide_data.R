library(kwb.utils)

relative_path <- "kwbGompitz/inst/extdata"

from_dir <- safePath(
  "//medusa/miacso$/REvaluation/RPackages_extdata", relative_path
)

to_dir <- safePath(
  get_homedir(), "Documents/github-repos", relative_path
)

relative_paths <- dir(from_dir, recursive = TRUE)

# Create vector of full target paths
to_paths <- file.path(to_dir, relative_paths)

# Create all target directories
for (path in unique(dirname(to_paths))) {
  
  createDirectory(path)  
}

# Copy all source files to the target locations
kwb.file::copy_files_to_target_dir(
  from_paths = file.path(from_dir, relative_paths), 
  target_dir = to_dir, 
  target_files = relative_paths
)
