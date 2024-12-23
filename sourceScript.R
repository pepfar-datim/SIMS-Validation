directory_path <- "/Users/41250/Documents/GitHub/pepfar/datim-validation/Untitled/R"

# List file names in the directory
file_names <- list.files(path = directory_path, full.names = TRUE)

# Run source command for each file name with full directory path
for (file in file_names) {
  source(file)
}
