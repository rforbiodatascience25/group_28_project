render_qmd <- function(qmd_file, 
                       output_path="../results", 
                       output_format="html") {
  #'Description: 
  #'  Helper function for 00_all.qmd and does the following:
  #'  1. Deletes render files/folders lost in /R 
  #'  2. Runs and renders a .qmd files at location run.
  #'  3. It moves the rendered file to output_path, 
  #'  4. converts .svg files to .png at the output location 
  #'  5. deletes .svg files at at output location
  #'  
  #'Arguments: 
  #'  qmd_file = file name string
  #'  output_path = the file path for the output location
  #'  output_format = the render format for the file. 
  #'  
  #'Example 1: 
  #'  render_qmd("01_load_data.qmd")
  #'  
  #'Example 2: 
  #'  render_qmd_list <- c(
  #'    "01_load_data.qmd",
  #'    "02_clean_data.qmd",
  #'    "03_aug_data.qmd")
  #'  
  #'  render_qmd_list |>  
  #'    lapply(render_qmd) |> 
  #'    invisible ()
  
  #check which png file exist before rendering
  png_list_before <- list.files(output_path, pattern = "\\.png$", full.names = TRUE) 
  
  # Create Results and Doc folder if they don't exist/not backed on git
  dir.create(file.path("..", "results"),  recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path("..", "doc"),  recursive = TRUE, showWarnings = FALSE)
  
  # Cleanup disrupted renderings
  #--------------------------------------------------
  # list all files at function call location
  all_paths <- list.files(".", full.names = TRUE, recursive = FALSE) 
  
  # files for deletion (prefix and suffix condition)
  is_render_file <- grepl("^[0-9]{2}_.+\\.(pdf|html|rmarkdown)$",basename(all_paths))
  delete_file_paths <- all_paths[is_render_file]
  
  # folders for deletion (prefix, suffix and state condition)
  is_folder <- file.info(all_paths)$isdir                           
  is_render_folder <- grepl("^[0-9]{2}_.+_files$", basename(all_paths)) 
  delete_folder_paths <- all_paths[is_folder & is_render_folder]
  
  #number of files and folders for deletion
  n_delete_folder_paths <- length(delete_folder_paths)
  n_delete_file_paths <- length(delete_file_paths)
  
  # print delete message
  if (n_delete_file_paths  > 0 || n_delete_folder_paths > 0) {
    unlink(c(delete_file_paths, delete_folder_paths), recursive = TRUE)
    n_deleted <- n_delete_file_paths + n_delete_folder_paths  # first count deleted files
    cat(" Failed renderings detected: ", n_deleted,"\n",
        "\n",sep = "")
    if (delete_file_paths > 0){
      cat("Files deleted:", "\n",
          paste(" -", basename(delete_file_paths), collapse = "\n"), "\n",
          "\n",sep = "")
    }
    if (delete_folder_paths > 0) {
      cat("Folders deleted:", "\n",
          paste(" -", basename(delete_folder_paths), collapse = "\n"), "\n",
          sep = "")
    }
  }

  # run/render .qmd file
  #--------------------------------------
  # run/render at location
  quarto::quarto_render(
    input = qmd_file,  # file in the R/ folder
    output_format = output_format,
  )
  
  # move html renders from "/R" to "/results"
  html_name <- sub("\\.qmd$", paste0(".",output_format), qmd_file) # define html name
  
  file.rename(
    from = html_name,
    to = file.path(output_path, html_name)
  )
  
  #add number prefix to png 
  #------------------------------------------------
  # list new png files
  png_list_after <- list.files(output_path, pattern = "\\.png$", full.names = TRUE) 
  png_list_diff  <- setdiff(png_list_after, png_list_before)
  
  # if no new png files were created
  if (length(png_list_diff) == 0) {
    cat("Rendered", qmd_file, "and 0 png files in", output_path, "\n")
    return(invisible())
  }
  
  #extract prefix
  qmd_prefix <- sub("_.*$", "", basename(qmd_file))  
  
  #create png base name with prefix
  Base_prefix_png_list_diff <- paste0(qmd_prefix, "_", basename(png_list_diff))
  prefix_png_list_diff <- png_list_diff |>
    dirname() |>
    file.path(Base_prefix_png_list_diff)
  
  #rename png files
  file.rename(png_list_diff, prefix_png_list_diff)

  # status message.
  cat(" ----------------------------------------------------------------\n",
    "Rendered", qmd_file, "and", length(png_list_diff), "png files in", output_path, "\n",
    "----------------------------------------------------------------",  "\n\n")
  
  # check for unnamed png files 
  #-----------------------------------------------------
  
  # check for unnamed png files (created from unlabeled chunks)
  unnamed_idx  <- grepl("^[0-9]{2}_+unnamed-chunk", basename(prefix_png_list_diff))
  unnamed_pngs <- prefix_png_list_diff[unnamed_idx]
  
  # print png name status
  if (length(unnamed_pngs) > 0) {
    cat(length(unnamed_pngs), " unnamed file(s) detected\n\n",
      "png file fors:\n",
      paste(" -", basename(unnamed_pngs), collapse = "\n"), "\n\n",
      "Created from .qmd document:\n",
      " - ", qmd_file, "\n\n",
      "Remember to name labels on ALL chunks with '#| label: ...'\n",
      sep = ""
    )
  }
}



