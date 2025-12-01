render_qmd <- function(qmd_file, 
                       output_path="../results", 
                       output_format="html") {
  #'Description: 
  #'  Helper function for 00_all.qmd. 
  #'  1. Creates results folder it doesn't exist
  #'  2. Deletes interrupted render files/folders in /R 
  #'  3. Runs and renders a .qmd files at call path.
  #'  4. moves rendered files to output_path, 
  #'  5. adds numbered prefix to png files 
  #'  6. checks if unnamed png files was created. 
  #'  
  #'Arguments: 
  #'  qmd_file = string file basename at call filepath (string or list of strings)
  #'  output_path = the file path for the output location
  #'  output_format = the render format for the file. 
  #'  
  #'Requirements:
  #' 1. All qmd documents must have below yaml header. 
  #'  ---
  #'  title: "04_describe_data"
  #'  format:
  #'    html:
  #'    embed-resources: true
  #'  knitr:
  #'    opts_chunk:
  #'    dev: "ragg_png"
  #'  fig.path: "../results/"
  #'  editor: visual
  #'  ---  
  #'  
  #' yaml header does the following: 
  #'  - Includes plots in html renderings.
  #'  - renders all plots/figures to fig.path location as png files
  #'    names them according to the block label. 
  #'  
  #'Example use:   
  #'  Example 1: 
  #'    render_qmd("01_load_data.qmd")
  #'  
  #'  Example 2: 
  #'    render_qmd_list <- c(
  #'      "01_load_data.qmd",
  #'      "02_clean_data.qmd",
  #'      "03_aug_data.qmd")
  #'  
  #'  render_qmd(render_qmd_list)
  #'-----------------------------
  
  # Check if list if given, iterate if true.
  #-----------------------------------------
  # flatten if list == True
  if (is.list(qmd_file)) {
    qmd_file <- unlist(qmd_file)
  }
  
  # recursive run 
  if (length(qmd_file) > 1) {
    lapply(qmd_file,
           render_qmd,
           output_path = output_path,
           output_format = output_format)
    return(invisible())
  }
  
  # Preparation
  #-------------
  # .png files before render 
  png_list_before <- list.files(output_path, pattern = "\\.png$", full.names = TRUE) 
  
  # check result/ exist
  dir.create(file.path("..", "results"),  recursive = TRUE, showWarnings = FALSE)
  
  # Cleanup disrupted renderings
  #-----------------------------
  # list all files at call
  all_paths <- list.files(".", full.names = TRUE, recursive = FALSE) 
  
  # identify mis-renders (prefix and suffix conditions)
  is_render_file <- grepl("^[0-9]{2}_.+\\.(pdf|html|rmarkdown)$",basename(all_paths))
  delete_file_paths <- all_paths[is_render_file]
  
  is_folder <- file.info(all_paths)$isdir                           
  is_render_folder <- grepl("^[0-9]{2}_.+_files$", basename(all_paths)) 
  delete_folder_paths <- all_paths[is_folder & is_render_folder]
  
  #number of files and folders for deletion
  n_delete_folder_paths <- length(delete_folder_paths)
  n_delete_file_paths <- length(delete_file_paths)
  
  # delete & message
  if (n_delete_file_paths  > 0 || n_delete_folder_paths > 0) {
    unlink(c(delete_file_paths, delete_folder_paths), recursive = TRUE)
    n_deleted <- n_delete_file_paths + n_delete_folder_paths 
    cat("Failed renderings detected: ", n_deleted,"\n",
        "\n",sep = "")
    if (n_delete_file_paths > 0){
      cat("Files deleted:", "\n",
          paste(" -", basename(delete_file_paths), collapse = "\n"), "\n",
          "\n",sep = "")
    }
    if (n_delete_folder_paths > 0) {
      cat("Folders deleted:", "\n",
          paste(" -", basename(delete_folder_paths), collapse = "\n"), "\n",
          sep = "")
    }
  }
  
  # run/render .qmd file
  #----------------------
  # run/render at call path
  quarto::quarto_render(
    input = qmd_file,  
    output_format = output_format,
  )
  
  # move render to output path
  html_name <- sub("\\.qmd$", paste0(".",output_format), qmd_file)
  
  file.rename(
    from = html_name,
    to = file.path(output_path, html_name)
  )
  
  #add number prefix to png 
  #-------------------------
  # new png files list
  png_list_after <- list.files(output_path, pattern = "\\.png$", full.names = TRUE) 
  png_list_diff  <- setdiff(png_list_after, png_list_before)
  
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
  #----------------------------
  
  # check for unnamed png files 
  unnamed_idx  <- grepl("^[0-9]{2}_+unnamed-chunk", basename(prefix_png_list_diff))
  unnamed_pngs <- prefix_png_list_diff[unnamed_idx]
  
  # print png name status
  if (length(unnamed_pngs) > 0) {
    cat(length(unnamed_pngs), " unnamed file(s) detected\n\n",
        ".png files:\n",
        paste(" -", basename(unnamed_pngs), collapse = "\n"), "\n\n",
        "Created from .qmd document:\n",
        " - ", qmd_file, "\n\n",
        "Remember to name labels on ALL chunks with '#| label: ...'\n",
        sep = ""
    )
  }
}



