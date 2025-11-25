#=========================================================
#              00_all.qmd HELPER FUNCTIONS
#=========================================================
convert_svg_to_png <- function(svg) {
  #'Description:
  #'  Converts a .svg file to .png
  #'  Helper function for render_qmd
  #'  
  #'Arguments: 
  #'  svg = .svg file name and path
  #' 
  #'Example 1: 
  #'  convert_svg_to_png("../results/04_scatter-1.svg)  
  #---------------------------------------
  # Convert .svg to .png
  png_file <- sub("\\.svg$", ".png", svg)
  system2(
    Sys.which("convert"),
    args   = c(svg, png_file),
    stdout = FALSE,   
    stderr = FALSE  
  )
}

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
  
  if (n_delete_file_paths  > 0 || n_delete_folder_paths > 0) {
    # delete files
    unlink(c(delete_file_paths, delete_folder_paths), recursive = TRUE)
    # print message
    n_deleted <- n_delete_file_paths + n_delete_folder_paths  # first count deleted files
    cat("==================================", "\n",
        "Cleaning failed renderings in ./R", "\n", 
        "==================================", "\n",
        "Failed renderings detected: ", n_deleted,"\n",
        "\n",
        "Files deleted:", "\n",
        paste(" -", basename(delete_file_paths), collapse = "\n"), "\n",
        "\n",
        "Folders deleted:", "\n",
        paste(" -", basename(delete_folder_paths), collapse = "\n"), "\n",
        sep = ""
        )
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
  
  #Convert .svg files in output folder to .png
  #------------------------------------------------
  # Search for SVG files at output_path
  svg_list <- list.files(output_path, pattern = "\\.svg$", full.names = TRUE) 
  if (length(svg_list) == 0) {
    cat("rendered", qmd_file, "and", length(svg_list), "png files in", output_path, "\n")
    return()
  }
  
  # Extract .qmd prefix
  qmd_prefix <- sub("_.*$", "",basename(qmd_file))  
  
  # Create new .svg files with prefix
  Base_prefix_svg_list <- paste0(qmd_prefix, "_", basename(svg_list))
  prefix_svg_list <- svg_list |> 
    dirname() |> 
    file.path(Base_prefix_svg_list )
    
  # Add prefix to .svg files
  file.rename(svg_list, prefix_svg_list)
  
  # Convert all .svg files to .png
  prefix_svg_list |> 
    lapply(convert_svg_to_png) |> 
    invisible()
  
  # Delete leftover .svg files
  file.remove(prefix_svg_list)
  
  cat("Rendered", qmd_file, "and", length(svg_list), "png files in", output_path, "\n")
}



