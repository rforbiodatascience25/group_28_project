#=============================
# 00_all.qmd HELPER FUNCTIONS
#=============================

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
  #' 
  #'Example 2: 
  #'  list.files(output_location, pattern = "\\.svg$", full.names = TRUE) |> 
  #'    lapply(convert_one_svg) |> 
  #'    invisible()

  # Convert .svg to .png
  #----------------------
  png_file <- sub("\\.svg$", ".png", svg)
  system2(
    Sys.which("convert"),
    args   = c(svg, png_file),
    stdout = FALSE,   
    stderr = FALSE  
  )
}

render_qmd <- function(qmd_file, 
                       output_location="../results", 
                       output_format="html") {
  #'Description: 
  #'  Helper function for 00_all.qmd and does the following:
  #'  1. Runs and renders a .qmd files at location run.
  #'  2. It moves the rendered file to output_location, 
  #'  3. converts .svg files to .png at the output location 
  #'  4. deletes .svg files at at output location
  #'  
  #'Arguments: 
  #'  qmd_file = file name string
  #'  output_location = the file path for the output location
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
  
  # run/render .qmd file
  #----------------------
  # run/render at location
  quarto::quarto_render(
    input = qmd_file,  # file in the R/ folder
    output_format = output_format,
  )
  
  # move html renders from "/R" to "/results"
  html_name <- sub("\\.qmd$", paste0(".",output_format), qmd_file) # define html name
  
  file.rename(
    from = html_name,
    to = file.path(output_location, html_name)
  )

  #Convert .svg files in output folder to .png
  #-------------------------------------------
  # Search for SVG files at output_location
  svg_list <- list.files(output_location, pattern = "\\.svg$", full.names = TRUE) 
  
  # return if none found
  if (length(svg_list) == 0) {
    cat("rendered", qmd_file, "and", length(svg_list), "png files in", output_location, "\n")
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
  
  cat("Rendered", qmd_file, "and", length(svg_list), "png files in", output_location, "\n")
}

