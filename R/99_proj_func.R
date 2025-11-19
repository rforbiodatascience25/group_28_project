#=============================
# 00_all.qmd HELPER FUNCTIONS
#=============================

render_scripts_to_results <- function(qmd_file=q) {
  # Create "/results" if none exist
  dir.create(file.path("..", "results"),  recursive = TRUE, showWarnings = FALSE)
  # Renders the qmd file(s) as html and moves them to "../results"
  quarto::quarto_render(
    input = qmd_file,  # file in the R/ folder
    output_format = "html"
  )
  
  #define name for html file
  html_name <- sub("\\.qmd$", ".html", qmd_file)
  
  # move html renders from "/R" to "/results"
  file.rename(
    from = html_name,
    to = file.path("../results", html_name)
  )
}

