OED_MD_to_HTML <- function(
    article_file_source = NULL,
    output_file_name    = NULL
) {
  if (!requireNamespace("knitr", quietly = TRUE))
    stop("Package 'knitr' is required.")
  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("Package 'rmarkdown' is required.")
  if (!requireNamespace("rstudioapi", quietly = TRUE))
    stop("Package 'rstudioapi' is required (for file picker / prompt).")
  
  # helper
  is_blank_scalar <- function(x) {
    if (is.null(x)) return(TRUE)
    if (length(x) == 0) return(TRUE)
    x1 <- tryCatch(as.character(x)[1], error = function(e) NA_character_)
    if (is.na(x1)) return(TRUE)
    !nzchar(trimws(x1))
  }
  
  # 1. pick source file
  if (is_blank_scalar(article_file_source)) {
    article_file_source <- rstudioapi::selectFile(
      caption = "Choose highchart Rmd file"
    )
    if (is_blank_scalar(article_file_source)) {
      stop("Canceled at prompt: Choose highchart file.")
    }
  }
  article_file_source <- normalizePath(article_file_source, winslash = "/")
  out_dir <- dirname(article_file_source)
  
  # 2. output name
  if (is_blank_scalar(output_file_name)) {
    output_file_name <- rstudioapi::showPrompt(
      title   = "Output Filename",
      message = "What do you want to call this HTML file? (ex., hicharts-test):"
    )
    if (is_blank_scalar(output_file_name)) {
      stop("Canceled or blank at prompt: Output Filename.")
    }
  }
  
  temp_md  <- file.path(out_dir, paste0(output_file_name, ".md"))
  out_html <- file.path(out_dir, paste0(output_file_name, ".html"))
  
  # 3. locate template inside the package
  template_path <- system.file(
    "extdata", "template-multi.html",
    package = "StylishOED"   
  )
  if (!nzchar(template_path)) {
    stop("Could not find 'template-multi.html' in yourpkg::inst/extdata.")
  }
  
  # 4. knit Rmd -> md
  knitr::knit(
    input  = article_file_source,
    output = temp_md
  )
  
  # 5. md -> HTML via Pandoc using that template
  rmarkdown::pandoc_convert(
    input   = temp_md,
    to      = "html",
    output  = out_html,
    options = c(
      paste0("--template=", template_path),
      "--from=markdown+raw_html-implicit_figures-header_attributes"
    )
  )
  
  message(paste0("✅ Clean HTML written to ", out_html))
  invisible(out_html)
}
