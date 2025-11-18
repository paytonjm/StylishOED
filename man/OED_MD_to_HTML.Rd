% OED_MD_to_HTML documentation
\name{OED_MD_to_HTML}
\alias{OED_MD_to_HTML}
\title{Generate HTML From MD in R}
\usage{
OED_MD_to_HTML(article_file_source = NA,
    output_file_name    = NA)
}
\value{
A formatted HTML file is generated and saved in the same folder as the article file.
}
\description{
Export a article script (Rmd) object to an HTML file.
This helper function is designed to recognize an markdown file set to be saved as an md file and extract the
minimum script needed to generate a clean HTML file.
}
\details{
\strong{Key features:}
\itemize{
  \item Accepts RMD files with YAML file type md.
  \item Uses pandoc to convert to HTML.
  \item Users may use prompts or manually set the file directory and output.
  \item Saves an HTML.
}
}
\section{Requirements}{
\itemize{
  \item Requires the \code{rstudioapi}, \code{knitr}, and \code{rmarkdown} packages.
  
}
}
\examples{
\dontrun{
# Example: export a Highcharts object to HTML
library(StylishOED)

# assuming hc_chart is a highcharter object created elsewhere
# OED_MD_to_HTML(
    article_file_source = NA,
    output_file_name    = NA)

}
}
