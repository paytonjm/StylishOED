% OED_HTML_Table documentation
\name{OED_HTML_Table}
\alias{OED_HTML_Table}
\title{Create Accessible HTML Table From Excel}
\usage{
OED_HTML_Table(
  Excel_Table  = NA,
  sheet_name   = NA,
  ada_caption  = NA,
  table_number = NA,
  col_width_1  = NA,
  output_name  = NA
)
}
\value{
A formatted HTML file is generated and saved in the same folder as the source Excel file.
}
\description{
Convert a formatted Excel table into an accessible HTML table for use on QualityInfo.org.
This version of the \strong{HTML Table Maker} includes improvements for accessibility,
formatting recognition, and multi-level table structures, using CSS classes aligned
with those used on QualityInfo.org.
}
\details{
\strong{Key features:}
\itemize{
  \item Prompts for Excel file, sheet, accessibility caption, table number, first column width, and output filename.
  \item Detects and applies Excel formatting such as bold text, indentation, and cell formats
        (currency, percentage, number, annual).
  \item Builds an internal \code{fmt_df} data frame for debugging and testing formatting behavior.
  \item Supports multiple footnotes, joining them with \code{<br>} tags.
  \item Adds skip logic and table numbering for improved screen reader accessibility.
}
}
\examples{
\dontrun{
# Launches user prompts and builds an HTML table
library(StylishOED)
OED_HTML_Table()
}
}
