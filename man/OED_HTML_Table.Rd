% Generated manually: OED_HTML_Table documentation
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
This version of the \strong{HTML Table Maker} has been upgraded with robust features for
accessibility, formatting recognition, and multi-level table structures.
Class structures and styling have been aligned to match what is found in the
\code{main.css} used on QualityInfo.org.
}
\details{
Convert a formatted Excel table into an accessible HTML table for use on QualityInfo.org.

\strong{Key upgrades include:}
\itemize{
  \item Screening and handling of nested titles, with support for multiple levels of detail.
  \item User-friendly prompts that display available sheets without opening Excel.
  \item Prompt for naming the output file (no need to type \code{.html}).
  \item Recognition and application of Excel bold formatting.
  \item Recognition of indentation for hierarchical data (multiple levels supported).
  \item Automatic detection of Excel cell formats, including currency, percentage,
        number, and annual.
  \item Saves an HTML file styled for QualityInfo.org, with CSS classes integrated
        for OED management.
  \item Adds skip logic and table numbering for improved screen reader accessibility.
  \item User prompt for adjusting first column width.
  \item Handles columns containing mixed formats.
  \item Builds an internal \code{fmt_df} data frame for debugging and testing.
  \item Supports multiple footnotes, which are appended with \code{<br>} tags automatically.
  \item Enhanced error handling for user prompt cancellations.
  \item Detects cells formatted as \emph{General} that contain dollar signs or percentages
        and assigns them to appropriate categories (currency, annual, employment,
        percentage, etc.).
  \item Cleans up percentages so they are shown with zero decimal places (integer percentages).
}
}
\section{Requirements}{
\itemize{
  \item Input must be a Microsoft Excel \code{.xlsx} file.
  \item The data table and title should begin in column \code{A1} (no helper columns).
  \item Include a space between the end of the data and the source and footnote(s)
        you want included.
  \item The script will look for formats (currency, percentage, employment). You may
        also enter symbols within the cell, and it will recognize them as:
        \itemize{
          \item Employment
          \item Currency (hourly)
          \item Currency (annual)
          \item Percentage
          \item Text
        }
}
}
\examples{
\dontrun{
# Launches user prompts and builds an HTML table
library(StylishOED)
OED_HTML_Table()
}
}

library(StylishOED)
OED_HTML_Table()
}
}
