\name{OED_colors}
\alias{OED_colors}
\title{Retrieve OED-approved color palette}
\description{
This function provides access to the official OED (Oregon Employment Department) color palette,
including individual colors, grouped color themes, or a default palette.
}
\usage{
OED_colors(n = NULL, group = NULL, color = NULL)
}
\arguments{
  \item{n}{
    Optional. Number of colors to return. If \code{group} is specified, returns up to \code{n} colors from that group.
    If neither \code{group} nor \code{color} is specified, returns the first \code{n} colors from the default palette.
  }
  \item{group}{
    Optional. A named group of colors. Valid groups include: \code{"darkblue"}, \code{"lightblue"}, \code{"teal"},
    \code{"pink"}, \code{"red"}, \code{"orange"}, \code{"darkorange"}.
  }
  \item{color}{
    Optional. A specific color name. Returns the corresponding hex code.
  }
}
\value{
A character vector of hex color codes.
}
\examples{
OED_colors(5)  # First 5 colors from the default palette
OED_colors(group = "teal")  # All teal group colors
OED_colors(n = 2, group = "orange")  # First 2 orange colors
OED_colors(color = "darkblue")  # Hex code for darkblue
}
