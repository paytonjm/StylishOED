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
\details{
The full OED color palette includes the following named colors and their hex codes:

\preformatted{
darkblue      - #1F4D70
darkblue75    - #3078AE
darkblue50    - #7FB4DB
darkblue25    - #BAD7EC
lightbluedark - #36769A
lightblue     - #61A3C8
lightblue75   - #87B9D5
lightblue50   - #BED9E8
lightblue25   - #E0EDF4
tealdark      - #275B54
teal          - #3B8A7F
teal75        - #64BCAF
teal50        - #99C8C0
teal25        - #D1EBE7
red           - #D01220
pink          - #F15B64
pink75        - #F39AA3
pink25        - #FCD8DB
darkorange    - #BE750A
orange        - #F39B16
orange50      - #FBDBAB
orange25      - #FDF1DF
}
}
\examples{
OED_colors(5)  # First 5 colors from the default palette
OED_colors(group = "teal")  # All teal group colors
OED_colors(n = 2, group = "orange")  # First 2 orange colors
OED_colors(color = "darkblue")  # Hex code for darkblue
}
