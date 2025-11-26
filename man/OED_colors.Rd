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
The full OED color palette includes the following named colors and their hex codes.

\if{html}{
  \out{
    <style>
      .swatch { display: inline-block; width: 20px; height: 20px; margin-right: 8px; border: 1px solid #ccc; vertical-align: middle; }
    </style>
    <p><span class="swatch" style="background-color:#1F4D70;"></span> darkblue - #1F4D70</p>
    <p><span class="swatch" style="background-color:#3078AE;"></span> darkblue75 - #3078AE</p>
    <p><span class="swatch" style="background-color:#7FB4DB;"></span> darkblue50 - #7FB4DB</p>
    <p><span class="swatch" style="background-color:#BAD7EC;"></span> darkblue25 - #BAD7EC</p>
    <p><span class="swatch" style="background-color:#36769A;"></span> lightbluedark - #36769A</p>
    <p><span class="swatch" style="background-color:#61A3C8;"></span> lightblue - #61A3C8</p>
    <p><span class="swatch" style="background-color:#87B9D5;"></span> lightblue75 - #87B9D5</p>
    <p><span class="swatch" style="background-color:#BED9E8;"></span> lightblue50 - #BED9E8</p>
    <p><span class="swatch" style="background-color:#E0EDF4;"></span> lightblue25 - #E0EDF4</p>
    <p><span class="swatch" style="background-color:#275B54;"></span> tealdark - #275B54</p>
    <p><span class="swatch" style="background-color:#3B8A7F;"></span> teal - #3B8A7F</p>
    <p><span class="swatch" style="background-color:#64BCAF;"></span> teal75 - #64BCAF</p>
    <p><span class="swatch" style="background-color:#99C8C0;"></span> teal50 - #99C8C0</p>
    <p><span class="swatch" style="background-color:#D1EBE7;"></span> teal25 - #D1EBE7</p>
    <p><span class="swatch" style="background-color:#D01220;"></span> red - #D01220</p>
    <p><span class="swatch" style="background-color:#F15B64;"></span> pink - #F15B64</p>
    <p><span class="swatch" style="background-color:#F39AA3;"></span> pink75 - #F39AA3</p>
    <p><span class="swatch" style="background-color:#FCD8DB;"></span> pink25 - #FCD8DB</p>
    <p><span class="swatch" style="background-color:#BE750A;"></span> darkorange - #BE750A</p>
    <p><span class="swatch" style="background-color:#F39B16;"></span> orange - #F39B16</p>
    <p><span class="swatch" style="background-color:#FBDBAB;"></span> orange50 - #FBDBAB</p>
    <p><span class="swatch" style="background-color:#FDF1DF;"></span> orange25 - #FDF1DF</p>
  }
}
}
\examples{
OED_colors(5)
OED_colors(group = "teal")
OED_colors(n = 2, group = "orange")
OED_colors(color = "darkblue")
}
