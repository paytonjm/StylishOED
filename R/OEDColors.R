OED_colors <- function(n = NULL, group = NULL) {
  # Full color palette (no "OED" prefix)
  colors <- c(
    darkblue = "#1F4D70",
    darkblue75 = "#3078AE",
    darkblue50 = "#7FB4DB",
    darkblue25 = "#BAD7EC",
    lightbluedark = "#36769A",
    lightblue = "#61A3C8",
    lightblue75 = "#87B9D5",
    lightblue50 = "#BED9E8",
    lightblue25 = "#E0EDF4",
    tealdark = "#275B54",
    teal = "#3B8A7F",
    teal75 = "#64BCAF",
    teal50 = "#99C8C0",
    teal25 = "#D1EBE7",
    red = "#D01220",
    pink = "#F15B64",
    pink75 = "#F39AA3",
    pink25 = "#FCD8DB",
    darkorange = "#BE750A",
    orange = "#F39B16",
    orange50 = "#FBDBAB",
    orange25 = "#FDF1DF"
  )

  # Manually defined groups
  groups <- list(
    darkblue = c("darkblue", "darkblue75", "darkblue50", "darkblue25"),
    lightblue = c("lightbluedark", "lightblue", "lightblue75", "lightblue50", "lightblue25"),
    teal = c("tealdark", "teal", "teal75", "teal50", "teal25"),
    pink = c("red", "pink", "pink75", "pink25"),
    orange = c("darkorange", "orange", "orange50", "orange25"),
    multiple = c("darkblue", "darkblue75", "darkblue50", "darkblue25", "orange25", "orange50", "orange", "darkorange", "red")
  )

  # Default order
  default_order <- c(
    "lightblue", "orange", "teal", "red", "pink",
    "darkblue", "darkblue75", "darkblue50", "darkblue25",
    "lightbluedark", "lightblue75", "lightblue50", "lightblue25",
    "tealdark", "teal75", "teal50", "teal25",
    "pink75", "pink25", "darkorange", "orange50", "orange25"
  )

  palette <- colors[default_order]

  # Case 1: group is specified
  if (!is.null(group)) {
    if (!(group %in% names(groups))) {
      stop("Invalid group name. Valid groups are: ", paste(names(groups), collapse = ", "))
    }
    group_colors <- colors[groups[[group]]]
    if (is.null(n)) return(unname(group_colors))
    if (n > length(group_colors)) {
      warning("Requested more colors than available in group. Returning all group colors. For a larger choice of colors for continious variables, choose 'group = continious'")
      return(unname(group_colors))
    }
    return(unname(group_colors[1:n]))
  }

  # Case 2: n is specified without group
  if (!is.null(n)) {
    if (n > length(palette)) {
      warning("Requested more colors than available. Returning all available colors.")
      return(unname(palette))
    }
    return(unname(palette[1:n]))
  }

  # Case 3: neither n nor group specified — return full default palette
  return(unname(palette))
}
