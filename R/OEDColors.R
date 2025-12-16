OED_colors <- function(index = NULL, group = NULL, color = NULL) {
  # Full color palette (no "OED" prefix)
  colors <- c(
    OEDblue = "#003764", #add OED blue color
    darkblue = "#1F4D70",
    darkblue75 = "#3078AE",
    darkblue50 = "#7FB4DB",
    darkblue25 = "#BAD7EC",
    lightbluedark = "#36769A",
    lightblue = "#61A3C8",
    lightblue75 = "#87B9D5",
    lightblue50 = "#BED9E8",
    lightblue25 = "#E0EDF4",
    darkteal = "#275B54",
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
    orange25 = "#FDF1DF",
    lightgold = "#FFC85A",
    lightgray = "#BFBFBF",
    darkgray = "#7F7F7F"
  )

  # Manually defined groups
  groups <- list(
    darkblue = c("OEDblue", "darkblue75", "darkblue50", "darkblue25"),
    lightblue = c("lightbluedark", "lightblue", "lightblue75", "lightblue50", "lightblue25"),
    teal = c("darkteal", "teal", "teal75", "teal50", "teal25"),
    pink = c("red", "pink", "pink75", "pink25"),
    orange = c("orange", "orange50", "orange25", "darkorange"),
    multi = c("darkblue", "lightbluedark", "lightblue", "lightblue75", "lightblue50", "lightblue25", "orange25", "orange50", "orange", "darkorange", "red")
  )

  # Default order


default_order <- c(
  "lightblue", "OEDblue", "lightgold", "teal",
  "pink75", "darkteal", "darkorange", "teal50",
  "pink", "darkgray",
  "orange25", "lightblue25", "darkblue25", "orange50",
  "lightgray", "red", "lightblue50", "pink25",
  "darkblue", "lightbluedark", "darkblue75", "orange",
  "teal25", "lightblue75", "teal75", "darkblue50"
  )


  palette <- colors[default_order]

  # Case 1: Specific color name
  if (!is.null(color)) {
    if (!(color %in% names(colors))) {
      stop("Invalid color name. Valid names are: ", paste(names(colors), collapse = ", "))
    }
    return(unname(colors[color]))
  }

  # Case 2: Group selection (with or without index)
  if (!is.null(group)) {
    if (!(group %in% names(groups))) {
      stop("Invalid group name. Valid groups are: ", paste(names(groups), collapse = ", "))
    }
    group_colors <- colors[groups[[group]]]
    if (!is.null(index)) {
      if (index < 1 || index > length(group_colors)) {
        stop("Index out of range for group '", group, "'. Must be between 1 and ", length(group_colors), ".")
      }
      return(unname(group_colors[index]))
    }
    return(unname(group_colors))
  }

  # Case 3: Index from default palette
  if (!is.null(index)) {
    if (index < 1 || index > length(palette)) {
      stop("Index out of range. Must be between 1 and ", length(palette), ".")
    }
    return(unname(palette[index]))
  }

  # Case 4: Return full default palette
  return(unname(palette))
}
