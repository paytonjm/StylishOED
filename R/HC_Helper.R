#' OED_Export_HiChart
#'
#' Convert a formatted HiChart table into an accessible HTML chart for use on QualityInfo.org.
#'
#' @description
#' This version of the **HiChart Helper** has been developed for testing
#' to recognize different chart types and extract the minimum script needed to generate a dynamic chart.
#' Class structures and styling will be aligned to match what is found on QualityInfo.org main.css
#'
#'
#' @details
#' **Key upgrades include:**
#' \itemize{
#'   \item Takes the hicharts object and pulls all options stored in json.
#'   \item QualityInfo already has hicharts.js loaded so no scripts needed in document headers.
#'   \item Prompt for naming the output file (no need to type \code{.html}).
#'   \item Saves an HTML file styled for QualityInfo.org, with CSS classes integrated for OED management.
#'   \item Adds skip logic and table numbering for improved screen reader accessibility.
#' }
#'
#' @section Requirements:
#' \itemize{
#'   \item Needs rstudioapi, and htmltools packages.
#'   \item Users should review the Division style guide so all colors and contrasts meet requirements.
#'         }
#'
#' @return
#' A formatted HTML file is generated and saved in the same folder as the article file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Launches user prompts and builds an HTML chart
#' #
#' library(StylishOED)
#' OED_Export_HiChart()
#' }

#Going to try a hybrid of the map helper
OED_Export_HiChart <- function(
  hc_chart, 
  container_id = "hc_container",
  .file = showPrompt("Output filename", "Save As", default = "chart_output"), 
  .title = showPrompt("Chart title", "Enter chart title", default = "Example Chart"), 
  .alt_text = showPrompt("Alt text", "Describe this chart", default = "Highcharts visualization."), 
  .chart_num = showPrompt("Chart number", "Enter chart number", default = "1")) {

  # ---- Required packages ----
  required_pkgs <- c("highcharter", "jsonlite", "rstudioapi")

  # Install any missing packages
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    message("📦 Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs)
  }

  # Load all required packages
  lapply(required_pkgs, library, character.only = TRUE)

  #----------------------------------------------------------------
 
  # Support for JS_EVAL and injection
  clean_opts_for_js <- function(obj) {
    if (inherits(obj, "JS_EVAL")) {
      paste0("JS_MARKER(", obj[[1]], ")")
    } else if (is.list(obj)) {
      lapply(obj, clean_opts_for_js)
    } else {
      obj
    }
  }
 
  inject_js_literals <- function(json_txt) {
      out <- gsub('"JS_MARKER\\((function\\s*\\(.*?\\{[\\s\\S]*?\\})\\)"', '\\1', json_txt, perl = TRUE)
      out <- gsub("\\\\n", "\n", out, perl = TRUE)
      out
  }
 
  if (!inherits(hc_chart, "highchart")) stop("Input must be a highchart object")
 
  file <- .file
  title <- .title
  alt_text <- .alt_text
  chart_num <- .chart_num

  if (!inherits(hc_chart, "highchart")) stop("Input must be a highchart object")

  file <- showPrompt("Output filename", "Save As", default = "chart_output")
  title <- showPrompt("Chart title", "Enter chart title", default = "Example Chart")
  alt_text <- showPrompt("Alt text", "Describe this chart", default = "Highcharts visualization.")
  chart_num <- showPrompt("Chart number", "Enter chart number", default = "1")

  # build unique container_id with chart number
  container_id_full <- sprintf("%s%s", container_id, chart_num)

  # update chart title
  hc_chart$x$hc_opts$title$text <- title

  # detect chart type
  chart_type <- hc_chart$x$type %||% "chart"

  #clear map for newer map version
  if (chart_type == "map") {

    # remove stale map references
    hc_chart$x$hc_opts$chart$map <- NULL
    if (!is.null(hc_chart$x$hc_opts$series)) {
      hc_chart$x$hc_opts$series <- lapply(
        hc_chart$x$hc_opts$series,
        function(s) { s$mapData <- NULL; s }
      )
    }

    get_map_id <- function(hc) {
      md <- hc$x$hc_opts$series[[1]]$mapData
      if (is.null(md)) return(NULL)
      if (is.character(md)) return(md)
      if (inherits(md, "JS_EVAL")) {
        code <- md[[1]]
        m <- regmatches(code, regexec("Highcharts\\.maps\\[['\"]([^'\"]+)['\"]\\]", code))[[1]]
        if (length(m) >= 2) return(m[2])
      }
      NULL
    }

    map_id <- get_map_id(hc_chart)
    geourl <- if (!is.null(map_id)) {
      sprintf("https://code.highcharts.com/mapdata/%s.topo.json", map_id)
    } else {
      # default fallback (Oregon)
      "https://code.highcharts.com/mapdata/countries/us/us-or-all.topo.json"
    }
  }

  #clean options
  opts_clean <- clean_opts_for_js(hc_chart$x$hc_opts)
  json <- jsonlite::toJSON(opts_clean, auto_unbox = TRUE, pretty = TRUE,
                           null = "null", dataframe = "rows")
  json <- inject_js_literals(json)

  # --- build script depending on chart type ---
  if (chart_type == "map") {
    script <- sprintf(
      '<script>
      fetch("%s")
        .then(r => r.json())
        .then(topology => {
          Highcharts.mapChart("%s",
            (function(c){
              if (!Array.isArray(c.series)) c.series = [];
              if (!c.series.length) c.series.push({});
              c.series[0] = Object.assign({}, c.series[0], { mapData: topology });
              return c;
            })(%s)
          );
        });
    </script>',
      geourl,             # dynamically built topo URL
      container_id_full,  # container id for chart div
      json                # serialized chart options
    )
  } else {
    script <- sprintf(
      '<script>
        Highcharts.chart("%s", %s);
      </script>',
      container_id_full,
      json
    )
  }

  # now embed script inside main template
  html <- sprintf(
    '<div>
  <style>
  #%2$s {
    font-family: Arial, sans-serif;
    width: 100%%;
    max-width: 880px;
    min-height:400px;   /* key fix */
    aspect-ratio: 4 / 3;
    margin: 0 auto;
  }

  @media screen and (max-width: 480px) {
    #%2$s { aspect-ratio: 1 / 1; }
  }
  </style>

  <div id="%2$s" role="region" aria-label="%3$s"></div>
  %4$s
</div>',
    title,             # %1$s (unused, kept for readability)
    container_id_full, # %2$s → hc_container + chart_num
    alt_text,          # %3$s
    script             # %4$s
  )

  writeLines(html, paste0(file, ".html"))
  message("✅ Exported interactive chart: ", file, " (container id = ", container_id, ")")
}



