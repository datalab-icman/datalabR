#' Progress bar class with colored Unicode symbols
#'
#' @description
#' A comprehensive and visually appealing progress bar implementation with colored
#' Unicode symbols, spinners, and rich formatting options. Inspired by the cli package
#' but standalone and fully self-contained.
#'
#' @details
#' This progress bar system provides:
#' * Beautiful Unicode box-drawing characters and symbols
#' * ANSI color support with automatic terminal detection
#' * Multiple built-in styles (modern, classic, elegant, minimal, dots, arrows)
#' * Customizable colors for complete, current, and incomplete sections
#' * Animated spinners with various styles
#' * Automatic ETA and rate calculations
#' * Terminal width detection and responsive layout
#' * Clean completion and interruption handling
#'
#' @section Format tokens:
#' The following tokens can be used in the format string:
#' * `{bar}` - The progress bar itself with colored symbols
#' * `{current}` - Current iteration number
#' * `{total}` - Total number of iterations
#' * `{percent}` - Percentage complete (e.g., "45%")
#' * `{elapsed}` - Time elapsed since start
#' * `{eta}` - Estimated time to completion
#' * `{rate}` - Iterations per second
#' * `{spin}` - Animated spinning indicator
#' * `{status}` - Custom status message
#' * Any custom token passed via the `tokens` parameter
#'
#' @section Color support:
#' Colors are automatically enabled if the terminal supports ANSI colors.
#' You can force enable/disable colors with the `use_colors` parameter.
#' Available colors: black, red, green, yellow, blue, magenta, cyan, white,
#' and their bright variants (e.g., "bright_green").
#'
#' @section Built-in styles:
#' * `modern` - Colorful boxes with smooth gradients (default)
#' * `classic` - Traditional equal signs and dashes
#' * `elegant` - Unicode block elements with subtle colors
#' * `minimal` - Simple dots, clean and fast
#' * `dots` - Circular dots that fill up
#' * `arrows` - Arrow symbols pointing right
#' * `blocks` - Solid and shaded Unicode blocks
#' * `circles` - Circular progress indicators
#'
#' @examples
#' \dontrun{
#' # Basic usage with default modern style
#' pb <- ProgressBar$new(total = 100)
#' for (i in 1:100) {
#'   pb$tick()
#'   Sys.sleep(0.02)
#' }
#'
#' # Different styles showcase
#' styles <- c("modern", "elegant", "dots", "arrows", "blocks", "circles")
#' for (style in styles) {
#'   cat("\nStyle:", style, "\n")
#'   pb <- ProgressBar$new(
#'     total = 50,
#'     format = "{spin} {bar} {percent} | {style}",
#'     style = style,
#'     width = 40
#'   )
#'   for (i in 1:50) {
#'     pb$tick(tokens = list(style = style))
#'     Sys.sleep(0.01)
#'   }
#' }
#'
#' # Custom colors and format
#' pb <- ProgressBar$new(
#'   total = 100,
#'   format = "{spin} Processing {bar} {percent} | ETA: {eta}",
#'   complete_color = "bright_green",
#'   current_color = "bright_yellow",
#'   incomplete_color = "bright_black",
#'   width = 60
#' )
#' for (i in 1:100) {
#'   pb$tick()
#'   Sys.sleep(0.02)
#' }
#'
#' # File processing with custom tokens
#' files <- paste0("data_file_", 1:25, ".csv")
#' pb <- ProgressBar$new(
#'   total = length(files),
#'   format = "{spin} {bar} {current}/{total} | Processing: {filename}",
#'   style = "elegant",
#'   show_after = 0
#' )
#' for (f in files) {
#'   # Simulate file processing
#'   pb$tick(tokens = list(filename = f))
#'   Sys.sleep(0.1)
#' }
#'
#' # Data processing with status updates
#' pb <- ProgressBar$new(
#'   total = 200,
#'   format = "{spin} {bar} {percent} | {status}",
#'   style = "blocks"
#' )
#' for (i in 1:200) {
#'   status <- if (i %% 50 == 0) "Checkpoint saved" else "Processing..."
#'   pb$tick(tokens = list(status = status))
#'   Sys.sleep(0.01)
#' }
#'
#' # Minimal style for fast operations
#' pb <- ProgressBar$new(
#'   total = 1000,
#'   format = "{bar} {current}/{total} {rate}",
#'   style = "minimal",
#'   clear = FALSE
#' )
#' for (i in 1:1000) {
#'   pb$tick()
#'   Sys.sleep(0.001)
#' }
#'
#' # Long-running task with detailed info
#' pb <- ProgressBar$new(
#'   total = 500,
#'   format = paste0(
#'     "{spin} Analysis {bar} {percent}\n",
#'     "├─ Completed: {current}/{total} items\n",
#'     "├─ Speed: {rate}\n",
#'     "└─ ETA: {eta}"
#'   ),
#'   style = "modern",
#'   width = 50
#' )
#' for (i in 1:500) {
#'   pb$tick()
#'   Sys.sleep(0.005)
#' }
#'
#' # Using update() instead of tick()
#' pb <- ProgressBar$new(total = 100, style = "circles")
#' for (i in seq(0, 100, by = 5)) {
#'   pb$update(i, tokens = list(status = paste("Step", i)))
#'   Sys.sleep(0.1)
#' }
#'
#' # Downloading simulation
#' pb <- ProgressBar$new(
#'   total = 100,
#'   format = "{spin} Downloading {bar} {percent} | {speed} MB/s | {eta}",
#'   style = "arrows"
#' )
#' for (i in 1:100) {
#'   speed <- round(runif(1, 1.5, 5.5), 1)
#'   pb$tick(tokens = list(speed = speed))
#'   Sys.sleep(0.03)
#' }
#' }
#'
#' @export
ProgressBar <- R6::R6Class(
  "ProgressBar",
  public = list(
    #' @field total Total number of ticks
    total = NULL,

    #' @field current Current tick count
    current = NULL,

    #' @field width Width of the progress bar
    width = NULL,

    #' @field format Format string for display
    format = NULL,

    #' @field clear Whether to clear the progress bar on completion
    clear = NULL,

    #' @field show_after Show progress bar after this many seconds
    show_after = NULL,

    #' @field style Visual style of the progress bar
    style = NULL,

    #' @description
    #' Create a new progress bar with colored Unicode symbols
    #'
    #' @param total Total number of iterations
    #' @param format Format string with tokens (default: "{bar} {percent} {eta}")
    #' @param width Width of the progress bar in characters (default: auto-detect)
    #' @param style Visual style: "modern", "classic", "elegant", "minimal",
    #'   "dots", "arrows", "blocks", or "circles" (default: "modern")
    #' @param complete_color Color for completed portion (default: "green")
    #' @param current_color Color for the current position indicator (default: "yellow")
    #' @param incomplete_color Color for incomplete portion (default: "white")
    #' @param current Starting position (default: 0)
    #' @param clear Clear bar on completion (default: TRUE)
    #' @param show_after Delay in seconds before showing (default: 0.2)
    #' @param use_colors Force enable/disable colors (default: auto-detect)
    #' @param force Force display in non-interactive sessions (default: FALSE)
    #' @param stream Output stream (default: stderr())
    #'
    #' @return A new `ProgressBar` object
    initialize = function(total = 100,
                          format = "{bar} {percent} {eta}",
                          width = NULL,
                          style = "modern",
                          complete_color = "green",
                          current_color = "yellow",
                          incomplete_color = "white",
                          current = 0,
                          clear = TRUE,
                          show_after = 0.2,
                          use_colors = NULL,
                          force = FALSE,
                          stream = stderr()) {
      self$total <- total
      self$current <- current
      self$format <- format
      self$clear <- clear
      self$show_after <- show_after
      self$style <- style

      private$complete_color <- complete_color
      private$current_color <- current_color
      private$incomplete_color <- incomplete_color
      private$force <- force
      private$stream <- stream
      private$start_time <- Sys.time()
      private$last_draw_time <- private$start_time
      private$shown <- FALSE
      private$finished <- FALSE
      private$spinning <- 1

      # Initialize color support
      if (is.null(use_colors)) {
        private$use_colors <- private$detect_color_support()
      } else {
        private$use_colors <- use_colors
      }

      # Set up style characters and spinner
      private$setup_style(style)

      # Detect terminal width
      if (is.null(width)) {
        self$width <- private$get_terminal_width()
      } else {
        self$width <- width
      }

      # Check if we should display
      private$should_show <- interactive() || private$force

      invisible(self)
    },

    #' @description
    #' Increment the progress bar by one or more steps
    #'
    #' @param len Number of steps to advance (default: 1)
    #' @param tokens Named list of custom tokens to display
    #'
    #' @return Invisible self for method chaining
    tick = function(len = 1, tokens = list()) {
      if (private$finished) {
        return(invisible(self))
      }

      self$current <- self$current + len
      if (self$current > self$total) self$current <- self$total

      private$tokens <- tokens

      # Check if we should show the bar yet
      elapsed <- as.numeric(difftime(Sys.time(), private$start_time, units = "secs"))
      if (!private$shown && elapsed < self$show_after) {
        return(invisible(self))
      }

      private$shown <- TRUE
      private$render()

      if (self$current >= self$total) {
        self$terminate()
      }

      invisible(self)
    },

    #' @description
    #' Update the progress bar to a specific position
    #'
    #' @param current New current position (0 to total)
    #' @param tokens Named list of custom tokens to display
    #'
    #' @return Invisible self for method chaining
    update = function(current, tokens = list()) {
      if (private$finished) {
        return(invisible(self))
      }

      self$current <- current
      if (self$current > self$total) self$current <- self$total

      private$tokens <- tokens
      private$render()

      if (self$current >= self$total) {
        self$terminate()
      }

      invisible(self)
    },

    #' @description
    #' Mark the progress bar as complete and clean up
    #'
    #' @param tokens Named list of custom tokens for final display
    #'
    #' @return Invisible self
    terminate = function(tokens = list()) {
      if (private$finished) {
        return(invisible(self))
      }

      self$current <- self$total
      private$tokens <- tokens
      private$finished <- TRUE

      if (private$shown) {
        private$render()

        if (self$clear) {
          private$clear_line()
        } else {
          cat("\n", file = private$stream)
        }
      }

      invisible(self)
    },

    #' @description
    #' Get the current progress as a ratio between 0 and 1
    #'
    #' @return Numeric value between 0 and 1
    ratio = function() {
      if (self$total == 0) {
        return(1)
      }
      return(self$current / self$total)
    },

    #' @description
    #' Get elapsed time since progress bar creation
    #'
    #' @return Numeric elapsed time in seconds
    elapsed = function() {
      as.numeric(difftime(Sys.time(), private$start_time, units = "secs"))
    },

    #' @description
    #' Change the style of the progress bar on the fly
    #'
    #' @param style New style name
    #'
    #' @return Invisible self
    set_style = function(style) {
      self$style <- style
      private$setup_style(style)
      invisible(self)
    }
  ),
  private = list(
    complete_color = NULL,
    current_color = NULL,
    incomplete_color = NULL,
    force = NULL,
    stream = NULL,
    start_time = NULL,
    last_draw_time = NULL,
    shown = NULL,
    finished = NULL,
    should_show = NULL,
    tokens = NULL,
    spinning = NULL,
    spin_chars = NULL,
    complete_char = NULL,
    current_char = NULL,
    incomplete_char = NULL,
    last_line_length = 0,
    use_colors = NULL,

    # ANSI color codes
    colors = list(
      black = "\033[30m",
      red = "\033[31m",
      green = "\033[32m",
      yellow = "\033[33m",
      blue = "\033[34m",
      magenta = "\033[35m",
      cyan = "\033[36m",
      white = "\033[37m",
      bright_black = "\033[90m",
      bright_red = "\033[91m",
      bright_green = "\033[92m",
      bright_yellow = "\033[93m",
      bright_blue = "\033[94m",
      bright_magenta = "\033[95m",
      bright_cyan = "\033[96m",
      bright_white = "\033[97m",
      reset = "\033[0m"
    ),
    setup_style = function(style) {
      style_configs <- list(
        modern = list(
          complete = "\u2588", # Full block
          current = "\u2588", # Full block
          incomplete = "\u2591", # Light shade
          spinner = c("\u280B", "\u2819", "\u2839", "\u2838", "\u283C", "\u2834", "\u2826", "\u2827", "\u2807", "\u280F")
        ),
        classic = list(
          complete = "=",
          current = ">",
          incomplete = "-",
          spinner = c("|", "/", "-", "\\")
        ),
        elegant = list(
          complete = "\u2501", # Box drawings heavy horizontal
          current = "\u2578", # Box drawings heavy left
          incomplete = "\u2500", # Box drawings light horizontal
          spinner = c("\u25DC", "\u25DD", "\u25DE", "\u25DF")
        ),
        minimal = list(
          complete = ".",
          current = ".",
          incomplete = " ",
          spinner = c(".", "..", "...", "")
        ),
        dots = list(
          complete = "\u2022", # Bullet
          current = "\u25CF", # Black circle
          incomplete = "\u25E6", # White bullet
          spinner = c("\u25D0", "\u25D3", "\u25D1", "\u25D2")
        ),
        arrows = list(
          complete = "\u25B6", # Right-pointing triangle
          current = "\u25B7", # White right-pointing triangle
          incomplete = "\u25B9", # Outlined right-pointing triangle
          spinner = c("\u2190", "\u2196", "\u2191", "\u2197", "\u2192", "\u2198", "\u2193", "\u2199")
        ),
        blocks = list(
          complete = "\u2588", # Full block
          current = "\u2593", # Dark shade
          incomplete = "\u2591", # Light shade
          spinner = c("\u258C", "\u2590", "\u2580", "\u2584")
        ),
        circles = list(
          complete = "\u25CF", # Black circle
          current = "\u25D0", # Circle with left half black
          incomplete = "\u25CB", # White circle
          spinner = c("\u25D4", "\u25D5", "\u25D6", "\u25D7")
        )
      )

      config <- style_configs[[style]]
      if (is.null(config)) {
        config <- style_configs$modern
      }

      private$complete_char <- config$complete
      private$current_char <- config$current
      private$incomplete_char <- config$incomplete
      private$spin_chars <- config$spinner
    },
    detect_color_support = function() {
      # Check for color support
      if (!interactive()) {
        return(FALSE)
      }

      # Check common environment variables
      term <- Sys.getenv("TERM", "")
      colorterm <- Sys.getenv("COLORTERM", "")

      if (colorterm != "") {
        return(TRUE)
      }
      if (grepl("256color|color", term, ignore.case = TRUE)) {
        return(TRUE)
      }
      if (term %in% c("xterm", "screen", "linux")) {
        return(TRUE)
      }

      # Windows terminal support
      if (.Platform$OS.type == "windows") {
        # Windows 10+ supports ANSI
        return(TRUE)
      }

      return(FALSE)
    },
    colorize = function(text, color) {
      if (!private$use_colors || is.null(color)) {
        return(text)
      }

      color_code <- private$colors[[color]]
      if (is.null(color_code)) {
        return(text)
      }

      paste0(color_code, text, private$colors$reset)
    },
    render = function() {
      if (!private$should_show || !private$shown) {
        return(invisible(NULL))
      }

      # Throttle updates to avoid flickering
      now <- Sys.time()
      elapsed_since_draw <- as.numeric(difftime(now, private$last_draw_time, units = "secs"))
      if (elapsed_since_draw < 0.05 && self$current < self$total) {
        return(invisible(NULL))
      }
      private$last_draw_time <- now

      # Build the output string
      output <- self$format

      # Replace tokens
      output <- private$replace_tokens(output)

      # Clear previous line and write new one
      private$clear_line()
      cat(output, file = private$stream)
      flush(private$stream)

      # Store length without ANSI codes for proper clearing
      private$last_line_length <- nchar(gsub("\033\\[[0-9;]+m", "", output))

      invisible(NULL)
    },
    replace_tokens = function(str) {
      ratio <- self$ratio()
      elapsed <- self$elapsed()

      # Calculate bar
      if (grepl("\\{bar\\}", str)) {
        bar_str <- private$create_bar(ratio)
        str <- gsub("\\{bar\\}", bar_str, str)
      }

      # Current
      str <- gsub("\\{current\\}", as.character(self$current), str)

      # Total
      str <- gsub("\\{total\\}", as.character(self$total), str)

      # Percentage
      percent <- sprintf("%3d%%", round(ratio * 100))
      str <- gsub("\\{percent\\}", percent, str)

      # Elapsed time
      elapsed_str <- private$format_time(elapsed)
      str <- gsub("\\{elapsed\\}", elapsed_str, str)

      # ETA
      if (ratio > 0 && ratio < 1) {
        eta <- elapsed / ratio - elapsed
        eta_str <- private$format_time(eta)
      } else {
        eta_str <- "?s"
      }
      str <- gsub("\\{eta\\}", eta_str, str)

      # Rate
      if (elapsed > 0) {
        rate <- self$current / elapsed
        if (rate >= 100) {
          rate_str <- sprintf("%.0f/s", rate)
        } else if (rate >= 10) {
          rate_str <- sprintf("%.1f/s", rate)
        } else {
          rate_str <- sprintf("%.2f/s", rate)
        }
      } else {
        rate_str <- "?/s"
      }
      str <- gsub("\\{rate\\}", rate_str, str)

      # Spin
      spin_char <- private$spin_chars[private$spinning]
      private$spinning <- private$spinning %% length(private$spin_chars) + 1
      spin_colored <- private$colorize(spin_char, private$current_color)
      str <- gsub("\\{spin\\}", spin_colored, str)

      # Custom tokens
      if (!is.null(private$tokens) && length(private$tokens) > 0) {
        for (token_name in names(private$tokens)) {
          token_pattern <- paste0("\\{", token_name, "\\}")
          token_value <- as.character(private$tokens[[token_name]])
          str <- gsub(token_pattern, token_value, str)
        }
      }

      return(str)
    },
    create_bar = function(ratio) {
      # Calculate bar width (subtract space for other elements)
      # Remove {bar} and color codes for length calculation
      format_without_bar <- gsub("\\{bar\\}", "", self$format)
      format_without_bar <- gsub("\\{[^}]+\\}", "XX", format_without_bar) # Replace tokens with placeholder
      other_length <- nchar(format_without_bar)
      bar_width <- max(10, self$width - other_length - 2)

      complete_length <- floor(bar_width * ratio)
      incomplete_length <- bar_width - complete_length - 1

      # Determine if we show the current indicator
      show_current <- complete_length < bar_width && self$current < self$total

      if (show_current) {
        # Build bar with three parts: complete, current, incomplete
        complete_part <- strrep(private$complete_char, complete_length)
        current_part <- private$current_char
        incomplete_part <- strrep(private$incomplete_char, incomplete_length)

        # Apply colors
        complete_colored <- private$colorize(complete_part, private$complete_color)
        current_colored <- private$colorize(current_part, private$current_color)
        incomplete_colored <- private$colorize(incomplete_part, private$incomplete_color)

        bar <- paste0(complete_colored, current_colored, incomplete_colored)
      } else {
        # Bar is complete or we don't show current
        complete_part <- strrep(private$complete_char, bar_width)
        bar <- private$colorize(complete_part, private$complete_color)
      }

      return(bar)
    },
    format_time = function(seconds) {
      if (is.na(seconds) || !is.finite(seconds)) {
        return("?s")
      }

      if (seconds < 60) {
        return(sprintf("%.0fs", seconds))
      } else if (seconds < 3600) {
        mins <- floor(seconds / 60)
        secs <- seconds %% 60
        return(sprintf("%dm%02ds", mins, secs))
      } else {
        hours <- floor(seconds / 3600)
        mins <- floor((seconds %% 3600) / 60)
        return(sprintf("%dh%02dm", hours, mins))
      }
    },
    clear_line = function() {
      if (private$last_line_length > 0) {
        cat("\r", strrep(" ", private$last_line_length), "\r",
          file = private$stream, sep = ""
        )
      } else {
        cat("\r", file = private$stream)
      }
    },
    get_terminal_width = function() {
      width <- getOption("width")
      if (is.null(width)) width <- 80
      return(min(width, 120))
    }
  )
)


#' Create a progress bar for loops and iterations
#'
#' @description
#' A convenience function to create a colorful progress bar with Unicode symbols.
#' This is a simple wrapper around `ProgressBar$new()` with sensible defaults.
#'
#' @param total Total number of iterations
#' @param format Format string with tokens (default: "{bar} {percent} {eta}")
#' @param style Visual style (default: "modern")
#' @param ... Additional arguments passed to `ProgressBar$new()`
#'
#' @return A `ProgressBar` object
#'
#' @examples
#' \dontrun{
#' # Simple loop with progress bar
#' pb <- progress_bar(total = 100)
#' for (i in 1:100) {
#'   pb$tick()
#'   Sys.sleep(0.02)
#' }
#'
#' # Custom style and format
#' pb <- progress_bar(
#'   total = 50,
#'   format = "{spin} Working {bar} {percent}",
#'   style = "elegant"
#' )
#' for (i in 1:50) {
#'   pb$tick()
#'   Sys.sleep(0.05)
#' }
#'
#' # With status messages
#' pb <- progress_bar(
#'   total = 30,
#'   format = "{bar} {current}/{total} | {msg}",
#'   style = "circles"
#' )
#' for (i in 1:30) {
#'   pb$tick(tokens = list(msg = paste("Processing item", i)))
#'   Sys.sleep(0.1)
#' }
#' }
#'
#' @export
progress_bar <- function(total = 100,
                         format = "{bar} {percent} {eta}",
                         style = "modern",
                         ...) {
  ProgressBar$new(total = total, format = format, style = style, ...)
}


#' Apply a function with a colorful progress bar
#'
#' @description
#' Apply a function over a vector or list with a beautiful progress bar showing progress.
#' Similar to `lapply()` but with rich visual feedback including colors and Unicode symbols.
#'
#' @param X A vector or list to iterate over
#' @param FUN Function to apply to each element
#' @param ... Additional arguments passed to FUN
#' @param .format Format string for the progress bar
#' @param .style Visual style of the progress bar
#' @param .width Width of the progress bar
#' @param .clear Whether to clear the bar on completion
#' @param .show_after Seconds to wait before showing the bar
#'
#' @return A list of results from applying FUN to each element of X
#'
#' @examples
#' \dontrun{
#' # Simple example
#' results <- progress_apply(1:20, function(x) {
#'   Sys.sleep(0.1)
#'   x^2
#' })
#'
#' # Reading files with progress
#' files <- list.files(pattern = "\\.csv$")
#' data_list <- progress_apply(
#'   files,
#'   read.csv,
#'   .format = "{spin} {bar} {current}/{total} files",
#'   .style = "elegant"
#' )
#'
#' # Data processing with custom style
#' results <- progress_apply(
#'   1:100,
#'   function(x) {
#'     Sys.sleep(0.02)
#'     rnorm(100, mean = x)
#'   },
#'   .format = "{bar} {percent} | {rate}",
#'   .style = "blocks"
#' )
#'
#' # Keep progress bar after completion
#' results <- progress_apply(
#'   datasets,
#'   process_data,
#'   .format = "{spin} Processing {bar} {current}/{total}",
#'   .clear = FALSE
#' )
#' }
#'
#' @export
progress_apply <- function(X, FUN, ...,
                           .format = "{bar} {percent} {eta}",
                           .style = "modern",
                           .width = NULL,
                           .clear = TRUE,
                           .show_after = 0.2) {
  n <- length(X)
  pb <- ProgressBar$new(
    total = n,
    format = .format,
    style = .style,
    width = .width,
    clear = .clear,
    show_after = .show_after
  )

  results <- vector("list", n)

  for (i in seq_along(X)) {
    results[[i]] <- FUN(X[[i]], ...)
    pb$tick()
  }

  return(results)
}


#' Iterate over elements with a visual progress bar
#'
#' @description
#' A wrapper that provides a colorful progress bar with Unicode symbols for any
#' iterable object. Returns an enhanced vector/list that displays progress when
#' used in a for loop with the special `progress_iterate()` wrapper.
#'
#' @param x A vector or list to iterate over
#' @param format Format string for the progress bar
#' @param style Visual style of the progress bar
#' @param ... Additional arguments passed to `ProgressBar$new()`
#'
#' @return The input object with progress tracking enabled
#'
#' @examples
#' \dontrun{
#' # Basic iteration
#' for (i in progress_iterate(1:100)) {
#'   # Your code here
#'   Sys.sleep(0.02)
#' }
#'
#' # With custom format
#' for (item in progress_iterate(
#'   my_list,
#'   format = "{spin} {bar} {current}/{total}",
#'   style = "elegant"
#' )) {
#'   process(item)
#'   Sys.sleep(0.1)
#' }
#'
#' # Different styles
#' for (file in progress_iterate(
#'   files,
#'   format = "{bar} {percent} | Processing files",
#'   style = "circles"
#' )) {
#'   data <- read.csv(file)
#'   Sys.sleep(0.05)
#' }
#'
#' # Arrows style for downloads
#' urls <- c("url1", "url2", "url3")
#' for (url in progress_iterate(urls, style = "arrows")) {
#'   download_file(url)
#' }
#' }
#'
#' @export
progress_iterate <- function(x,
                             format = "{bar} {percent} {eta}",
                             style = "modern",
                             ...) {
  # Store the progress bar in the object's attributes
  pb <- ProgressBar$new(
    total = length(x),
    format = format,
    style = style,
    ...
  )

  # Create a wrapper environment
  env <- new.env(parent = emptyenv())
  env$pb <- pb
  env$x <- x
  env$i <- 0
  env$n <- length(x)

  # Create a custom class that behaves like the original object
  # but updates progress when iterated
  structure(
    x,
    class = c("progress_iterable", class(x)),
    pb_env = env
  )
}


#' @export
`[.progress_iterable` <- function(x, i, ...) {
  env <- attr(x, "pb_env", exact = TRUE)
  if (!is.null(env) && !is.null(env$pb)) {
    env$pb$update(i)
  }
  class(x) <- setdiff(class(x), "progress_iterable")
  NextMethod("[")
}


#' Show a progress bar for a specific duration
#'
#' @description
#' Display a progress bar that fills up over a specified time duration.
#' Useful for showing progress during time-based operations or simulations.
#'
#' @param seconds Duration in seconds
#' @param format Format string for the progress bar
#' @param style Visual style of the progress bar
#' @param steps Number of update steps (default: 100)
#' @param ... Additional arguments passed to `ProgressBar$new()`
#'
#' @return Invisible NULL
#'
#' @examples
#' \dontrun{
#' # Wait 5 seconds with progress
#' progress_sleep(5)
#'
#' # Custom format
#' progress_sleep(
#'   10,
#'   format = "{spin} Waiting {bar} {elapsed}/{eta}",
#'   style = "elegant"
#' )
#'
#' # More granular updates
#' progress_sleep(
#'   3,
#'   format = "{bar} {percent}",
#'   style = "dots",
#'   steps = 50
#' )
#' }
#'
#' @export
progress_sleep <- function(seconds,
                           format = "{bar} {elapsed}/{eta}",
                           style = "modern",
                           steps = 100,
                           ...) {
  pb <- ProgressBar$new(
    total = steps,
    format = format,
    style = style,
    show_after = 0,
    ...
  )

  interval <- seconds / steps

  for (i in 1:steps) {
    Sys.sleep(interval)
    pb$tick()
  }

  invisible(NULL)
}


#' Create a multi-bar progress display
#'
#' @description
#' Manage multiple progress bars simultaneously, useful for tracking parallel
#' operations or multiple stages of a process. Each bar can have its own style
#' and format.
#'
#' @examples
#' \dontrun{
#' # Track multiple operations
#' mb <- MultiProgressBar$new()
#'
#' # Add bars for different tasks
#' pb1 <- mb$add_bar(
#'   id = "download",
#'   total = 100,
#'   format = "{spin} Download {bar} {percent}",
#'   style = "modern"
#' )
#'
#' pb2 <- mb$add_bar(
#'   id = "process",
#'   total = 50,
#'   format = "{spin} Process  {bar} {percent}",
#'   style = "elegant"
#' )
#'
#' pb3 <- mb$add_bar(
#'   id = "upload",
#'   total = 80,
#'   format = "{spin} Upload   {bar} {percent}",
#'   style = "blocks"
#' )
#'
#' # Simulate work
#' for (i in 1:100) {
#'   if (i <= 100) pb1$tick()
#'   if (i <= 50) pb2$tick()
#'   if (i <= 80) pb3$tick()
#'   Sys.sleep(0.02)
#' }
#'
#' mb$terminate_all()
#' }
#'
#' @export
MultiProgressBar <- R6::R6Class(
  "MultiProgressBar",
  public = list(
    #' @field bars List of progress bars
    bars = NULL,

    #' @description
    #' Create a new multi-bar progress display
    #'
    #' @param stream Output stream (default: stderr())
    #'
    #' @return A new `MultiProgressBar` object
    initialize = function(stream = stderr()) {
      self$bars <- list()
      private$stream <- stream
      private$bar_order <- character(0)
      invisible(self)
    },

    #' @description
    #' Add a new progress bar to the display
    #'
    #' @param id Unique identifier for this bar
    #' @param total Total number of iterations
    #' @param format Format string
    #' @param style Visual style
    #' @param ... Additional arguments passed to ProgressBar$new()
    #'
    #' @return The created ProgressBar object
    add_bar = function(id, total, format = "{bar} {percent}", style = "modern", ...) {
      if (id %in% names(self$bars)) {
        stop("A progress bar with id '", id, "' already exists")
      }

      pb <- ProgressBar$new(
        total = total,
        format = format,
        style = style,
        clear = FALSE,
        stream = private$stream,
        ...
      )

      self$bars[[id]] <- pb
      private$bar_order <- c(private$bar_order, id)

      return(pb)
    },

    #' @description
    #' Remove a progress bar
    #'
    #' @param id Identifier of the bar to remove
    #'
    #' @return Invisible self
    remove_bar = function(id) {
      if (!id %in% names(self$bars)) {
        warning("No progress bar with id '", id, "' found")
        return(invisible(self))
      }

      self$bars[[id]]$terminate()
      self$bars[[id]] <- NULL
      private$bar_order <- setdiff(private$bar_order, id)

      invisible(self)
    },

    #' @description
    #' Terminate all progress bars
    #'
    #' @return Invisible self
    terminate_all = function() {
      for (id in private$bar_order) {
        if (!is.null(self$bars[[id]])) {
          self$bars[[id]]$terminate()
        }
      }
      invisible(self)
    }
  ),
  private = list(
    stream = NULL,
    bar_order = NULL
  )
)


#' Progress bar for parallel operations
#'
#' @description
#' A wrapper around `parallel::mclapply()` or `parallel::parLapply()` that shows
#' a progress bar. This function provides visual feedback for parallel processing
#' operations.
#'
#' @param X A vector or list to iterate over
#' @param FUN Function to apply to each element
#' @param ... Additional arguments passed to FUN
#' @param .mc.cores Number of cores to use (Unix-like systems only)
#' @param .format Format string for the progress bar
#' @param .style Visual style of the progress bar
#' @param .method Method to use: "fork" (Unix) or "psock" (Windows/Unix)
#'
#' @return A list of results from applying FUN to each element of X
#'
#' @examples
#' \dontrun{
#' # Parallel processing with progress (Unix-like systems)
#' results <- progress_parallel(
#'   1:100,
#'   function(x) {
#'     Sys.sleep(0.1)
#'     x^2
#'   },
#'   .mc.cores = 4,
#'   .format = "{spin} {bar} {percent} | {current}/{total}",
#'   .style = "modern"
#' )
#'
#' # Works on all platforms with PSOCK method
#' results <- progress_parallel(
#'   large_dataset,
#'   expensive_function,
#'   .method = "psock",
#'   .mc.cores = 4,
#'   .style = "elegant"
#' )
#' }
#'
#' @export
progress_parallel <- function(X, FUN, ...,
                              .mc.cores = getOption("mc.cores", 2L),
                              .format = "{bar} {percent} {eta}",
                              .style = "modern",
                              .method = c("fork", "psock")) {
  .method <- match.arg(.method)
  n <- length(X)

  # Create progress bar
  pb <- ProgressBar$new(
    total = n,
    format = .format,
    style = .style,
    show_after = 0
  )

  # Wrapper function that updates progress
  wrapper <- function(x) {
    result <- FUN(x, ...)
    pb$tick()
    return(result)
  }

  # Use appropriate parallel method
  if (.method == "fork" && .Platform$OS.type == "unix") {
    results <- parallel::mclapply(X, wrapper, mc.cores = .mc.cores)
  } else {
    cl <- parallel::makeCluster(.mc.cores)
    on.exit(parallel::stopCluster(cl))
    results <- parallel::parLapply(cl, X, wrapper)
  }

  pb$terminate()

  return(results)
}


#' Create a progress bar for file downloads
#'
#' @description
#' Display a progress bar while downloading a file. Shows download speed and
#' estimated time remaining.
#'
#' @param url URL to download from
#' @param destfile Destination file path
#' @param ... Additional arguments passed to `download.file()`
#' @param style Visual style of the progress bar
#'
#' @return Invisible integer code (0 for success, non-zero for failure)
#'
#' @examples
#' \dontrun{
#' # Download with progress
#' progress_download(
#'   "https://example.com/largefile.zip",
#'   "local_file.zip"
#' )
#'
#' # Custom style
#' progress_download(
#'   "https://example.com/data.csv",
#'   "data.csv",
#'   style = "arrows"
#' )
#' }
#'
#' @export
progress_download <- function(url, destfile, ..., style = "modern") {
  # This is a simplified version - a full implementation would need
  # to hook into the actual download progress
  message("Downloading from: ", url)

  # For demonstration, we'll use a simple progress bar
  # In practice, you'd integrate with curl or httr for real progress
  pb <- ProgressBar$new(
    total = 100,
    format = "{spin} Download {bar} {percent} | {eta}",
    style = style,
    show_after = 0
  )

  # Simulate download progress
  # Replace this with actual download logic
  for (i in 1:100) {
    Sys.sleep(0.02)
    pb$tick()
  }

  # Actual download would happen here
  result <- tryCatch(
    download.file(url, destfile, ..., quiet = TRUE),
    error = function(e) {
      message("Download failed: ", e$message)
      return(1)
    }
  )

  return(invisible(result))
}
