#' @title Displays a legend of status and progress symbols
#'
#' @description Prints a legend of common symbols used in R package console
#' messages, using their Unicode escape codes.
#'
#' @return Prints the messages to the console.
#' @examples
#' \dontrun{
#' show_symbol_legend()
#' }
#'
#' @export
show_symbol_legend <- function() {
  cat("\n")
  cat("--- Package Console Symbol Legend ---\n")
  cat("\n")

  # --- Status Messages ---
  cat("STATUS MESSAGES:\n")
  cat(" \u2705 Success/Approved: Task completed successfully.\n")
  cat(" \u274c Error/Failure: Task could not be completed due to an issue.\n")
  cat(" \u26a0\ufe0f Warning: Task completed, but with potential issues or anomalies.\n")
  cat(" \u26d4\ufe0f Forbidden/Stopped: Action is not valid or has been restricted.\n")
  cat("\n")

  # --- Progress and Actions ---
  cat("PROGRESS & ACTIONS:\n")
  cat(" \u2699\ufe0f Configuration/Setup: Starting a configuration or pre-processing step.\n")
  cat(" \U0001f680 Rocket Launch/Quick Start: Initiating a critical process or deployment.\n")
  cat(" \u23f3 In Progress/Waiting: Task is currently running and may take time.\n")
  cat(" \u231b\ufe0f Time Completed: The waiting period or process has finished.\n")
  cat(" \U0001f50d Search/Investigation: Executing a query or data exploration function.\n")
  cat(" \U0001f4c1 Directory/File: Creating or accessing a file path/folder.\n")
  cat(" \U0001f4be Save/Write: Data has been successfully written to disk.\n")
  cat("\n")

  # --- Informational Messages ---
  cat("INFORMATIONAL:\n")
  cat(" \u2139\ufe0f Information: General informational or debugging message.\n")
  cat(" \U0001f4a1 Hint/Idea: A useful tip or additional note for the user.\n")
  cat(" \U0001f389 Celebration: Milestone reached or installation complete!\n")
  cat("\n")
  cat("--------------------------------------\n")
}
