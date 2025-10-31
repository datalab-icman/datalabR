#' @title Get a BibTeX entry from a DOI
#' @description Fetches a bibliographic entry in BibTeX format using a Digital Object Identifier (DOI).
#'
#' @param doi A character string representing the DOI of the article.
#' @return A character string with the entry in BibTeX format.
#'         Returns \code{NULL} if the DOI is invalid, not found, or the request fails.
#' @importFrom rcrossref cr_cn
#' @export
#' @examples
#' \dontrun{
#' entry <- get_bibtex_entry("10.1126/science.1172133")
#' if (!is.null(entry)) {
#'   cat(entry)
#' }
#' }
get_bibtex_entry <- function(doi) {
  if (is.na(doi) || doi == "") {
    return(NULL)
  }
  tryCatch(
    {
      bib_entry <- rcrossref::cr_cn(dois = doi, format = "bibtex")
      return(bib_entry)
    },
    error = function(e) {
      warning(paste("Could not retrieve entry for DOI:", doi, "-", e$message))
      return(NULL)
    }
  )
}

#' @title Create a .bib File from an Excel Sheet of DOIs
#' @description Reads a list of DOIs from a specified column in an Excel file,
#' fetches their BibTeX entries, and compiles them into a single .bib file.
#'
#' @param excel_path Path to the .xlsx or .xls file.
#' @param doi_column_name Name of the column in the Excel file that contains the DOIs.
#' @param output_bib_path Path and name for the output .bib file (e.g., "references.bib").
#' @importFrom readxl read_excel
#' @export
#' @examples
#' \dontrun{
#' # First, create a dummy Excel file for demonstration
#' if (requireNamespace("writexl", quietly = TRUE)) {
#'   dummy_df <- data.frame(
#'     Title = c("Article One", "Article Two"),
#'     DOI = c("10.1126/science.1172133", "10.1073/pnas.0806414105")
#'   )
#'   writexl::write_xlsx(dummy_df, "my_articles.xlsx")
#'
#'   # Now, run the function
#'   create_bib_file_from_excel(
#'     excel_path = "my_articles.xlsx",
#'     doi_column_name = "DOI",
#'     output_bib_path = "references.bib"
#'   )
#' }
#' }
create_bib_file_from_excel <- function(excel_path, doi_column_name, output_bib_path) {
  # --- 1. Read and validate data ---
  if (!file.exists(excel_path)) {
    stop("Excel file not found at path: ", excel_path)
  }

  message("Reading Excel file...")
  df <- readxl::read_excel(excel_path)

  if (!doi_column_name %in% names(df)) {
    stop("Column '", doi_column_name, "' not found in the Excel file.")
  }

  # --- 2. Extract and clean DOIs ---
  dois <- df[[doi_column_name]]
  dois <- na.omit(dois) # Remove NA values
  dois <- dois[dois != ""] # Remove empty strings

  message(paste("Found", length(dois), "valid DOIs to process..."))

  # --- 3. Fetch all BibTeX entries ---
  # lapply will apply the get_bibtex_entry function to each DOI.
  bib_list <- lapply(dois, function(doi) {
    cat(paste("Processing:", doi, "\n")) # Show progress
    get_bibtex_entry(doi)
  })

  # --- 4. Combine and save the .bib file ---
  # unlist() converts the list to a vector of strings.
  full_bib_text <- paste(unlist(bib_list), collapse = "\n\n")

  # Write the content to the output file.
  writeLines(full_bib_text, output_bib_path)

  message(paste("\nProcess complete! File created at:", output_bib_path))
}
