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

#' Create a .bib File from an Excel Sheet with DOI-Based Unique Keys
#'
#' This function reads a list of DOIs from an Excel file, fetches their BibTeX
#' entries, and rewrites the citation key for each entry to be a sanitized
#' version of its own DOI. This guarantees that every key is unique and traceable.
#'
#' @param excel_path A string with the path to the .xlsx or .xls file.
#' @param doi_column_name A string with the exact name of the column containing
#'   the DOIs.
#' @param output_bib_path A string with the desired file path for the output
#'   .bib file (e.g., "references.bib").
#'
#' @return This function does not return a value but prints messages to the
#'   console and writes a .bib file to the specified path.
#'
#' @export
#' @importFrom readxl read_excel
#' @importFrom rcrossref cr_cn
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
  # --- 1. Validate inputs and read the Excel file ---
  if (!file.exists(excel_path)) {
    stop("Error: Excel file not found at path: ", excel_path)
  }
  message("Reading Excel file...")
  df <- readxl::read_excel(excel_path)
  if (!doi_column_name %in% names(df)) {
    stop("Error: Column '", doi_column_name, "' not found in the Excel file.")
  }

  # --- 2. Extract and clean the list of DOIs ---
  dois <- df[[doi_column_name]]
  dois <- na.omit(dois)
  dois <- dois[dois != ""]
  message(paste("Found", length(dois), "valid DOIs to process..."))

  # --- 3. Loop through DOIs and generate corrected BibTeX entries ---
  final_bib_entries <- c() # To store the final, corrected BibTeX strings

  for (doi in dois) {
    cat(paste("Processing:", doi, "\n"))

    # Fetch the BibTeX entry from CrossRef
    bib_entry <- tryCatch(
      {
        rcrossref::cr_cn(dois = doi, format = "bibtex")
      },
      error = function(e) {
        warning(paste("Could not fetch entry for DOI:", doi, "-", e$message))
        return(NULL)
      }
    )

    if (is.null(bib_entry)) {
      next
    }

    # --- DOI-Based Unique Key Generation Logic ---

    # 1. Generate a new, safe, and unique key directly from the DOI
    #    Replace all non-alphanumeric characters with hyphens
    unique_key_from_doi <- gsub("[^A-Za-z0-9]", "-", doi)

    # 2. Remove leading/trailing hyphens and collapse multiple consecutive hyphens
    unique_key_from_doi <- gsub("-+", "-", unique_key_from_doi)
    unique_key_from_doi <- sub("^-+|-+$", "", unique_key_from_doi)

    # 3. Extract the entire first line of the BibTeX entry (the @type{key, part)
    #    and replace it with our new key
    first_line <- sub(
      "@(.*?)\\{(.*?),",
      paste0("@\\1{", unique_key_from_doi, ","),
      bib_entry
    )

    # 4. If the first line replacement didn't work properly, use a more robust approach
    if (grepl(unique_key_from_doi, first_line)) {
      modified_bib_entry <- first_line
    } else {
      # Fallback: split entry into lines, modify first line, rejoin
      bib_lines <- strsplit(bib_entry, "\n")[[1]]
      bib_lines[1] <- sub("@(.*?)\\{(.*?),", paste0("@\\1{", unique_key_from_doi, ","), bib_lines[1])
      modified_bib_entry <- paste(bib_lines, collapse = "\n")
    }

    final_bib_entries <- c(final_bib_entries, modified_bib_entry)

    # --- End of Logic ---
  }

  # --- 4. Write all corrected BibTeX entries to the output file ---
  full_bib_text <- paste(final_bib_entries, collapse = "\n\n")
  writeLines(full_bib_text, output_bib_path)

  message(paste("\nProcess complete! File created at:", output_bib_path))
  message(paste("A total of", length(final_bib_entries), "unique entries were generated."))
}
