#' Filter out genes with low expression in RNAseq data
#'
#' This function checks for and filters out low expressing genes in RNAseq
#' expression data. Based on user input, the function returns an expression
#' data frame that contains genes expressed above a minimum expression threshold
#' across a certain percentage of samples. Upon completion, a message
#' summarizing how many genes were removed is also output.
#'
#' @param df expression data. A data frame in which columns are genes and
#' rows are samples.
#'
#' @param min_expr minimum expression. The minimum value that a gene must be
#' expressed across the percentage of samples selected. If no user input is
#' provided the default is 10.
#'
#' @param percent_cutoff percentage cutoff. The percent of samples that minimum
#' gene expression (min_expr) is required. If no user input is provided the
#' default is 90.
#'
#' @param metric metric/type of data. The type of data (e.g. FPKM, RSEM,
#' raw count, etc.) the expression data is formatted in. This does not change
#' how analysis is done, it simply makes the summary message of the function
#' relevant to your specific analysis. If no user input is provided the the
#' default is "unit".
#'
#' @export
#'
# Check if rownames are sample IDs
check_rownames <- function(df) {
  if (!is.character(rownames(df))) {
    stop("Make sure rownames in dataframe are sample IDs.")
  }
}

# Calculate the rounded cutoff
get_rounded_cutoff <- function(samples_num, percent_cutoff) {
  dec_cutoff <- percent_cutoff / 100
  sample_cutoff <- dec_cutoff * samples_num
  result <- sample_cutoff %>%
    ceiling()
  return(result)
}

# Remove genes based on the given parameters
remove_low_genes <- function(df_t, min_expr, rounded_cutoff) {
  result <- df_t %>%
    dplyr::mutate(samples_w_min_expr = rowSums(. >= min_expr)) %>%
    dplyr::filter(samples_w_min_expr >= rounded_cutoff) %>%
    dplyr::select(-samples_w_min_expr)
  return(result)
}

# Print the message with removed genes count
print_message <- function(removed_genes_count, min_expr, metric, percent_cutoff, rounded_cutoff, samples_num) {
  message_text <- glue::glue("ATTENTION: A total of {removed_genes_count} genes were removed. ",
                             "This was based on your parameters that stated a minimum of {min_expr} {metric} ",
                             "must be in at least {percent_cutoff}% (≥{rounded_cutoff}) ",
                             "of the {samples_num} samples provided.")

  wrapped_message <- strwrap(message_text, width = getOption("width"))
  message(cat(wrapped_message, sep = "\n"))
}

filter_low_genes <- function(df,
                             min_expr = 10,
                             percent_cutoff = 90,
                             metric = "units") {
  check_rownames(df)

  samples_num <- nrow(df)
  rounded_cutoff <- get_rounded_cutoff(samples_num, percent_cutoff)

  df_t <- df %>% t() %>% as.data.frame()

  df_t_filt <- df_t %>%
    remove_low_genes(min_expr, rounded_cutoff)

  number_of_rm_genes <- nrow(df_t) - nrow(df_t_filt)

  df_filt_final <- df_t_filt %>%
    t() %>%
    as.data.frame() %>%
    mutate(across(everything(), as.numeric)) # convert to numeric

  print_message(number_of_rm_genes, min_expr, metric, percent_cutoff, rounded_cutoff, samples_num)

  return(df_filt_final)
}
