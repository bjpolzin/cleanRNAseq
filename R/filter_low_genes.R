#' Filter out genes with low expression in RNAseq data
#'
#' This function checks data for individual genes that express below a certain
#' threshold across a given percentage of samples, filters them out, and returns
#' a data frame with the genes that successfully met the user-provided criteria.
#' Upon completion, a message summarizing how many genes were removed is also
#' output.
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
filter_low_genes <- function(df, min_expr = 10,
                             percent_cutoff = 90, metric = "units") {
  if (is.character(rownames(df)) == F) {
    stop("Make sure rownames in dataframe are sample IDs.")
  }
  else {
    samples_num <- nrow(df)
    dec_cutoff <- percent_cutoff/100
    df_t_orig <- df %>%
      t() %>%
      as.data.frame()
    df_t <- df_t_orig
    df_t$samples_w_min_expr <- rowSums(df_t >= min_expr)
    cutoff <- dec_cutoff * samples_num
    rounded_cutoff <- ceiling(cutoff)
    df_t_filt <- df_t %>%
      dplyr::filter(samples_w_min_expr >= rounded_cutoff)
    number_of_rm_genes <- nrow(df_t) - nrow(df_t_filt)
    df_t_filt_final <- df_t_filt %>%
      dplyr::select(-samples_w_min_expr)
    df_filt <- df_t_filt_final %>%
      t() %>%
      as.data.frame()
    all_gene_cols <- colnames(df_filt)
    df_filt_final <- df_filt %>%
      dplyr::mutate(across(all_of(all_gene_cols), as.numeric)) # convert to numeric
    message("ATTENTION: A total of ",
            number_of_rm_genes,
            " genes were removed. This was based on your parameters that stated a minimum of ",
            min_expr,
            " ",
            metric,
            " must be in at least ",
            percent_cutoff,
            "% (≥",
            rounded_cutoff,
            ") of the ",
            samples_num,
            " samples provided.")
return(df_filt_final)
    }
}
