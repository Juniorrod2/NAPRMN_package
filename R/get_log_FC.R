#' Compute Log2 Fold Change Between Two Groups
#'
#' This function calculates fold change (FC), log2 fold change (log2FC),
#' and percent variation between two groups in a dataset.
#' The function expects exactly two groups in the specified grouping variable
#' and computes group means for a selected variable before calculating FC.
#'
#' @param dataset A data frame or tibble containing the data.
#'   Must include the variable to be summarized and the grouping variable.
#'
#' @param x A character referring to the numeric variable
#'   for which the fold change should be calculated (e.g., \code{"Intensity")}).
#'
#' @param grouping_variable grouping_variable. A character specifying the
#'   grouping variable that defines the two comparison groups.
#'   It must contain exactly two unique levels.
#'
#' @param comparison A character string specifying the comparison direction.
#'   Options are:
#'   - `"a/b"` (default): FC = mean(group A) / mean(group B)
#'   - `"b/a"`: FC = mean(group B) / mean(group A)
#'   Any other value returns an error message.
#'
#' @returns
#'
#' A data frame containing:
#' - group means for each level of the grouping variable
#' - fold change (FC)
#' - log2 fold change (LogFC)
#' - percent variation relative to the denominator group
#'
#' If the grouping variable does not contain exactly two groups,
#' the function returns \code{NULL} and prints an informative message.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(dplyr)
#' library(tidyr)
#'
#' # Example: calculate log2 fold change per NMR bin
#'
#' test_data <- test_dataset |>
#'   tidyr::gather(-atleta:-sample_year, key = "bin", value = "intensity") |>
#'   dplyr::group_by(bin)
#'
#' # Compute log2 fold change between two groups defined in "LIP_adq"
#' test_data |>
#'   get_log_fc(
#'     x = "intensity",
#'     grouping_variable = "LIP_adq",
#'     comparison = "a/b"
#'   )
#' }
#'
get_log_fc <- function(dataset,
                       x,
                       grouping_variable,
                       comparison = "a/b") {


  if (dim(dplyr::distinct(dplyr::ungroup(dataset), dplyr::across(!!grouping_variable)))[1] == 2) {
    FC_data <- dplyr::group_by(dataset, dplyr::across(!!grouping_variable), .add = T) |>
      dplyr::summarise(dplyr::across(!!x, mean)) |>
      tidyr::spread(key = !!grouping_variable, value = !!x)

    if (comparison == "a/b") {
      FC_data <- dplyr::mutate(
        FC_data,
        FC = unlist(dplyr::across(colnames(FC_data)[3]) / dplyr::across(colnames(FC_data)[2])),
        LogFC = log2(FC),
        Percentual_variation = (2^LogFC - 1) * 100
      )
    } else {
      if (comparison == "b/a") {
        FC_data <- dplyr::mutate(
          FC_data,
          FC = unlist(dplyr::across(colnames(FC_data)[2]) / dplyr::across(colnames(FC_data)[3])),
          LogFC = log2(FC),
          Percentual_variation = (2^LogFC - 1) * 100
        )
      } else {
        print("Invalid comparison design")
        return(NULL)
      }
    }

  } else {
    print("Log2Fc can only be calculated for two groups! Check if the correct/existent grouping variable was supplied")
    return(NULL)
  }
}
