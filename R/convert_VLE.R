#' Convert VLE data format
#'
#' Convert the format of the Virtual Learning Environment (VLE) dataset from total view counts to binary, standardised or logarithmic view count data
#'
#' @param data VLE dataset to be converted. The VLE dataset should consist of student_id as the first column followed by total view counts in the remaining columns.
#' @param conversion Type of conversion to implement, either \code{"binary"}, \code{"standardise"},
#' or \code{"logarithmic"}.
#'
#' @return  Two tibbles are returned: 1) original_data, and 2) converted_data.
#'
#' @section original_data tibble:
#' A tibble of the original data inputted into the function.
#'
#' @section converted_data tibble:
#' A tibble where all columns, except the first column, have been changed from total view counts to either binary, standardised or logarithmic view count data
#'
#' @seealso
#' \code{\link{dataset_VLE_time}} or \code{\link{dataset_VLE_activity}} for obtaining the VLE dataset needed for the input data
#'
#' @export
#' @examples
#' VLE_data = dataset_VLE_activity(module = "BBB",
#' presentation = "2013J", repeat_students = "remove",
#' week_begin = 1, week_end = 39, example_data = TRUE)$resource_data
#' convert_VLE(VLE_data, conversion = "standardise")
#'
#'\dontrun{
#' VLE_data = dataset_VLE_time(module = "BBB",
#' presentation = "2013J", repeat_students = "remove", example_data=TRUE)$weekly_data
#' convert_VLE(VLE_data, conversion = "binary")}
convert_VLE = function(data, conversion = c("binary", "standardise", "logarithmic")){

  data_converted = data[,2:ncol(data)]

  # For matching
  conversion = match.arg(conversion)

  # convert to binary data
  if(conversion == "binary"){

    data_converted[data_converted > 0] <- 1
    colnames(data_converted) = paste0(colnames(data[,2:ncol(data)]), "B")
    data_converted = cbind.data.frame(data[,1], data_converted)

    # standardise data
  }else if(conversion == "standardise"){

    data_converted = scale(data_converted)
    colnames(data_converted) = paste0(colnames(data[,2:ncol(data)]), "S")
    data_converted = cbind.data.frame(data[,1], data_converted)

  }else{

    data_converted = log(data_converted + 1)
    colnames(data_converted) = paste0(colnames(data[,2:ncol(data)]), "L")
    data_converted = cbind.data.frame(data[,1], data_converted)

  }

  return(list(original_data = tibble(data),
              converted_data = tibble(data_converted)))

}
