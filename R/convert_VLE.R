#' Converts the VLE data format
#'
#' Converts the data format of the Virtual Learning Environment (VLE) data set from total view counts to binary, standardised or logarithmic view count data.
#'
#' @param data VLE data set to be converted. The VLE data set should consist of student_id
#' as the first column followed by total view counts per time period/activity in
#' each of the remaining columns.
#' @param conversion type of conversion to implement, either `"binary"`, `"standardise1"` (standardises each variable individually by using the scale function),
#' `"standardise2"` (performs global standardisation of the data set),
#' or `"logarithmic"` (where 1 is added first to the data).
#'
#' @return  Two tibbles are returned: 1) original_data, and 2) converted_data.
#'
#' @section original_data tibble:
#' A tibble of the original data inputted into the function.
#'
#' @section converted_data tibble:
#' A tibble where all columns, except the first column, have been changed from total view counts
#' to either binary, standardised, globally standardised or logarithmic view count data.
#'
#' @seealso
#' [dataset_VLE_time()] or [dataset_VLE_activity()] for obtaining the VLE data set needed for the input data.
#'
#' @export
#' @importFrom stats "sd"
#' @examples
#' VLE_data = dataset_VLE_activity(example_data = TRUE)$resource_data
#' convert_VLE(VLE_data, conversion = "standardise1")
#'
#' VLE_data = dataset_VLE_time(example_data = TRUE)$weekly_data
#' convert_VLE(VLE_data, conversion = "logarithmic")
convert_VLE = function(data, conversion = c("binary", "standardise1", "standardise2", "logarithmic")){

  data_converted = data[,2:ncol(data)]

  # For matching
  conversion = match.arg(conversion)

  # convert to binary data
  if(conversion == "binary"){

    data_converted[data_converted > 0] <- 1
    data_converted = cbind.data.frame(data[,1], data_converted)

    # standardise data
  }else if(conversion == "standardise1"){

    data_converted = scale(data_converted)
    data_converted = cbind.data.frame(data[,1], data_converted)

  }else if(conversion == "standardise2"){

    std_df = as.matrix(data_converted) %>% sd()
    mean_df = as.matrix(data_converted) %>% mean()
    data_converted = (data_converted - mean_df)/std_df
    data_converted = cbind.data.frame(data[,1], data_converted)

  }else{

    data_converted = log(data_converted + 1)
    data_converted = cbind.data.frame(data[,1], data_converted)

  }

  return(list(original_data = tibble(data),
              converted_data = tibble(data_converted)))

}
