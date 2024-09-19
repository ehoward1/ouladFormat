#' Returns the combined formatted OULAD
#'
#' Combines multiple OULAD .csv files into one tibble that is formatted for data analysis and
#' where each row represents a unique student.
#'
#' @param module name of the module to be included, either `"AAA"`, `"BBB"`, `"CCC"`,
#' `"DDD"`, `"EEE"`, `"FFF"` or `"GGG"`.
#' @param presentation name of the semester of the module to be included, either `"2013B"`,
#' `"2014B"`, `"2013J"`, `"2014J"`, or `"All"`.
#' `"B"` indicates a February start time whereas `"J"` indicates an October start time. `"All"` indicates that all presentations of the module will be included in the returned data.
#' @param repeat_students indicator of whether students who had previous attempts at the module should be removed, either `"remove"` or `"keep"`.
#' When presentation is set to `"All"`, this value is set to `"remove"`.
#' @param withdrawn_students indicator of whether students who had withdrawn from the module should be removed, either `"remove"` or `"keep"`.
#' Students are removed based on whether they have withdrawn by the `"week_end"` value.
#' @param demographics logical. Indicates whether demographic data is included in the outputted data set.
#' @param registration logical. Indicates whether registration data is included in the outputted data set.
#' @param VLE indicates whether VLE data is included in the outputted data set
#' (default is `"omit"` - VLE data not to be included).
#' If included, the VLE data can be returned based on `"weekly"` VLE interactions, `"daily"` VLE
#' interactions or interactions according to Open University activities (`"activity"`).
#' The VLE data can also be returned as classified under a
#' specific learning model, either `"FSLM"`, `"FSLSM"`, `"OLS"`, or `"VARK"`.
#' @param VLE_clicks indicates the format that the VLE data should be returned as, either `"total"` views (default),
#' `"binary"` views, `"standardise1"` (standardises each variable individually by using the scale function),
#' `"standardise2"` (performs global standardisation of the data set), or `"logarithmic"` values.
#' @param week_begin the first semester week of VLE and assessment data to be included in formatted data. Depending on the module presentation, students
#' started to view activities four weeks prior to the initial module start date. Weeks prior to the initial module start
#' are indicated by a negative integer.
#' @param week_end the last semester week of VLE and assessment data to be included in the formatted data.
#' Week 39 is the last week material was viewed (and earlier in some module presentations).
#' This parameter is also used to remove withdrawn students.
#' @param assessment logical. Indicates whether assessment data is included in the outputted data set.
#' @param na.rm logical. Indicates whether NAs should be omitted from the average continuous assessment calculations
#' or treated as zeroes (default). This calculation only includes continuous assessment that
#' was due between the period set by `"week_begin"` and `"week_end"` inclusive,
#' and only occurs when a specific module presentation is requested (e.g., 'BBB 2013J').
#' @param example_data logical. Indicates whether to run a subset of the VLE data as an example.
#'
#' @returns Returns the inputs specified for whether assessment, demographics, registration and VLE variables are to be included,
#' as well as the other inputs. Also, one `tibble` (object of class `tbl_df`) is returned, dataset_combined.
#'
#' @section dataset_combined tibble:
#' A `tibble` where each row represents a unique student. Depending on the inputs specified,
#' the `tibble` includes assessment, demographics, registration and VLE data for each student.
#'
#' @export
#'
#' @seealso
#' For more information on different inputs and variables in the dataset_combined `tibble`, see:
#' \itemize{
#' \item{ [dataset_assessment()] for information on the assessment performance data,}
#' \item{[dataset_demographics()] for information on the demographics data,}
#' \item{[dataset_registration()] for information on the registration data,}
#' \item{[dataset_VLE_time()] for information on the VLE daily or weekly data,}
#' \item{[dataset_VLE_activity()] for information on the VLE activity data,}
#' \item{[VLE_learning_classification()] for information on the VLE activities classified under a learning model, and}
#' \item{[convert_VLE()] for information on transforming the data type of the VLE data.}
#'}
#'
#' @references
#' Kuzilek, J., Hlosta, M., & Zdrahal, Z. (2017). Open university learning analytics dataset. Scientific Data
#' volume 4 , (pp. 1–8). https://doi.org/10.1038/sdata.2017.171.
#'
#' @examples
#' # Uses subset of the VLE data set for example
#' combined_dataset(module = "AAA", presentation = "2013J",
#' repeat_students = "remove", withdrawn_students = "remove",
#' demographics = TRUE, registration = TRUE,
#' assessment = TRUE, na.rm = FALSE,
#' VLE = "weekly", VLE_clicks = "total",
#' example_data = TRUE)
#' \donttest{
#' # Slow to run as it loads the full VLE data set
#' combined_dataset(module = "BBB", presentation = "2013J",
#' repeat_students = "remove", withdrawn_students = "remove",
#' demographics = TRUE, registration = FALSE,
#' assessment = TRUE, na.rm = FALSE,
#' VLE = "activity", VLE_clicks = "standardise1",
#' week_begin = -4, week_end = 14, example_data = FALSE)}
combined_dataset = function(module = c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG"),
                            presentation = c("2013J", "2014J", "2013B", "2014B", "All"),
                            repeat_students = c("remove", "keep"),
                            withdrawn_students = c("remove", "keep"),
                            demographics = FALSE,
                            registration = FALSE,
                            VLE = c("omit","daily", "weekly", "activity", "FSLM",
                                    "FSLSM", "OLS" ,"VARK"),
                            VLE_clicks = c("total", "binary",
                                           "standardise1", "standardise2",
                                           "logarithmic"),
                            week_begin = -4, week_end = 39,
                            assessment = FALSE,
                            na.rm = FALSE,
                            example_data = FALSE){

  # For matching inputs
  VLE = match.arg(VLE)
  module = match.arg(module)
  presentation = match.arg(presentation)
  repeat_students = match.arg(repeat_students)
  withdrawn_students = match.arg(withdrawn_students)
  VLE_clicks = match.arg(VLE_clicks)

  # Bind variables
  date_unregistration = date_registration = code_module = code_presentation = NULL

  if(example_data == TRUE){
    module = "AAA"
    presentation="2013J"
  }

  if(demographics == FALSE & assessment == FALSE & registration == FALSE & VLE == "omit"){
    message("Neither demographics, assessment, registration or VLE data have been included")
  }

  # For dataset_VLE_time function to work, repeating students cannot be present in it
  if(presentation == "All"){
    repeat_students = "remove"
    message("Repeat students are always removed when presentation set to All")
  }

  dataset_combined = dataset_registration(module, presentation, repeat_students)$studentRegistration

  if(demographics == TRUE){
    demographics_data = dataset_demographics(module, presentation, repeat_students)$studentInfo
    dataset_combined = merge(dataset_combined, demographics_data, by = c("id_student", "code_module", "code_presentation"))
  }

  if(assessment == TRUE){
      assessment_data = dataset_assessment(module, presentation, repeat_students, week_begin, week_end, na.rm)$assessment_performance
      if(!is.null(assessment_data)){dataset_combined = merge(dataset_combined, assessment_data, by = "id_student")}
    }

  # Remove withdrawn students
  if(withdrawn_students == "remove"){
    withdrawal_day = ifelse(week_end > 0, (week_end*7)-1, ((week_end+1)*7)-1)
    dataset_combined = filter(dataset_combined, date_unregistration > withdrawal_day | is.na(date_unregistration))
  }

  if(VLE == "daily"){

    VLE_data = dataset_VLE_time(module, presentation, repeat_students, week_begin, week_end, example_data)$daily_data

    if(VLE_clicks == "binary"){
      VLE_data = convert_VLE(VLE_data, "binary")$converted_data
    }else if(VLE_clicks == "standardise1"){
      VLE_data = convert_VLE(VLE_data, "standardise1")$converted_data
    }else if(VLE_clicks == "standardise2"){
      VLE_data = convert_VLE(VLE_data, "standardise2")$converted_data
    }else if(VLE_clicks == "logarithmic"){
      VLE_data = convert_VLE(VLE_data, "logarithmic")$converted_data
    }else{
      VLE_data = VLE_data
    }

  }else if(VLE == "weekly"){

    VLE_data = dataset_VLE_time(module, presentation, repeat_students, week_begin, week_end, example_data)$weekly_data

    if(VLE_clicks == "binary"){
      VLE_data = convert_VLE(VLE_data, "binary")$converted_data
    }else if(VLE_clicks == "standardise1"){
      VLE_data = convert_VLE(VLE_data, "standardise1")$converted_data
    }else if(VLE_clicks == "standardise2"){
      VLE_data = convert_VLE(VLE_data, "standardise2")$converted_data
    }else if(VLE_clicks == "logarithmic"){
      VLE_data = convert_VLE(VLE_data, "logarithmic")$converted_data
    }else{
      VLE_data = VLE_data
    }

  }else if(VLE == "activity"){

    VLE_data = dataset_VLE_activity(module, presentation, repeat_students, week_begin, week_end, example_data)$resource_data

    if(VLE_clicks == "binary"){
      VLE_data = convert_VLE(VLE_data, "binary")$converted_data
    }else if(VLE_clicks == "standardise1"){
      VLE_data = convert_VLE(VLE_data, "standardise1")$converted_data
    }else if(VLE_clicks == "standardise2"){
      VLE_data = convert_VLE(VLE_data, "standardise2")$converted_data
    }else if(VLE_clicks == "logarithmic"){
      VLE_data = convert_VLE(VLE_data, "logarithmic")$converted_data
    }else{
      VLE_data = VLE_data
    }

  }else if(VLE == "FSLM"){

    VLE_data = dataset_VLE_activity(module, presentation, repeat_students, week_begin, week_end, example_data)$resource_data
    VLE_data = VLE_learning_classification(VLE_data, classification = "FSLM")$VLE_classified_data

    if(VLE_clicks == "binary"){
      VLE_data = convert_VLE(VLE_data, "binary")$converted_data
    }else if(VLE_clicks == "standardise1"){
      VLE_data = convert_VLE(VLE_data, "standardise1")$converted_data
    }else if(VLE_clicks == "standardise2"){
      VLE_data = convert_VLE(VLE_data, "standardise2")$converted_data
    }else if(VLE_clicks == "logarithmic"){
      VLE_data = convert_VLE(VLE_data, "logarithmic")$converted_data
    }else{
      VLE_data = VLE_data
    }

  }else if(VLE == "FSLSM"){

    VLE_data = dataset_VLE_activity(module, presentation, repeat_students, week_begin, week_end, example_data)$resource_data
    VLE_data = VLE_learning_classification(VLE_data, classification = "FSLSM")$VLE_classified_data

    if(VLE_clicks == "binary"){
      VLE_data = convert_VLE(VLE_data, "binary")$converted_data
    }else if(VLE_clicks == "standardise1"){
      VLE_data = convert_VLE(VLE_data, "standardise1")$converted_data
    }else if(VLE_clicks == "standardise2"){
      VLE_data = convert_VLE(VLE_data, "standardise2")$converted_data
    }else if(VLE_clicks == "logarithmic"){
      VLE_data = convert_VLE(VLE_data, "logarithmic")$converted_data
    }else{
      VLE_data = VLE_data
    }

  }else if(VLE == "OLS"){

    VLE_data = dataset_VLE_activity(module, presentation, repeat_students, week_begin, week_end, example_data)$resource_data
    VLE_data = VLE_learning_classification(VLE_data, classification = "OLS")$VLE_classified_data

    if(VLE_clicks == "binary"){
      VLE_data = convert_VLE(VLE_data, "binary")$converted_data
    }else if(VLE_clicks == "standardise1"){
      VLE_data = convert_VLE(VLE_data, "standardise1")$converted_data
    }else if(VLE_clicks == "standardise2"){
      VLE_data = convert_VLE(VLE_data, "standardise2")$converted_data
    }else if(VLE_clicks == "logarithmic"){
      VLE_data = convert_VLE(VLE_data, "logarithmic")$converted_data
    }else{
      VLE_data = VLE_data
    }


  }else if(VLE == "VARK"){

    VLE_data = dataset_VLE_activity(module, presentation, repeat_students, week_begin, week_end, example_data)$resource_data
    VLE_data = VLE_learning_classification(VLE_data, classification = "VARK")$VLE_classified_data

    if(VLE_clicks == "binary"){
      VLE_data = convert_VLE(VLE_data, "binary")$converted_data
    }else if(VLE_clicks == "standardise1"){
      VLE_data = convert_VLE(VLE_data, "standardise1")$converted_data
    }else if(VLE_clicks == "standardise2"){
      VLE_data = convert_VLE(VLE_data, "standardise2")$converted_data
    }else if(VLE_clicks == "logarithmic"){
      VLE_data = convert_VLE(VLE_data, "logarithmic")$converted_data
    }else{
      VLE_data = VLE_data
    }

  }

  # Combine VLE data
  env = environment()
  if(exists("VLE_data", envir = env)){
    dataset_combined = merge(dataset_combined, VLE_data, by = c("id_student"))
  }

  if(registration == FALSE){
    dataset_combined = select(dataset_combined,!(date_registration:date_unregistration))
  }

  return(list(dataset_combined = tibble(dataset_combined),
              assessment_dataset = assessment,
              demographic_dataset = demographics,
              registration_dataset = registration,
              VLE_dataset = VLE,
              repeat_students = repeat_students,
              withdrawn_students = withdrawn_students,
              week_begin = paste("Inputted week", week_begin),
              week_end = paste("Inputted week", week_end),
              VLE_clicks = VLE_clicks,
              na.rm = na.rm))

}
