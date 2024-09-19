#' Returns the formatted registration data set
#'
#' Load and formats the student registration data set from the OULAD for data analysis.
#'
#' @param module name of the module to be included, either `"All"`, `"AAA"`, `"BBB"`, `"CCC"`, `"DDD"`, `"EEE"`, `"FFF"` or `"GGG"`.
#' @param presentation name of the semester of the module to be included, either `"2013B"`,
#' `"2014B"`, `"2013J"`, `"2014J"`, `"All"`, `"Summer"` or `"Winter"`.
#' `"B"` indicates a February start time whereas `"J"` indicates an October start time. `"All"` indicates
#' that all presentations of the module will be included in the returned data. Where possible,  `"Summer"` returns
#' both `"2013B"` and `"2014B"`, and `"Winter"` returns both `"2013J"` and `"2014J"`.
#' @param repeat_students indicator of whether students who had previous attempts at the module should be removed, either `"remove"` or `"keep"`.
#'
#' @returns Returns one `tibble` (object of class `tbl_df`), called 'studentRegistration', based on the OULAD studentRegistration.csv file
#' and the specified inputs (module, presentation, and repeat_students).
#'
#' The `tibble` consists of five columns (Kuzilek et al., 2017):
#'
#' \itemize{
#' \item{id_student - the unique student identification number.}
#' \item{code_module - module identification code.}
#' \item{code_presentation - module presentation identification code.}
#' \item{date_registration - the day of student’s registration for the module presentation. Modules start on day 0.}
#' \item{date_unregistration - the day of student unregistration from the module presentation. This is NA if the student completed the module presentation.}
#' }
#'
#' @references
#' Kuzilek, J., Hlosta, M., & Zdrahal, Z. (2017). Open university learning analytics data set. Scientific Data
#' volume 4 , (pp. 1–8). https://doi.org/10.1038/sdata.2017.171.
#'
#' @seealso [combined_dataset()]
#'
#' @export
#' @importFrom dplyr "filter" "tibble" "select"
#' @importFrom magrittr "%>%"
#' @examples
#' dataset_registration(module = "BBB", presentation = "2013J", repeat_students = "remove")
dataset_registration = function(module = c("All", "AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG"),
                           presentation = c("2013B", "2014B", "2013J", "2014J",
                                            "All", "Summer", "Winter"),
                           repeat_students = c("remove", "keep")){

  # Bind variables
  code_module = pres = possible_pres = id_student = code_presentation = num_of_prev_attempts = NULL

  # Load data
  env=environment()
  load_github_modified("https://github.com/ehoward1/oulad_data/blob/5bbf34af9922471385371e0d26133d54015f21a2/studentRegistration.RData", env)

  module = match.arg(module)
  presentation = match.arg(presentation)
  repeat_students = match.arg(repeat_students)

  # Convert to correct data types
  studentRegistration$code_module = factor(studentRegistration$code_module)
  studentRegistration$code_presentation = factor(studentRegistration$code_presentation)
  studentRegistration$id_student = as.character(studentRegistration$id_student)
  studentRegistration$date_registration = as.numeric(studentRegistration$date_registration)
  studentRegistration$date_unregistration = as.numeric(studentRegistration$date_unregistration)

  # Filter by module
  if(module != "All"){
    studentRegistration = filter(studentRegistration, code_module == module)
  }

  # Filter presentation
  if(presentation == "Summer"){
    studentRegistration = filter(studentRegistration, code_presentation == "2013B" | code_presentation == "2014B")

  }else if(presentation == "Winter"){
    studentRegistration = filter(studentRegistration, code_presentation == "2013J" | code_presentation == "2014J")

  }else if(presentation == "All"){
    studentRegistration = studentRegistration

  }else{
    # filter by specific presentation
    possible_pres = unique(studentRegistration$code_presentation)

    if(presentation %in% possible_pres){
      studentRegistration = filter(studentRegistration, code_presentation == presentation)
    }else{
      stop("This presentation does not exist for ", module)
    }
  }

  # Remove repeating students
  if(repeat_students == "remove"){
    load_github_modified("https://github.com/ehoward1/oulad_data/blob/5bbf34af9922471385371e0d26133d54015f21a2/studentInfo.RData", env)
    studentInfo = select(studentInfo, id_student, code_module, code_presentation, num_of_prev_attempts)
    combined = merge(studentRegistration, studentInfo, by=c("id_student", "code_module", "code_presentation"))
    studentRegistration = filter(combined, num_of_prev_attempts == 0) %>% select(!(num_of_prev_attempts))
  }

  return(list(studentRegistration = tibble(studentRegistration),
              module = module,
              presentation = presentation,
              repeat_students = repeat_students))

}
