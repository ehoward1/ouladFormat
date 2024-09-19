#' Returns the formatted demographic data set
#'
#' Load and formats the student demographic data set from the OULAD for data analysis.
#'
#' @param module name of the module to be included, either `"All"`, `"AAA"`, `"BBB"`, `"CCC"`, `"DDD"`, `"EEE"`, `"FFF"` or `"GGG"`.
#' @param presentation name of the semester of the module to be included, either `"2013B"`,
#' `"2014B"`, `"2013J"`, `"2014J"`, `"All"`, `"Summer"` or `"Winter"`.
#' `"B"` indicates a February start time whereas `"J"` indicates an October start time. `"All"` indicates
#' that all presentations of the module will be included in the returned data. `"Summer"` returns both `"2013B"` and `"2014B"`.
#' `"Winter"` returns both `"2013J"` and `"2014J"`.
#' @param repeat_students indicator of whether students who had previous attempts at the module should be removed, either `"remove"` or `"keep"`.
#'
#' @returns Returns one `tibble` (object of class `tbl_df`), called 'studentInfo', based on the OULAD studentInfo.csv file
#' and the specified inputs (module, presentation, and repeat_students).
#'
#' The `tibble` consists of 12 columns (Kuzilek et al., 2017):
#'
#' \itemize{
#' \item{code_module - module identification code.}
#' \item{code_presentation - module presentation identification code.}
#' \item{id_student - the unique student identification number.}
#' \item{gender - student’s gender, either Male or Female.}
#' \item{region - the geographic region where the student lived while taking the module-presentation.}
#' \item{highest_education - the highest student education level on entry to the module presentation.}
#' \item{imd_band - the index of multiple deprivation band of the place where the student lived during the module-presentation.}
#' \item{age_band - a band of student’s age.}
#' \item{num_of_prev_attempts - the number of times the student has attempted this module previously.}
#' \item{studied_credits - the total number of credits for the modules the student is currently studying.}
#' \item{disability - indicates whether the student has declared a disability.}
#' \item{final_result - student’s final result in the module-presentation.}
#' }
#'
#' @references
#' Kuzilek, J., Hlosta, M., & Zdrahal, Z. (2017). Open university learning analytics dataset. Scientific Data
#' volume 4 , (pp. 1–8). https://doi.org/10.1038/sdata.2017.171.
#'
#' @seealso [combined_dataset()]
#' @export
#' @importFrom dplyr "filter" "tibble"
#' @importFrom magrittr "%>%"
#' @examples
#' dataset_demographics(module = "BBB", presentation = "2013J", repeat_students = "remove")
dataset_demographics = function(module = c("All", "AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG"),
                            presentation = c("2013B", "2014B", "2013J", "2014J",
                                             "All", "Summer", "Winter"),
                            repeat_students = c("remove", "keep")){

  # Bind the variables locally
  code_module = pres = possible_pres = code_presentation = num_of_prev_attempts = NULL

  # Demographic data
  env=environment()
  load_github_modified("https://github.com/ehoward1/oulad_data/blob/5bbf34af9922471385371e0d26133d54015f21a2/studentInfo.RData", env)

  # For matching
  module = match.arg(module)
  presentation = match.arg(presentation)
  repeat_students = match.arg(repeat_students)

  # Convert to correct data types
  studentInfo$code_module = factor(studentInfo$code_module)
  studentInfo$code_presentation = factor(studentInfo$code_presentation)
  studentInfo$id_student = as.character(studentInfo$id_student)
  studentInfo$gender = factor(studentInfo$gender, levels=c("F", "M"), labels=c("Female", "Male"))
  studentInfo$region = factor(studentInfo$region)
  studentInfo$highest_education = factor(studentInfo$highest_education,
                                         levels = c("No Formal quals", "Lower Than A Level",
                                                    "A Level or Equivalent", "HE Qualification",
                                                    "Post Graduate Qualification"),
                                         labels = c("No Formal quals", "Lower Than A Level",
                                                    "A Level or Equivalent", "HE Qualification",
                                                    "Post Graduate Qualification"),
                                         ordered = TRUE)
  studentInfo$imd_band = factor(studentInfo$imd_band, ordered = TRUE)
  levels(studentInfo$imd_band)[levels(studentInfo$imd_band) == "10-20"] <- "10-20%"
  levels(studentInfo$imd_band)[levels(studentInfo$imd_band) == ""] <- NA
  studentInfo$age_band = factor(studentInfo$age_band, levels = c("0-35", "35-55", "55<="),
                                labels = c("0-35", "35-55", "55<="), ordered = TRUE)
  studentInfo$num_of_prev_attempts = as.numeric(as.character(studentInfo$num_of_prev_attempts))
  studentInfo$disability = factor(studentInfo$disability)
  studentInfo$studied_credits = factor(studentInfo$studied_credits, ordered=TRUE)
  studentInfo$final_result = factor(studentInfo$final_result)

  # Filter by module
  if(module != "All"){
    studentInfo = filter(studentInfo, code_module == module)
  }

  # Remove repeating students
  if(repeat_students == "remove"){
    studentInfo = filter(studentInfo, num_of_prev_attempts == 0)
  }

  # Filter presentation
  if(presentation == "Summer"){
    studentInfo = filter(studentInfo, code_presentation == "2013B" | code_presentation == "2014B")

  }else if(presentation == "Winter"){
    studentInfo = filter(studentInfo, code_presentation == "2013J" | code_presentation == "2014J")

  }else if(presentation == "All"){
    studentInfo = studentInfo

  }else{
    # filter by specific presentation
    possible_pres = unique(studentInfo$code_presentation)

    if(presentation %in% possible_pres){
      studentInfo = filter(studentInfo, code_presentation == presentation)
    }else{
      stop("This presentation does not exist for ", module)
    }
  }

  return(list(studentInfo = tibble(studentInfo),
              module = module,
              presentation = presentation,
              repeat_students = repeat_students))

}
