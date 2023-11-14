#' Return formatted demographic dataset
#'
#' Load and formats the student demographic dataset from the OULAD for data analysis.
#'
#' @param module Name of the module to be included, either \code{"AAA"}, \code{"BBB"}, \code{"CCC"}, \code{"DDD"}, \code{"EEE"}, \code{"FFF"} or \code{"GGG"}.
#' @param presentation Name of the semester of the module to be included, either \code{"2013B"},
#' \code{"2014B"}, \code{"2013J"}, \code{"2014J"}, \code{"All"}, \code{"Summer"} or \code{"Winter"}.
#' \code{"B"} indicates a February start time whereas \code{"J"} indicates an October start time. \code{"All"} indicates that all presentations of the module will be included in the returned data.
#' @param repeat_students Indicator of whether students who had previous attempts at the module should be removed, either \code{"remove"} or \code{"keep"}.
#'
#' @returns Returns the inputs specified - module, presentation and whether repeat students are to be included.
#' One tibble based on the oulad studentInfo.csv file and the inputs is also returned.
#' The tibble consists of 12 columns (Kuzilek et al., 2017):
#'
#' \itemize{
#' \item{code_module - module identification code.}
#' \item{code_presentation - presentation identification code.}
#' \item{id_student - the unique student identification number.}
#' \item{gender - student’s gender, either Male or Female.}
#' \item{region - the geographic region where the student lived while taking the module-presentation}
#' \item{highest_education - the highest student education level on entry to the module presentation.}
#' \item{imd_band - the Index of multiple deprivation band of the place where the student lived during the module-presentation.}
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
#' @seealso \code{\link{combined_dataset}}
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
  studentInfo$gender = factor(studentInfo$gender, levels=c("F", "M"), labels=c("Female", "Male"))
  studentInfo$region = factor(studentInfo$region)
  studentInfo$highest_education = factor(studentInfo$highest_education)
  studentInfo$imd_band = factor(studentInfo$imd_band)
  levels(studentInfo$imd_band)[levels(studentInfo$imd_band) == "10-20"] <- "10-20%"
  studentInfo$age_band = factor(studentInfo$age_band)
  studentInfo$num_of_prev_attempts = as.numeric(studentInfo$num_of_prev_attempts)
  studentInfo$disability = factor(studentInfo$disability)
  studentInfo$studied_credits = factor(studentInfo$studied_credits, ordered=TRUE)
  studentInfo$final_result = factor(studentInfo$final_result)

  # Filter by module
  if(module != "All"){
    studentInfo = dplyr::filter(studentInfo, code_module == module)
  }

  # Remove repeating students
  if(repeat_students == "remove"){
    studentInfo = dplyr::filter(studentInfo, num_of_prev_attempts == 0)
  }

  # Filter presentation
  if(presentation == "Summer"){
    studentInfo = dplyr::filter(studentInfo, code_presentation == "2013B" | code_presentation == "2014B")
    pres=unique(studentInfo$code_presentation)
    print("This includes presentations:")
    print(paste0(pres))

  }else if(presentation == "Winter"){
    studentInfo = dplyr::filter(studentInfo, code_presentation == "2013J" | code_presentation == "2014J")
    pres=unique(studentInfo$code_presentation)
    print("This includes presentations:")
    print(paste0(pres))

  }else if(presentation == 'All' ){
    studentInfo = studentInfo

  }else{
    # filter by specific presentation
    possible_pres = unique(studentInfo$code_presentation)

    if(presentation %in% possible_pres){
      studentInfo = dplyr::filter(studentInfo, code_presentation == presentation)
    }else{
      stop("This presentation does not exist for ", module)
    }
  }

  return(list(studentInfo = tibble(studentInfo),
              module = module,
              presentation = presentation,
              repeat_students = repeat_students))

}
