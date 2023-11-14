#' Return formatted VLE dataset based on activity types
#'
#' Load and formats the student Virtual Learning Environment (VLE) dataset from the OULAD for data analysis.
#'
#' @param module Name of the module to be included, either \code{"AAA"}, \code{"BBB"}, \code{"CCC"}, \code{"DDD"}, \code{"EEE"}, \code{"FFF"} or \code{"GGG"}.
#' @param presentation Name of the semester of the module to be included, either \code{"2013B"},
#' \code{"2014B"}, \code{"2013J"}, \code{"2014J"}, or \code{"All"}.
#' \code{"B"} indicates a February start time whereas \code{"J"} indicates an October start time. \code{"All"} indicates that all presentations of the module will be included in the returned data.
#' @param repeat_students Indicator of whether students who had previous attempts at the module should be removed, either \code{"remove"} or \code{"keep"}.
#' If presentation is set to \code{"All"}, this is automatically set to \code{"remove"}.
#' @param week_begin First week of VLE data to be included in formatted data. Depending on the presentation, students
#' started to view activities four weeks prior to the initial module start date. Weeks prior to the initial module start
#' are indicated by a negative integer.
#' @param week_end Last week of VLE data to be included in the formatted data.
#' @param example_data TRUE/FALSE indicator for whether to run a subset of the data as an example
#'
#' @returns Returns the inputs specified - module, presentation, whether repeat students are to be included, the first week of VLE data to be included
#' and the last week of VLE data to be included.
#' Two tibbles are also returned: 1) filtered_data, and 2) resource_data.
#'
#' @section filtered_data tibble:
#' A tibble based on the combined oulad files of studentVLE.csv and vle.csv,
#' and the inputs. The tibble consists of (Kuzilek et al., 2017):
#'
#' \itemize{
#' \item{code_module - the module identification code.}
#' \item{code_presentation - the presentation identification code.}
#' \item{id_site - the VLE material identification number.}
#' \item{activity_type - the role associated with the module material.}
#' \item{week_from - the week from which the material is planned to be used.}
#' \item{week_to - the week until which the material is planned to be used.}
#' \item{id_student - the unique student identification number.}
#' \item{date - the day of student’s interaction with the material.}
#' \item{sum_click - the number of times the student interacted with the material.}
#' }
#'
#' @section resource_data tibble:
#' A tibble where each row represents a unique student and their number of interactions with
#' different VLE activity types for the period (weeks) and module presentation inputted.
#'
#' @references
#' Kuzilek, J., Hlosta, M., & Zdrahal, Z. (2017). Open university learning analytics dataset. Scientific Data
#' volume 4 , (pp. 1–8). https://doi.org/10.1038/sdata.2017.171.
#'
#' @export
#' @importFrom dplyr "select" "filter" "mutate_all" "tibble"
#' @importFrom magrittr "%>%"
#' @importFrom tidyr "pivot_wider" "replace_na"
#' @importFrom stats "aggregate"
#' @seealso \code{\link{convert_VLE}}, \code{\link{dataset_VLE_time}}, \code{\link{VLE_learning_classification}} and \code{\link{combined_dataset}}
#' @examples
#' dataset_VLE_activity(module = "AAA", presentation = "2013J", repeat_students = "remove",
#'                             week_begin = 1, week_end = 39, example_data = TRUE)
dataset_VLE_activity = function(module = c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG"),
                       presentation = c("2013B", "2014B", "2013J", "2014J", "All"),
                       repeat_students = c("remove", "keep"),
                       week_begin = -4, week_end = 39, example_data = FALSE){

  # Bind variables
  code_module = possible_pres = id_student = code_presentation = num_of_prev_attempts =
    activity =  count = NULL

  # Summary: Need dataset with click data and vle_activity description
  env = environment()
  load_github_modified("https://github.com/ehoward1/oulad_data/blob/d451a05599dfa66223197a917f4ca84d1849b3a2/studentVle.RData", env)
  load_github_modified("https://github.com/ehoward1/oulad_data/blob/5bbf34af9922471385371e0d26133d54015f21a2/vle.RData", env)

  # For matching
  module = match.arg(module)
  presentation = match.arg(presentation)
  repeat_students = match.arg(repeat_students)

  # Work with subset of data for example as full dataset takes >5 sec to run
  if(example_data == TRUE){
    studentVle = studentVle[12000:13500,]
    module = "AAA"
    presentation = "2013J"
    print("For this example, a subset of the data is used. This subset is drawn from module AAA and presentation 2013J.")
  }

  # Data types
  vle$code_module = factor(vle$code_module)
  vle$code_presentation = factor(vle$code_presentation)
  vle$activity_type = factor(vle$activity_type)
  vle$week_from = as.numeric(as.character(vle$week_from))
  vle$week_to = as.numeric(as.character(vle$week_to))
  studentVle$code_module = factor(studentVle$code_module)
  studentVle$code_presentation = factor(studentVle$code_presentation)
  studentVle$id_student = as.character(studentVle$id_student)
  studentVle$date = as.numeric(as.character(studentVle$date))
  studentVle$sum_click = as.numeric(as.character(studentVle$sum_click))

  # Filter by module
  studentVle = filter(studentVle, code_module == module)

  if(presentation == "All"){

    # Find and remove students who are not on their first attempt of a module
    load_github_modified("https://github.com/ehoward1/oulad_data/blob/5bbf34af9922471385371e0d26133d54015f21a2/studentInfo.RData", env)
    studentInfo = select(studentInfo, id_student, code_module, code_presentation, num_of_prev_attempts)
    combined = merge(studentVle, studentInfo, by=c("id_student", "code_module", "code_presentation"))
    studentVle = filter(combined, num_of_prev_attempts == 0) %>% select(!(num_of_prev_attempts))
    repeat_students = "remove"
  }else{

    # filter by specific presentation
    possible_pres = unique(studentVle$code_presentation)

    if(presentation %in% possible_pres){

      studentVle = filter(studentVle, code_presentation == presentation)

      if(repeat_students == "remove"){
        load_github_modified("https://github.com/ehoward1/oulad_data/blob/5bbf34af9922471385371e0d26133d54015f21a2/studentInfo.RData", env)
        studentInfo = select(studentInfo, id_student, code_module, code_presentation, num_of_prev_attempts)
        combined = merge(studentVle, studentInfo, by=c("id_student", "code_module", "code_presentation"))
        studentVle = filter(combined, num_of_prev_attempts == 0) %>% select(!(num_of_prev_attempts))
      }

    }else{
      stop("Possible presentations for the module chosen are ", list(possible_pres))
    }
  }

  # Checks to ensure weeks requested are valid for inputs
  if(week_begin == 0){stop("Beginning week cannot be 0")}
  if(week_end == 0){stop("Ending week cannot be 0")}
  if(week_begin > ceiling(max(studentVle$date)/7)){stop(paste("For the current inputs, the VLE data can be filtered between weeks",
                                                              floor(min(studentVle$date)/7), "and weeks", ceiling(max(studentVle$date)/7)))}
  if(week_end < floor(min(studentVle$date)/7)){stop(paste("For the current inputs, the VLE data can be filtered between weeks",
                                                          floor(min(studentVle$date)/7), "and weeks", ceiling(max(studentVle$date)/7)))}
  if(week_begin > week_end){stop("week_begin should not be greater than week_end")}

  #Filter by week
  date_lower = ifelse(week_begin > 0, (week_begin-1)*7, (week_begin*7)-1)
  date_upper = ifelse(week_end > 0, (week_end*7)+1, (week_end+1)*7)
  studentVle = filter(studentVle, date > date_lower, date < date_upper)

  # Now merge with description
  vle_combined = merge(vle, studentVle, by=c("code_module", "code_presentation", "id_site"))

  # Create a count for each date
  studentVle = aggregate(vle_combined$sum_click, by=list(vle_combined$id_student, vle_combined$activity_type), FUN=sum) %>% tibble()

  # rename columns
  colnames(studentVle) = c("id_student", "activity", "count")

  # Change from a long to wide dataframe where each column represents one date
  studentVle_resources = pivot_wider(studentVle, names_from = activity, values_from = count)%>%
    mutate_all(~replace_na(.,0)) # replace NAs with 0 (i.e., no view of an activity)


  return(list(filtered_data = tibble(vle_combined),
              resource_data = tibble(studentVle_resources),
              module = module,
              presentation = presentation,
              repeat_students = repeat_students,
              week_begin = week_begin,
              week_end = week_end))
}


