#' Returns the formatted VLE data set based on activity types
#'
#' Load and formats the student Virtual Learning Environment (VLE) data set from the OULAD for data analysis.
#'
#' @param module name of the module to be included, either `"AAA"`, `"BBB"`, `"CCC"`, `"DDD"`, `"EEE"`, `"FFF"` or `"GGG"`.
#' @param presentation name of the semester of the module to be included, either `"2013B"`,
#' `"2014B"`, `"2013J"`, `"2014J"`, or `"All"`.
#' `"B"` indicates a February start time whereas `"J"` indicates an October start time. `"All"` indicates that all presentations of the module will be included in the returned data.
#' @param repeat_students indicator of whether students who had previous attempts at the module should be removed, either `"remove"` or `"keep"`.
#' If presentation is set to `"All"`, this is automatically set to `"remove"`.
#' @param week_begin the first semester week of VLE data to be included in the formatted data. Depending on the module presentation, students
#' started to view activities up to four weeks prior to the initial module start date. Weeks prior to the initial module start
#' are indicated by a negative integer.
#' @param week_end the last semester week of VLE data to be included in the outputted data.
#' Week 39 is the last week material was viewed (and earlier in some module presentations).
#' @param example_data logical. Indicates whether to run a subset of the data as an example.
#'
#' @returns Returns two `tibbles` (objects of class `tbl_df`) based on the OULAD studentVle.csv and vle.csv files,
#' the specified inputs (module, presentation, and repeat_students), and
#' the range of the weeks included in the `tibbles`. `week_begin` and `week_end`
#' indicates the first and last semester week respectively that is included in
#' the output `tibbles`. These may be different to the corresponding input parameters.
#' Weeks prior to the initial module start day are indicated by a negative integer.
#' The two `tibbles` returned are: 1) filtered_data, and 2) resource_data.
#'
#' @section filtered_data tibble:
#' A `tibble` based on the combined oulad files of studentVLE.csv and vle.csv,
#' and the inputs. The `tibble` consists of (Kuzilek et al., 2017):
#'
#' \itemize{
#' \item{code_module - the module identification code.}
#' \item{code_presentation - the presentation identification code.}
#' \item{id_site - the VLE material identification number.}
#' \item{activity_type - the role associated with the module material.}
#' \item{week_from - the week from which the material is planned to be used.}
#' \item{week_to - the week until which the material is planned to be used.}
#' \item{id_student - the unique student identification number.}
#' \item{date - the day of student’s interaction with the material (module starts on day 0).}
#' \item{sum_click - the number of times the student interacted with the material for the date.}
#' }
#'
#' @section resource_data tibble:
#' A `tibble` where each row represents a unique student and each column their number of interactions with
#' different VLE activity types for the weeks and module presentation inputted.
#'
#' @references
#' Kuzilek, J., Hlosta, M., & Zdrahal, Z. (2017). Open university learning analytics dataset. Scientific Data
#' volume 4 , (pp. 1–8). https://doi.org/10.1038/sdata.2017.171.
#'
#' @export
#' @importFrom dplyr "select" "filter" "mutate_all" "tibble"
#' @importFrom magrittr "%>%"
#' @importFrom utils "read.csv"
#' @importFrom tidyr "pivot_wider" "replace_na"
#' @importFrom stats "aggregate"
#' @seealso [convert_VLE()], [dataset_VLE_time()], [VLE_learning_classification()] and [combined_dataset()]
#' @examples
#' # Uses subset of the VLE data set for example
#' dataset_VLE_activity(example_data = TRUE)
#'\donttest{
#' # Slow to run as it loads the full VLE data set
#' dataset_VLE_activity(module = "BBB", presentation = "2013J",
#' repeat_students = "remove", week_begin = 1, week_end = 13,
#' example_data=FALSE)}
dataset_VLE_activity = function(module = c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG"),
                       presentation = c("2013B", "2014B", "2013J", "2014J", "All"),
                       repeat_students = c("remove", "keep"),
                       week_begin = -4, week_end = 39, example_data = FALSE){

  # Bind variables
  code_module = possible_pres = id_student = code_presentation = num_of_prev_attempts =
    activity = count = min_week = max_week = module_col = NULL

  # Checks to ensure weeks requested are valid for inputs (pt.1)
  if(week_begin == 0){
    message("VLE data: There is no week 0 in the data set; week_begin has been set to 1")
    week_begin = 1}
  if(week_end == 0){
    message("VLE data: There is no week 0 in the data set; week_end has been set to 39")
    week_end = 39}
  if(week_begin > week_end){
    warning("VLE data: week_begin should not be greater than week_end; week_begin and week_end have been set to 1 and 39 respectively")
    week_begin = 1
    week_end = 39}

  # For matching
  module = match.arg(module)
  presentation = match.arg(presentation)
  repeat_students = match.arg(repeat_students)

  # Set local environment
  env = environment()
  load_github_modified("https://github.com/ehoward1/oulad_data/blob/5bbf34af9922471385371e0d26133d54015f21a2/vle.RData", env)

  # Work with subset of data for example as full dataset takes >5 sec to run
  if(example_data == TRUE){
    module = "AAA"
    presentation = "2013J"
    message("For this example, a subset of the data is used. This subset is drawn from module AAA and presentation 2013J.")
    studentVle = read.csv(path_to_file("sample_studentVLE.csv"), header = TRUE)
  }else{
    load_github_modified("https://github.com/ehoward1/oulad_data/blob/d451a05599dfa66223197a917f4ca84d1849b3a2/studentVle.RData", env)
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

    # Filter by specific presentation
    module1 = c('BBB', 'DDD', 'FFF', 'AAA', 'BBB', 'DDD', 'EEE', 'FFF',
                'GGG', 'BBB', 'CCC', 'DDD', 'EEE', 'FFF', 'GGG', 'AAA',
                'BBB', 'CCC', 'DDD', 'EEE', 'FFF', 'GGG')
    pres1 = c('2013B', '2013B', '2013B', '2013J', '2013J', '2013J',
             '2013J', '2013J', '2013J', '2014B', '2014B', '2014B',
             '2014B', '2014B', '2014B', '2014J', '2014J', '2014J',
             '2014J', '2014J', '2014J', '2014J')
    df = data.frame(module_col = module1, pres = pres1)
    df = filter(df, module_col == module)
    possible_pres = df$pres

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

  # Checks to ensure weeks requested are valid for inputs (pt.2)
  if(week_begin > ceiling(max(studentVle$date)/7)){
    warning("For the current inputs, the VLE data can be filtered between week ",
         floor(min(studentVle$date)/7), " and week ", ceiling(max(studentVle$date)/7),
         "; week_begin and week_end have been set to 1 and 39 respectively")
          week_begin = 1
          week_end = 39}
  if(week_end < floor(min(studentVle$date)/7)){
    warning("For the current inputs, the VLE data can be filtered between week ",
            floor(min(studentVle$date)/7), " and week ", ceiling(max(studentVle$date)/7),
            "; week_begin and week_end have been set to 1 and 39 respectively")
    week_begin = 1
    week_end = 39}

  #Filter by week
  date_lower = ifelse(week_begin > 0, (week_begin-1)*7, (week_begin*7))
  date_upper = ifelse(week_end > 0, (week_end*7)-1, ((week_end+1)*7)-1)
  studentVle = filter(studentVle, date >= date_lower, date <= date_upper)
  maxweek = max(studentVle$date)
  max_week = ifelse(maxweek<0, floor(maxweek/7), ceiling((maxweek+1)/7))
  minweek = min(studentVle$date)
  min_week = ifelse(minweek<0, floor(minweek/7), ceiling((minweek+1)/7))

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
              week_begin = min_week,
              week_end = max_week))
}


