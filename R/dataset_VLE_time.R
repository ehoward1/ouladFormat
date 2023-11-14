#' Return formatted VLE dataset for daily and weekly counts
#'
#' Load and formats the student Virtual Learning Environment (VLE) dataset from the OULAD for data analysis.
#'
#' @param module Name of the module to be included, either \code{"AAA"}, \code{"BBB"}, \code{"CCC"}, \code{"DDD"}, \code{"EEE"}, \code{"FFF"} or \code{"GGG"}.
#' @param presentation Name of the semester of the module to be included, either \code{"2013B"},
#' \code{"2014B"}, \code{"2013J"}, \code{"2014J"}, or \code{"All"}.
#' \code{"B"} indicates a February start time whereas \code{"J"} indicates an October start time. \code{"All"} indicates that all presentations of the module will be included in the returned data.
#' @param repeat_students Indicator of whether students who had previous attempts at the module should be removed, either \code{"remove"} or \code{"keep"}.
#' If presentation is set to \code{"All"}, this is automatically set to \code{"remove"}.
#' @param example_data TRUE/FALSE indicator for whether to run a subset of the data as an example.
#'
#' @returns Returns the inputs specified - module, presentation, and whether repeat students are to be included.
#' Three tibbles are also returned: 1) filtered_data, 2) daily_data, and 3) weekly_data.
#'
#' @section filtered_data tibble:
#' A tibble based on the students' VLE interactions and the inputs. The tibble consists of (Kuzilek et al., 2017):
#'
#' \itemize{
#' \item{id_student - the unique student identification number.}
#' \item{date - the day of the student’s interaction with the material.}
#' \item{sum_click - the number of times the student interacted with the material on a specific day.}
#' }
#'
#' @section daily_data:
#' A tibble where each row represents a unique student and their number of interactions with
#' the VLE for different days of the module. The module starts on day 0. Days prior to the initial start day, are indicated by negative integers.
#'
#' @section weekly_data:
#' A tibble where each row represents a unique student and their number of interactions with
#' the VLE for different weeks of the module. Weeks prior to the initial start week (week 1) are indicated by the term 'pre' in the week name.
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
#' @seealso \code{\link{convert_VLE}}, \code{\link{dataset_VLE_activity}}, and \code{\link{combined_dataset}}
#' @examples
#' VLE_data = dataset_VLE_time(module = "AAA", presentation = "2013J",
#' repeat_students = "remove", example_data = TRUE)
dataset_VLE_time = function(module = c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG"),
                             presentation = c("2013B", "2014B", "2013J", "2014J", "All"),
                             repeat_students = c("remove", "keep"),
                             example_data = FALSE){

  # Bind variables
  code_module = possible_pres = id_student = code_presentation = num_of_prev_attempts =
    comparison = missing_days = zero = column_number = sum_click = NULL

  # Summary: Need dataset with click data only
  env = environment()
  load_github_modified("https://github.com/ehoward1/oulad_data/blob/d451a05599dfa66223197a917f4ca84d1849b3a2/studentVle.RData", env)

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

  # now to create daily counts!

  # Create a count for each date
  studentVle = aggregate(studentVle$sum_click, by=list(studentVle$id_student, studentVle$date), FUN=sum) %>% tibble()

  # rename columns
  colnames(studentVle) = c("id_student", "date", "sum_click")

  # Change from a long to wide dataframe where each column represents one date
  studentVle_daily = pivot_wider(studentVle, names_from = date, values_from = sum_click)%>%
    mutate_all(~replace_na(.,0)) # replace NAs with 0 (i.e., no view on a day)

  # Add in any days we are missing
  days = colnames(studentVle_daily[-1]) %>% as.numeric()
  comparison = seq(from = min(days), to = max(days), by=1)
  missing_days = which(is.na(match(comparison, days))) +1
  column_number = ncol(studentVle_daily) + length(missing_days)

  j=2
  test = studentVle_daily[,1]

  for(i in 2:column_number){
    if(i %in% missing_days){
      test = cbind.data.frame(test, 0)
    }else{
      test = cbind.data.frame(test, studentVle_daily[,j])
      j=j+1
    }
  }

  colnames(test) = c("id_student", min(days):max(days))
  studentVle_daily = tibble(test)

  # Now to create weekly counts!
  studentVle_weekly = studentVle_daily[,1]
  zero = which(colnames(studentVle_daily)==0) # find column where module began
  if(zero == 2){ # then there are no pre-weeks
    pre_week_numbers = 0
  }else{
    pre_week_numbers = ceiling(zero/7)
    }

  # Weeks prior to start (Day 0)
  if(pre_week_numbers != 0){
  for(i in 1:pre_week_numbers){
    if((zero-i*7)<0){ # check if we go out of bounds
      studentVle_weekly[,i+1] = apply(studentVle_daily[,2:(zero-(i-1)*7-1)], MARGIN=1, sum)
    }else{
      studentVle_weekly[,i+1] = apply(studentVle_daily[,(zero-i*7):(zero-(i-1)*7-1)], MARGIN=1, sum)
    }
  }
  studentVle_weekly = data.frame(studentVle_weekly[,1], rev(studentVle_weekly[,-1]))
  }

  # Weeks during semester
  max_weeks = ceiling(ncol(studentVle_daily[,-(1:zero-1)])/7)
  max_days = max(days)

  for(i in 1:max_weeks){
    if((zero+6+(i-1)*7) < (zero+max_days)){
      hold_day = (zero+6+(i-1)*7)
    }else{
      hold_day = zero+max_days
        }
    studentVle_weekly[,pre_week_numbers+i+1] = apply(studentVle_daily[,(zero+(i-1)*7):hold_day], MARGIN=1, sum)
  }

  if(pre_week_numbers != 0){
  colnames(studentVle_weekly) = c("id_student",
                                  paste0("Week-pre-",pre_week_numbers:1),
                                  paste0("Week",1:max_weeks))
  }else{
    colnames(studentVle_weekly) = c("id_student",
                                    paste0("Week",1:max_weeks))
  }

  return(list(filtered_data = tibble(studentVle),
              daily_data = tibble(studentVle_daily),
              weekly_data = tibble(studentVle_weekly),
              module = module,
              presentation = presentation,
              repeat_students = repeat_students))
}
