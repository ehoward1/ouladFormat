#' Returns the formatted assessment data sets
#'
#' Load, combines and formats the assessment and student assessment data sets from the OULAD for data analysis.
#'
#' @param module name of the module to be included, either \code{"All"}, \code{"AAA"}, \code{"BBB"}, \code{"CCC"}, \code{"DDD"}, \code{"EEE"}, \code{"FFF"} or \code{"GGG"}.
#' @param presentation name of the semester of the module to be included, either \code{"2013B"},
#' \code{"2014B"}, \code{"2013J"}, \code{"2014J"}, \code{"All"}, \code{"Summer"} or \code{"Winter"}.
#' \code{"B"} indicates a February start time whereas \code{"J"} indicates an October start time. \code{"All"} indicates
#' that all presentations of the module will be included in the returned data. \code{"Summer"} returns both \code{"2013B"} and \code{"2014B"}.
#' \code{"Winter"} returns both \code{"2013J"} and \code{"2014J"}.
#' @param repeat_students indicator of whether students who had previous attempts at the module should be removed, either \code{"remove"} or \code{"keep"}.
#' @param week_begin the first semester week of the assessment data to be included in formatted data.
#' @param week_end the last semester week of the assessment data to be included in the formatted data. To ensure all
#' continuous assessment is included regardless of the module, set this to 39, the last week of data.
#' @param na.rm logical. Indicates whether NAs should be omitted from the average continuous assessment calculations (default)
#' or treated as zeroes. This calculation only includes continuous assessment that
#' were due between the period set by \code{"week_begin"} and \code{"week_end"} inclusive.
#'
#' @returns Returns four tibbles based on the OULAD studentAssessment.csv and assessments.csv files
#' and the specified inputs (module, presentation, and repeat_students). The four tibbles are:
#' 1) assessment_data, 2) assessment_performance, 3) assessment_reactivity and 4) assessments.
#'
#' @section assessment_data tibble:
#' A tibble based on the combined oulad files of studentAssessment.csv and assessments.csv,
#' and the inputs. The tibble consists of (Kuzilek et al., 2017):
#'
#' \itemize{
#' \item{id_assessment - assessment identification number.}
#' \item{code_module - module identification code.}
#' \item{code_presentation - module presentation identification code.}
#' \item{assessment_type - type of assessment.}
#' \item{date - due date of the assessment in the module term.}
#' \item{weight - weight of the assessment.}
#' \item{id_student - unique student identification number.}
#' \item{date_submitted - day of the assessment submission by student.}
#' \item{is_banked - logical indicator whether assessment result has been transferred from a previous presentation.}
#' \item{score - student’s score for the specific assessment (range 0-100).}
#' \item{reactivity - calculated using the date variable minus date_submitted variable.}
#' }
#'
#' @section assessments:
#' A tibble of six columns which details for each assessment item for a module presentation,
#' the type of assessment it is, the due date of the assessment,
#' and the weight of the assessment. The assessment types are Tutor Marked Assessment (TMA), Computer Marked Assessment (CMA)
#' and Final Exam (Exam). Exams are generally treated separately to other assessments
#' and have weight equal to 100\%. The sum of all other assessments is also 100\% (Kuzilek et al., 2017).
#'
#' @section assessment_performance tibble:
#' A tibble where each row represents a unique student and their scores in the range of 0-100
#' for different assessment items. The final column states each student's average assessment score based on their assessment
#' scores and the assessments weights. The NAs may be as 0 or not included (default; see na.rm argument).
#' Details of the assessment items can be seen in the assessments tibble.
#'
#' @section assessment_reactivity:
#' A tibble where each row represents a unique student and their reactivity for
#' different assessment items. Treuillier and Boyer (2021) define reactivity for the OULAD as the delay between
#' the date the assessment is returned and the deadline (in days). Negative numbers indicate overdue assessments.
#' Details of the assessment items can be seen in the assessments tibble.
#'
#' @references
#' Treuillier, C., & Boyer, A. (2021). Identification of class-representative learner personas.
#' In LA4SLE 2021 - Learning Analytics for Smart Learning Environments (pp. 38-45). Bolzano, Italy.
#' <hal-0354991>.
#'
#' Kuzilek, J., Hlosta, M., & Zdrahal, Z. (2017). Open university learning analytics dataset. Scientific Data
#' volume 4, (pp. 1–8). https://doi.org/10.1038/sdata.2017.171.
#'
#' @seealso \code{\link{combined_dataset}}
#' @export
#' @importFrom dplyr "filter" "tibble" "select" "mutate_at"
#' @importFrom tidyr "pivot_wider"
#' @importFrom magrittr "%>%"
#' @examples
#' dataset_assessment(module = "BBB", presentation = "2013J",
#' repeat_students = "remove", na.rm = FALSE)
dataset_assessment = function(module = c("All", "AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG"),
                              presentation = c("2013B", "2014B", "2013J", "2014J",
                                               "All","Summer", "Winter"),
                              repeat_students = c("remove", "keep"),
                              week_begin = 1, week_end = 39, na.rm = FALSE){

  # Bind variables
  code_module = pres = possible_pres = id_student = id_assessment =
    score = code_presentation = num_of_prev_attempts = reactivity = names =
    CAcol = CA = weights_all = totalweights = totalCA = min_week = max_week = NULL

  # For matching inputs
  module = match.arg(module)
  presentation = match.arg(presentation)
  repeat_students = match.arg(repeat_students)

  # Read in relevant data sets
  env=environment()
  load_github_modified("https://github.com/ehoward1/oulad_data/blob/5bbf34af9922471385371e0d26133d54015f21a2/studentAssessment.RData", env)
  load_github_modified("https://github.com/ehoward1/oulad_data/blob/5bbf34af9922471385371e0d26133d54015f21a2/assessments.RData", env)
  studentAssessment = studentAssessment
  assessment_data = merge(assessments, studentAssessment, by = "id_assessment")

  # Convert data types
  assessment_data$id_assessment = factor(assessment_data$id_assessment)
  assessment_data$code_module = factor(assessment_data$code_module)
  assessment_data$code_presentation = factor(assessment_data$code_presentation)
  assessment_data$assessment_type = factor(assessment_data$assessment_type)
  assessment_data$date = as.numeric(as.character(assessment_data$date))
  assessment_data$weight = as.numeric(as.character(assessment_data$weight))
  assessment_data$id_student = as.character(assessment_data$id_student)
  assessment_data$date_submitted = as.numeric(as.character(assessment_data$date_submitted))
  assessment_data$is_banked = factor(assessment_data$is_banked, levels=c("0", "1"))
  assessment_data$score = factor(assessment_data$score)

  # Filter by module
  if(module != "All"){
    assessment_data = filter(assessment_data, code_module == module)
    assessments = filter(assessments, code_module == module)
  }

  # Remove repeating students
  if(repeat_students == "remove"){
    load_github_modified("https://github.com/ehoward1/oulad_data/blob/5bbf34af9922471385371e0d26133d54015f21a2/studentInfo.RData", env)
    studentInfo = select(studentInfo, id_student, code_module, code_presentation, num_of_prev_attempts)
    combined = merge(assessment_data, studentInfo, by=c("id_student", "code_module", "code_presentation"))
    assessment_data = filter(combined, num_of_prev_attempts == 0) %>% select(!(num_of_prev_attempts))
  }

  # Filter presentation
  if(presentation == "Summer"){
    assessment_data = filter(assessment_data, code_presentation == "2013B" | code_presentation == "2014B")
    assessments = filter(assessments, code_presentation == "2013B" | code_presentation == "2014B")
    pres=unique(assessment_data$code_presentation)
    print("This includes the presentations:")
    print(paste0(pres))

  }else if(presentation == "Winter"){
    assessment_data = filter(assessment_data, code_presentation == "2013J" | code_presentation == "2014J")
    assessments = filter(assessments, code_presentation == "2013J" | code_presentation == "2014J")
    pres=unique(assessment_data$code_presentation)
    print("This includes the presentations:")
    print(paste0(pres))

  }else if(presentation == "All"){
    assessment_data = assessment_data

  }else{
    # Filter by specific presentation
    possible_pres = unique(assessment_data$code_presentation)

    if(presentation %in% possible_pres){
      assessment_data = filter(assessment_data, code_presentation == presentation)
      assessments = filter(assessments, code_presentation == presentation)

    }else{
      stop("This presentation does not exist for ", module)
    }
  }

  # Checks to ensure weeks requested are valid for inputs
  if(week_begin == 0){stop("Beginning week cannot be 0")}
  if(week_end == 0){stop("Ending week cannot be 0")}
  if(week_begin > ceiling(max(assessment_data$date)/7)){stop(paste("For the current inputs, the VLE data can be filtered between weeks",
                                                              floor(min(assessment_data$date)/7), "and weeks", ceiling(max(assessment_data$date)/7)))}
  if(week_end < floor(min(assessment_data$date)/7)){stop(paste("For the current inputs, the VLE data can be filtered between weeks",
                                                          floor(min(assessment_data)/7), "and weeks", ceiling(max(assessment_data$date)/7)))}
  if(week_begin > week_end){stop("week_begin should not be greater than week_end")}

  #Filter by week
  date_lower = ifelse(week_begin > 0, (week_begin-1)*7, (week_begin*7))
  date_upper = ifelse(week_end > 0, (week_end*7)-1, ((week_end+1)*7)-1)
  assessment_data = filter(assessment_data, date >= date_lower, date <= date_upper)
  maxweek = max(assessment_data$date)
  max_week = ifelse(maxweek<0, floor(maxweek/7), ceiling((maxweek+1)/7))
  minweek = min(assessment_data$date)
  max_week = ifelse(minweek<0, floor(minweek/7), ceiling((minweek+1)/7))

  # Convert to wide format for data analysis
  assessment_wide = assessment_data %>% select("id_assessment", "id_student", "score")
  assessment_wide = pivot_wider(assessment_wide, names_from = id_assessment, values_from = score)

  # Calculate comparison between assignment submission date and date due
  assessment_data$reactivity = assessment_data$date - assessment_data$date_submitted
  assessment_reactivity = assessment_data %>% select("id_assessment", "id_student", "reactivity")
  assessment_reactivity = pivot_wider(assessment_reactivity, names_from = id_assessment, values_from = reactivity)

  # Calculate average score
  names = colnames(assessment_wide)
  assessment_wide = assessment_wide %>% mutate_at(names, as.character) %>% mutate_at(names, as.numeric)
  index = match(names, assessments$id_assessment)
  CA = 0

  for(i in 2:length(index)){
    CAcol = assessment_wide[,i] * assessments$weight[index[i]]
    CA = cbind.data.frame(CA, CAcol)
  }

  CA = CA[,-1]
  # Treat NAs for continuous assessment as a 0 mark
  if(na.rm == FALSE){
      CA[is.na(CA)] = 0
      totalCA = rowSums(CA, na.rm=TRUE)
    # Do not include NAs in continuous assessment calculation
    }else{
      totalCA = rowSums(CA, na.rm=TRUE)
    }

  # Calculate weights taking into consideration NAs included or not included
  weights_all = data.frame(matrix(rep(assessments$weight[index[-1]], nrow(assessment_wide)),
                                  nrow = nrow(assessment_wide), byrow = TRUE))
  weights_all = weights_all + CA - CA
  totalweights = rowSums(weights_all, na.rm=TRUE)
  assessment_wide$average_score = totalCA/totalweights

  return(list(assessment_data = tibble(assessment_data),
              assessments = tibble(assessments),
              assessment_performance = tibble(assessment_wide),
              assessment_reactivity = tibble(assessment_reactivity),
              module = module,
              presentation = presentation,
              repeat_students = repeat_students))

}

