#' Returns the formatted assessment data sets
#'
#' Load, combines and formats the assessment and student assessment data sets from the OULAD for data analysis.
#'
#' @param module name of the module to be included, either `"All"`, `"AAA"`, `"BBB"`, `"CCC"`, `"DDD"`, `"EEE"`, `"FFF"` or `"GGG"`.
#' @param presentation name of the semester of the module to be included, either `"2013B"`,
#' `"2014B"`, `"2013J"`, `"2014J"`, `"All"`, `"Summer"` or `"Winter"`.
#' `"B"` indicates a February start time whereas `"J"` indicates an October start time. `"All"` indicates
#' that all presentations of the module will be included in the returned data. Where possible, `"Summer"` returns both `"2013B"` and `"2014B"`, and
#' `"Winter"` returns both `"2013J"` and `"2014J"`.
#' @param repeat_students indicator of whether students who had previous attempts at the module should be removed, either `"remove"` or `"keep"`.
#' @param week_begin the first semester week of the assessment data to be included in formatted data.
#' @param week_end the last semester week of the assessment data to be included in the formatted data. To ensure all
#' continuous assessment is included regardless of the module presentation, set this to 39, the last week of data in the OULAD.
#' @param na.rm logical. Indicates whether NAs should be omitted from the average continuous assessment calculations
#' or treated as zeroes (default). This calculation only includes continuous assessment that
#' were due between the period set by `"week_begin"` and `"week_end"` inclusive,
#' and only occurs when a specific module presentation is requested (e.g., 'BBB 2013J').
#'
#' @returns Returns four `tibbles` (objects of class `tbl_df`) based on the OULAD studentAssessment.csv and assessments.csv files
#' and the specified inputs (module, presentation, and repeat_students).
#' The four `tibbles` are: 1) assessment_data, 2) assessments, 3) assessment_performance, and 4) assessment_reactivity.
#'
#' @section assessment_data tibble:
#' A `tibble` based on the combined OULAD files of studentAssessment.csv and assessments.csv,
#' and the inputs. The `tibble` consists of (Kuzilek et al., 2017):
#'
#' \itemize{
#' \item{id_student - unique student identification number.}
#' \item{code_module - module identification code.}
#' \item{code_presentation - module presentation identification code.}
#' \item{id_assessment - assessment identification number.}
#' \item{assessment_type - type of assessment.}
#' \item{date - information about the final submission date of the assessment calculated as the number of days since the start of the module-presentation.}
#' \item{weight - weight of the assessment in %.}
#' \item{date_submitted - day of the assessment submission by student.}
#' \item{is_banked - logical indicator whether assessment result has been transferred from a previous presentation.}
#' \item{score - student’s score for the specific assessment (range 0-100).}
#' \item{reactivity - calculated using the date variable minus date_submitted variable.}
#' }
#'
#' @section assessments tibble:
#' A `tibble` of seven columns which details for each assessment item for a module presentation,
#' the type of assessment it is, the due date and week of the assessment,
#' and the weight of the assessment. The assessment types are Tutor Marked Assessment (TMA), Computer Marked Assessment (CMA)
#' and Final Exam (Exam). Exams are generally treated separately to other assessments
#' and have weight equal to 100%. The sum of all other assessments is also 100% (Kuzilek et al., 2017).
#'
#' @section assessment_performance tibble:
#' A `tibble` where each row represents a unique student and their scores in the range of 0-100
#' for different assessment items. When a specific module (not 'All') and presentation (not 'All' or 'Winter' or 'Summer') is selected,
#' the final column states each student's average continuous assessment score based on their continuous assessment
#' scores and the assessments weights. For this calculation, the NAs may be not included or replaced by 0 (default; see na.rm argument).
#' The average assessment score will not be calculated if the only variable outputted is exam score.
#' Not all module presentations have exam scores available.
#' Details of the assessment items can be seen in the assessments `tibble`.
#'
#' @section assessment_reactivity tibble:
#' A `tibble` where each row represents a unique student and their reactivity for
#' different assessment items. Treuillier and Boyer (2021) define reactivity for the OULAD as the delay between
#' the date the assessment is returned and the deadline (in days). Negative numbers indicate overdue assessments.
#' Details of the assessment items can be seen in the assessments `tibble`.
#'
#' @references
#' Treuillier, C., & Boyer, A. (2021). Identification of class-representative learner personas.
#' In LA4SLE 2021 - Learning Analytics for Smart Learning Environments (pp. 38-45). Bolzano, Italy.
#'
#' Kuzilek, J., Hlosta, M., & Zdrahal, Z. (2017). Open university learning analytics dataset. Scientific Data
#' volume 4, (pp. 1–8). https://doi.org/10.1038/sdata.2017.171.
#'
#' @seealso [combined_dataset()]
#' @export
#' @importFrom dplyr "filter" "tibble" "select" "mutate_at"
#' @importFrom tidyr "pivot_wider"
#' @importFrom magrittr "%>%"
#' @importFrom stats "na.omit"
#' @examples
#' dataset_assessment(module = "BBB", presentation = "2013J",
#' repeat_students = "remove", week_begin = 1, week_end=39, na.rm = FALSE)
dataset_assessment = function(module = c("All", "AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG"),
                              presentation = c("2013B", "2014B", "2013J", "2014J",
                                               "All","Summer", "Winter"),
                              repeat_students = c("remove", "keep"),
                              week_begin = 1, week_end = 39, na.rm = FALSE){

  # Bind variables
  code_module = pres = possible_pres = id_student = id_assessment =
    score = code_presentation = num_of_prev_attempts = reactivity = names =
    CAcol = CA = weights_all = totalweights = totalCA =  NULL

  # For matching inputs
  module = match.arg(module)
  presentation = match.arg(presentation)
  repeat_students = match.arg(repeat_students)

  # Checks to ensure weeks requested are valid for inputs (pt.1)
  if(week_begin == 0){
    message("Assessment data: There is no week 0 in the data set; week_begin has been set to 1")
    week_begin = 1}
  if(week_end == 0){
    message("Assessment data: There is no week 0 in the data set; week_end has been set to 39")
    week_end = 39}
  if(week_begin > week_end){
    warning("Assessment data: week_begin should not be greater than week_end; week_begin and week_end have been set to 1 and 39 respectively")
    week_begin = 1
    week_end = 39}

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
  assessment_data$score = as.numeric(assessment_data$score)

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

  }else if(presentation == "Winter"){
    assessment_data = filter(assessment_data, code_presentation == "2013J" | code_presentation == "2014J")
    assessments = filter(assessments, code_presentation == "2013J" | code_presentation == "2014J")

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

  # Add 'week' to assessment information
  assessments$week = ceiling(assessments$date/7)

  # Checks to ensure weeks requested are valid for inputs (pt.2)
  any_in_range <- any(assessments$week >= week_begin & assessments$week <= week_end)
  if(any_in_range == FALSE | is.na(any_in_range)) {
    warning("For assessment data, the period for inclusion needs to include a week containing an assessment")
    return(list(assessments = tibble(assessments),
                assessment_performance = NULL,
                assessment_performance = NULL,
                assessment_reactivity = NULL))}


  # Filter by week
  date_lower = ifelse(week_begin > 0, (week_begin-1)*7, (week_begin*7))
  date_upper = ifelse(week_end > 0, (week_end*7)-1, ((week_end+1)*7)-1)
  assessment_data = filter(assessment_data, date >= date_lower, date <= date_upper)

  # Convert to wide format for data analysis
  assessment_wide = assessment_data %>% select("id_assessment", "id_student", "score")
  assessment_wide = pivot_wider(assessment_wide, names_from = id_assessment, values_from = score)

  # Calculate comparison between assignment submission date and date due
  assessment_data$reactivity = assessment_data$date - assessment_data$date_submitted
  assessment_reactivity = assessment_data %>% select("id_assessment", "id_student", "reactivity")
  assessment_reactivity = pivot_wider(assessment_reactivity, names_from = id_assessment, values_from = reactivity)

  # Calculate average score
  if(presentation != "All" & presentation != "Summer" & presentation != "Winter"){
    if(module != "All"){
      if(ncol(assessment_wide) == 2){ # no need to calculate average if only one CA completed
          if(assessments$assessment_type[assessments$id_assessment==colnames(assessment_wide[,2])] != "Exam"){
            assessment_wide$average_CA_score = assessment_wide[[2]]
            if(na.rm == FALSE){
              assessment_wide$average_CA_score[is.na(assessment_wide$average_CA_score)] = 0
            }
      }}else{

        names = colnames(assessment_wide)
        assessment_wide = assessment_wide %>% mutate_at(names, as.character) %>% mutate_at(names, as.numeric)
        index = match(names, assessments$id_assessment)
        CA = 0

        # Ensure exam does not contribute to average CA mark
        weights = assessments$weight
        weights[weights == 100] <- 0

        for(i in 2:length(index)){
          CAcol = assessment_wide[,i] * weights[index[i]]
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
        weights_all = data.frame(matrix(rep(weights[index[-1]], nrow(assessment_wide)),
                                        nrow = nrow(assessment_wide), byrow = TRUE))
        weights_all = weights_all + CA - CA
        totalweights = rowSums(weights_all, na.rm=TRUE)
        assessment_wide$average_CA_score = totalCA/totalweights
  }}}

  return(list(assessment_data = tibble(assessment_data),
              assessments = tibble(assessments),
              assessment_performance = tibble(assessment_wide),
              assessment_reactivity = tibble(assessment_reactivity),
              module = module,
              presentation = presentation,
              repeat_students = repeat_students))

}

