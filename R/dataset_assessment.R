#' Return formatted assessment dataset
#'
#' Load, combines and formats the assessment and student assessment datasets from the OULAD for data analysis.
#'
#' @param module Name of the module to be included, either \code{"AAA"}, \code{"BBB"}, \code{"CCC"}, \code{"DDD"}, \code{"EEE"}, \code{"FFF"} or \code{"GGG"}.
#' @param presentation Name of the semester of the module to be included, either \code{"2013B"},
#' \code{"2014B"}, \code{"2013J"}, \code{"2014J"}, \code{"All"}, \code{"Summer"} or \code{"Winter"}.
#' \code{"B"} indicates a February start time whereas \code{"J"} indicates an October start time. \code{"All"} indicates that all presentations of the module will be included in the returned data.
#' @param repeat_students Indicator of whether students who had previous attempts at the module should be removed, either \code{"remove"} or \code{"keep"}.
#'
#' @returns Returns the inputs specified - module, presentation and whether repeat students are to be included.
#' Four tibbles are also returned: 1) assessment_data, 2) assessment_performance,
#' 3) assessment_reactivity and 4) assessments.
#'
#' @section assessment_data tibble:
#' A tibble based on the combined oulad files of studentAssessment.csv and assessments.csv,
#' and the inputs. The tibble consists of (Kuzilek et al., 2017):
#'
#' \itemize{
#' \item{id_assessment - the assessment identification number.}
#' \item{code_module - the module identification code.}
#' \item{code_presentation - the presentation identification code.}
#' \item{assessment_type - the type of assessment.}
#' \item{date - due date of the assessment in the module term.}
#' \item{weight - the weight of the assessment.}
#' \item{id_student - the unique student identification number.}
#' \item{date_submitted - the day of the assessment submission by student.}
#' \item{is_banked - logical indicator whether assessment result has been transferred from a previous presentation.}
#' \item{score - the student’s score for the specific assessment (range 0-100).}
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
#' scores and the assessments weights. For this calculation NA is treated as 0.
#' Details of the assessment items can be seen in the assessments tibble.
#'
#' @section assessment_reactivity:
#' A tibble where each row represents a unique student and their reactivity for
#' different assessment items. Treuillier and Boyer (2021) define reactivity for the oulad as the delay between
#' the date the assessment is returned and the deadline (in days). Negative numbers indicate overdue assessments.
#' Details of the assessment items can be seen in the assessments tibble.
#'
#' @references
#' Treuillier, C., & Boyer, A. (2021). Identification of class-representative learner personas.
#' In LA4SLE 2021 - Learning Analytics for Smart Learning Environments (pp. 38-45). Bolzano, Italy.
#' ⟨hal-03549915
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
#' dataset_assessment(module = "BBB", presentation = "2013J", repeat_students = "remove")
dataset_assessment = function(module = c("All", "AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG"),
                              presentation = c("2013B", "2014B", "2013J", "2014J",
                                               "All","Summer", "Winter"),
                              repeat_students = c("remove", "keep")){

  # Bind variables
  code_module = pres = possible_pres = id_student = id_assessment =
    score = code_presentation = num_of_prev_attempts = reactivity = names = NULL

  # For matching inputs
  module = match.arg(module)
  presentation = match.arg(presentation)
  repeat_students = match.arg(repeat_students)

  # Read in relevant datasets
  env=environment()
  load_github_modified("https://github.com/ehoward1/oulad_data/blob/5bbf34af9922471385371e0d26133d54015f21a2/studentAssessment.RData", env)
  load_github_modified("https://github.com/ehoward1/oulad_data/blob/5bbf34af9922471385371e0d26133d54015f21a2/assessments.RData", env)
  studentAssessment = studentAssessment
  assessment_data = merge(assessments, studentAssessment, by = "id_assessment")

  # Data types
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
  if(presentation == 'Summer'){
    assessment_data = filter(assessment_data, code_presentation == "2013B" | code_presentation == "2014B")
    assessments = filter(assessments, code_presentation == "2013B" | code_presentation == "2014B")
    pres=unique(assessment_data$code_presentation)
    print("This includes presentations:")
    print(paste0(pres))

  }else if(presentation == 'Winter'){
    assessment_data = filter(assessment_data, code_presentation == "2013J" | code_presentation == "2014J")
    assessments = filter(assessments, code_presentation == "2013J" | code_presentation == "2014J")
    pres=unique(assessment_data$code_presentation)
    print("This includes presentations:")
    print(paste0(pres))

  }else if(presentation == 'All' ){
    assessment_data = assessment_data

  }else{
    # filter by specific presentation
    possible_pres = unique(assessment_data$code_presentation)

    if(presentation %in% possible_pres){
      assessment_data = filter(assessment_data, code_presentation == presentation)
      assessments = filter(assessments, code_presentation == presentation)

    }else{
      stop("This presentation does not exist for ", module)
    }
  }

  # convert to wide format for data analysis
  assessment_wide = assessment_data %>% select("id_assessment", "id_student", "score")
  assessment_wide = pivot_wider(assessment_wide, names_from = id_assessment, values_from = score)

  # calculate comparison between assignment submission date and date due
  assessment_data$reactivity = assessment_data$date - assessment_data$date_submitted
  assessment_reactivity = assessment_data %>% select("id_assessment", "id_student", "reactivity")
  assessment_reactivity = pivot_wider(assessment_reactivity, names_from = id_assessment, values_from = reactivity)

  # calculate average score
  names = colnames(assessment_wide)
  assessment_wide = assessment_wide %>% mutate_at(names, as.character) %>% mutate_at(names, as.numeric)
  index = match(names, assessments$id_assessment)
  weights = 0
  totalCA = vector(mode = "numeric", length = nrow(assessment_wide))
  for(i in 2:length(index)){
    CA = assessment_wide[,i] * assessments$weight[index[i]]
    CA[is.na(CA)] = 0
    totalCA = totalCA + CA
    weights = weights + assessments$weight[index[i]]
  }
  assessment_wide$average_score = totalCA[,1]/weights

  return(list(assessment_data = tibble(assessment_data),
              assessments = tibble(assessments),
              assessment_performance = tibble(assessment_wide),
              assessment_reactivity = tibble(assessment_reactivity),
              module = module,
              presentation = presentation,
              repeat_students = repeat_students))

}

