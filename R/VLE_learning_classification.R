#' Classify VLE activities
#'
#' Transform Open University VLE activities to classifications under either the:
#' \itemize{
#' \item{Filder-Selverman Learning Style Model (FSLM) mapped by Balti at al. (2023),}
#' \item{Felder–Silverman Learning Style Model (FSLSM) (see Felder & Silverman, 1988) mapped by Nazempour and Darabi (2023),}
#' \item{VARK Learning Style Model (see Fleming and Colleen, 1992) mapped by Balti at al. (2023), or}
#' \item{Corresponding to the Online Learning Style Characteristics (OLS) mapped by Yan et al. (2021) and drawing
#' on Li and Yin (2017).}
#' }
#'
#' @param activity_data VLE activity data set to be converted.
#' @param classification classification system to be used to categorise the different VLE activities under.
#'
#' @returns Returns the classification specified, the mapping for the VLE activities to each category in the specified
#' learning model, and one tibble, VLE_classified_data.
#'
#' @section VLE_classified_data:
#' A tibble where each row represents a unique student and their number of interactions with
#' different categories of the specified learning model.
#'
#' @seealso
#' [dataset_VLE_activity()] for obtaining the VLE data set needed for input data
#' @export
#' @importFrom dplyr "mutate" "tibble" "select" "rowwise"
#' @importFrom magrittr "%>%"
#'
#' @references
#' Balti, R., Hedhili, A., Chaari, W.L. & Abed, M. (2023). Hybrid analysis of the learner’s online behavior based
#' on learning style. Education and Information Technologies. https://doi.org/10.1007/s10639-023-11595-x
#'
#' Felder, R. M., & Silverman, L. K. (1988). Learning and Teaching Styles in Engineering Education.
#' Engineering Education, 78(7), 674–681. https://www.engr.ncsu.edu/wp-content/uploads/drive/1QP6kBI1iQmpQbTXL-08HSl0PwJ5BYnZW/1988-LS-plus-note.pdf
#'
#' Fleming, N. D., & Mills, C. (1992). Not Another Inventory, Rather a Catalyst for Reflection.
#' To Improve the Academy. 246. https://digitalcommons.unl.edu/podimproveacad/246
#'
#' Kuzilek, J., Hlosta, M., & Zdrahal, Z. (2017). Open university learning analytics dataset. Scientific Data
#' volume 4 , (pp. 1–8). https://doi.org/10.1038/sdata.2017.171.
#'
#' Li, R., & Yin, C. (2017). Analysis of Online Learning Style Model Based on K-means Algorithm.
#' Proceedings of the 3rd International Conference on Economics, Management, Law and Education (EMLE 2017).
#' (pp. 692-697). Atlantis Press. https://doi.org/10.2991/emle-17.2017.148
#'
#' Nazempour, R, & Darabi, H. (2023). Personalized Learning in Virtual Learning Environments Using Students’ Behavior Analysis.
#' Education Sciences. 2023; 13(5), 457. https://doi.org/10.3390/educsci13050457
#'
#' Yan, L., Yin, C., Chen, H., Rong, W., Xiong, Z., & David, B. (2021). Learning Resource
#' Recommendation in E-Learning Systems Based on Online Learning Style. In: Qiu, H., Zhang, C.,
#' Fei, Z., Qiu, M., Kung, SY. (eds) Knowledge Science, Engineering and Management. KSEM 2021.
#' Lecture Notes in Computer Science, 12817. Springer, Cham. https://doi.org/10.1007/978-3-030-82153-1_31
#'
#' @examples
#' \donttest{
#' VLE_data = dataset_VLE_activity(example_data = TRUE)$resource_data
#' VLE_learning_classification(VLE_data, classification = "FSLSM")}
VLE_learning_classification = function(activity_data, classification = c("FSLM",  "FSLSM", "OLS" ,"VARK")){

  # For matching
  classification = match.arg(classification)

  # Bind variables
  module = presentation = repeat_students = week_begin = week_end =
    active = reflective = sensing = visual = verbal = global = sequential =
    processing = perception = input = understanding =
    motivational = communicational = aural = read_write =
    forumng = oucollaborate = questionnaire = sharedsubpage =
    resource = quiz = oucontent = ouwiki = externalquiz = repeatactivity =
    glossary = dataplus = page = ouelluminate = url = subpage =
    folder = dualpane = htmlactivity = page = id_student = homepage = NULL

  # convert to FSLM model
  if(classification == "FSLM"){

    mapping = list(active = c("forumng", "oucollaborate", "questionnaire", "sharedsubpage"),
                   relective = c("resource", "quiz", "oucontent"),
                   sensing = c("ouwiki", "externalquiz", "repeatactivity", "glossary"),
                   visual = c("dataplus", "page"),
                   verbal = c("ouelluminate", "url"),
                   global = c("subpage", "folder"),
                   sequential = c("dualpane"))

  activity_data = activity_data %>% rowwise() %>%
      mutate(active = (ifelse("forumng" %in% names(activity_data), forumng, 0)  +
                         ifelse("oucollaborate" %in% names(activity_data), oucollaborate, 0) +
                         ifelse("questionnaire" %in% names(activity_data), questionnaire, 0) +
                         ifelse("sharedsubpage" %in% names(activity_data), sharedsubpage, 0)))

  activity_data = activity_data %>% rowwise() %>%
      mutate(reflective = (ifelse("resource" %in% names(activity_data), resource, 0)  +
                             ifelse("quiz" %in% names(activity_data), quiz, 0) +
                             ifelse("oucontent" %in% names(activity_data), oucontent, 0)))

  activity_data = activity_data %>% rowwise() %>%
      mutate(sensing = (ifelse("ouwiki" %in% names(activity_data), ouwiki, 0)  +
                          ifelse("externalquiz" %in% names(activity_data), externalquiz, 0) +
                          ifelse("repeatactivity" %in% names(activity_data), repeatactivity, 0) +
                          ifelse("glossary" %in% names(activity_data), glossary, 0)))

  activity_data = activity_data %>% rowwise() %>%
      mutate(visual = (ifelse("dataplus" %in% names(activity_data), dataplus, 0)  +
                         ifelse("page" %in% names(activity_data), page, 0)))

  activity_data = activity_data %>% rowwise() %>%
      mutate(verbal = (ifelse("ouelluminate" %in% names(activity_data), ouelluminate, 0)  +
                         ifelse("url" %in% names(activity_data), url, 0)))

  activity_data = activity_data %>% rowwise() %>%
      mutate(global = (ifelse("subpage" %in% names(activity_data), subpage, 0)  +
                         ifelse("folder" %in% names(activity_data), folder, 0)))

  activity_data = activity_data %>% rowwise() %>%
      mutate(sequential =  (ifelse("dualpane" %in% names(activity_data), dualpane, 0))) %>%
      select(id_student, active, reflective, sensing, visual, verbal, global, sequential)

    # For FSLSM
  }else if(classification == "FSLSM"){

    mapping = list(processing= c("resource", "oucollaborate", "ouwiki", "glossary", "htmlactivity"),
                   perception = c("oucontent", "questionnaire", "quiz", "externalquiz"),
                   input = c("dataplus", "dualpane", "folder", "page", "homepage", "resource",
                             "url", "ouelluminate", "subpage"),
                   understanding = c("repeatactivity", "sharedsubpage"))

    activity_data = activity_data %>% rowwise() %>%
      mutate(processing = (ifelse("resource" %in% names(activity_data), resource, 0)  +
                             ifelse("oucollaborate" %in% names(activity_data), oucollaborate, 0) +
                             ifelse("ouwiki" %in% names(activity_data), ouwiki, 0) +
                             ifelse("glossary" %in% names(activity_data), glossary, 0) +
                             ifelse("htmlactivity" %in% names(activity_data), htmlactivity, 0)))

    activity_data = activity_data %>% rowwise() %>%
      mutate(perception = (ifelse("oucontent" %in% names(activity_data), oucontent, 0)  +
                         ifelse("questionnaire" %in% names(activity_data), questionnaire, 0) +
                         ifelse("quiz" %in% names(activity_data), quiz, 0) +
                         ifelse("externalquiz" %in% names(activity_data), externalquiz, 0)))

    activity_data = activity_data %>% rowwise() %>%
      mutate(input = (ifelse("dataplus" %in% names(activity_data), dataplus, 0)  +
                         ifelse("dualpane" %in% names(activity_data), dualpane, 0) +
                         ifelse("folder" %in% names(activity_data), folder, 0) +
                         ifelse("page" %in% names(activity_data), page, 0) +
                         ifelse("homepage" %in% names(activity_data), homepage, 0) +
                         ifelse("resource" %in% names(activity_data), resource, 0) +
                         ifelse("url" %in% names(activity_data), url, 0) +
                         ifelse("ouelluminate" %in% names(activity_data), ouelluminate, 0) +
                         ifelse("subpage" %in% names(activity_data), subpage, 0)))

    activity_data = activity_data %>% rowwise() %>%
      mutate(understanding =  (ifelse("repeatactivity" %in% names(activity_data), repeatactivity, 0) +
                              ifelse("sharedsubpage" %in% names(activity_data), sharedsubpage, 0))) %>%
      select(id_student, processing, perception, input, understanding)

     # For OLS
  }else if(classification == "OLS"){

    mapping = list(verbal= c("resource", "ouwiki"),
                   visual = c("url", "ouelluminate"),
                   global = c("oucontent", "repeatactivity", "page"),
                   sensing = c("dualpane"),
                   motivational = c("homepage", "externalquiz", "quiz"),
                   communicational = c("forumng", "oucollaborate"),
                   sequential = c("subpage"))

    activity_data = activity_data %>% rowwise() %>%
      mutate(verbal = (ifelse("resource" %in% names(activity_data), resource, 0)  +
                         ifelse("ouwiki" %in% names(activity_data), ouwiki, 0)))

    activity_data = activity_data %>% rowwise() %>%
      mutate(visual = (ifelse("url" %in% names(activity_data), url, 0)  +
                         ifelse("ouelluminate" %in% names(activity_data), ouelluminate, 0)))

    activity_data = activity_data %>% rowwise() %>%
      mutate(global = (ifelse("oucontent" %in% names(activity_data), oucontent, 0)  +
                         ifelse("repeatactivity" %in% names(activity_data), repeatactivity, 0)  +
                         ifelse("page" %in% names(activity_data), page, 0)))

    activity_data = activity_data %>% rowwise() %>%
      mutate(sensing = (ifelse("dualpane" %in% names(activity_data), dualpane, 0)))

    activity_data = activity_data %>% rowwise() %>%
      mutate(motivational = (ifelse("homepage" %in% names(activity_data), homepage, 0)  +
                               ifelse("externalquiz" %in% names(activity_data), externalquiz, 0)  +
                               ifelse("quiz" %in% names(activity_data), quiz, 0)))

    activity_data = activity_data %>% rowwise() %>%
      mutate(communicational = (ifelse("forumng" %in% names(activity_data), forumng, 0)  +
                         ifelse("oucollaborate" %in% names(activity_data), oucollaborate, 0)))

    activity_data = activity_data %>% rowwise() %>%
      mutate(sequential = (ifelse("subpage" %in% names(activity_data), subpage, 0))) %>%
      select(id_student, verbal, visual, global, sensing, motivational, communicational, sequential)

    # For VARK model
  }else if(classification == "VARK"){

    mapping = list(visual= c("dataplus"),
                   aural = c("dataplus", "url", "ouelluminate", "oucollaborate"),
                   read_write = c("glossary", "ouwiki", "resource",
                                  "oucontent", "quiz", "externalquiz"))

    activity_data = activity_data %>% rowwise() %>%
      mutate(visual = (ifelse("dataplus" %in% names(activity_data), dataplus, 0)))

     activity_data = activity_data %>% rowwise() %>%
       mutate(aural = (ifelse("dataplus" %in% names(activity_data), dataplus, 0)  +
                          ifelse("url" %in% names(activity_data), url, 0)) +
                          ifelse("ouelluminate" %in% names(activity_data), ouelluminate, 0) +
                          ifelse("oucollaborate" %in% names(activity_data), oucollaborate, 0))

     activity_data = activity_data %>% rowwise() %>%
       mutate(read_write = (ifelse("glossary" %in% names(activity_data), glossary, 0) +
                         ifelse("ouwiki" %in% names(activity_data), ouwiki, 0)  +
                           ifelse("resource" %in% names(activity_data), resource, 0)  +
                           ifelse("oucontent" %in% names(activity_data), oucontent, 0) +
                           ifelse("quiz" %in% names(activity_data), quiz, 0) +
                            ifelse("externalquiz" %in% names(activity_data), externalquiz, 0))) %>%
      select(id_student, visual, aural, read_write)

  }

  return(list(VLE_classified_data = tibble(activity_data),
              classification = classification,
              mapping = mapping))

}
