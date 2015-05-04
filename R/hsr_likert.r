#' @title Plot five point Likert Scale data from HSR survey
#'
#' @description `plot_likert` generates a floating bar graph (horizontally) aligned
#' for survey data that is on lickert scale. Currently this function supports a a five
#' point Likert scale the with levels "Strongly Disagree", "Disagree", "Neutral",
#' "Agree", and "Strongly Agree"
#'
#'
#' @param .data `data.frame` of Healthy Schools and Regions data in long format
#' @param topic character vector of parent topics to filter form `.data`
#' @param ... arguments passed to \code{\link{pre_process_likert}}
#'
#' @examples
#' data(ex_hsr)
#'
#' plot_likert(ex_hsr,
#'             topic="Instructional Leadership",
#'             school_order=c("KHS1",
#'                             KMS1",
#'                             "KMS2",
#'                             "KHS2",
#'                             "KR",
#'                             "KN"))
#'
#' @export

plot_likert <- function(.data,
                        topic="Instructional Leadership", ...){
  data <- .data %>%
    dplyr::filter(topic_name %in% topic) %>%
    pre_process_likert(...)

  hsr_sum_pos_neg <- data %>%
    dplyr::group_by(topic_name, SQ, School, Sign) %>%
    dplyr::summarize(Sum=sum(measure_values)) %>%
    dplyr::mutate(x=ifelse(Sign == "Strongly Agree", 1.05, NA),
                  x=ifelse(Sign == "Strongly Disagree", -0.85, x),
                  x=ifelse(Sign == "Neutral", 0, x)
    )

  data_labels <- hsr_sum_pos_neg

  p <- ggplot(data %>%
                       dplyr::arrange(Measure.Names),
                     aes(y=School, x=x)) +
    geom_segment(aes(xend=xend,
                     yend=School,
                     color=Measure.Names,
                     alpha=School),
                     size=2.5) +
    #geom_vline(aes(xintercept=0), type=3, color="lightgray") +
    geom_text(data=data_labels %>%
                         dplyr::filter(Sign != "Neutral"),
                       aes(x=x,
                  y=School,
                  label=round(100 * Sum),
                  color=Sign,
                  alpha=School),
              size=1.75
    ) +
    geom_text(data=data_labels %>%
                         dplyr::filter(Sign == "Neutral" & Sum != 0),
                       aes(x=x,
                                    y=School,
                                    label=round(100 * Sum),
                                    alpha=School),
                      color="black",
                      size=1.75
    ) +
    scale_color_brewer("", palette="RdYlGn") +
    scale_x_continuous("",limits=c(-1,1.1),
                       breaks=seq(from=-.8, to=.8, by=.2),
                       labels=c("80%", "60%", "40%", "20%", "0%",
                                "20%", "40%", "60%", "80%")
    ) +
    scale_alpha_manual(values=c(.7,.7,1,1,1,1), guide=FALSE) +
    facet_wrap(~SQ, ncol=1) +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.y=element_text(size=5),
          plot.title=element_text(size=10)) +
    ggtitle(paste(topic, collapse = " | "))

    # return plot
    p
}

#' @title Preprocess HSR data for plotting Likert data
#'
#' @description `plot_likert` generates a floating bar graph (horizontally) aligned
#' for survey data that is on lickert scale. Currently this function supports a a five
#' point Likert scale the with levels "Strongly Disagree", "Disagree", "Neutral",
#' "Agree", and "Strongly Agree"
#'
#'
#' @param .data \code{data.frame} of Healthy Schools and Regions data in long format
#' @param school_order  order schools (abbreviated names) should be listed in
#' @param ... args passed to \code{\link{abbrev}}.
#' @export
#'

pre_process_likert <- function(.data,
                             school_order=c("KAP",
                                            "KAMS",
                                            "KCCP",
                                            "KBCP",
                                            "KC",
                                            "KN"),
                               ...) {
  #subset lickert measures
  lick_levels <- c("Strongly Disagree",
                   "Disagree",
                   "Neutral",
                   "Agree",
                   "Strongly Agree")

  hsr_lickert <- .data %>%
    dplyr::filter(measure_names %in% lick_levels) %>%
    dplyr::mutate(Measure.Names=factor(measure_names,
                                       levels=lick_levels,
                                       ordered=TRUE))

  #identify center level
  lick_names_len <- length(levels(hsr_lickert$Measure.Names))
  center <- (lick_names_len - 1) / 2 + 1
  center_name <- levels(hsr_lickert$Measure.Names)[center]
  neg_names <- levels(hsr_lickert$Measure.Names)[1: (center - 1)]
  pos_names <- levels(hsr_lickert$Measure.Names)[ (center + 1):lick_names_len]


  neu <- hsr_lickert %>%
    dplyr::filter(Measure.Names %in% center_name)

  pos <- hsr_lickert %>%
    dplyr::filter(Measure.Names %in% pos_names) %>%
    dplyr::arrange(Measure.Names) %>%
    dplyr::group_by(school, survey_question) %>%
    dplyr::mutate(x=cumsum(measure_values) - measure_values,
                  xend=x + measure_values
                  )

  neg <- hsr_lickert %>%
    dplyr::filter(Measure.Names %in% neg_names) %>%
    dplyr::arrange(desc(Measure.Names)) %>%
    dplyr::group_by(school, survey_question) %>%
    dplyr::mutate(x=cumsum(measure_values) - measure_values,
                  xend=x + measure_values,
                  x=-x,
                  xend=-xend
                  )

  neu <- neu %>%
    dplyr::mutate(x=-measure_values / 2,
                  xend=-x) %>%
    dplyr::ungroup()


  nn <- dplyr::left_join(neg,
                       neu %>%
                         dplyr::select(school, survey_question, adj=x),
                       by = c("school", "survey_question")) %>%
    dplyr::mutate(x=x + adj,
                  xend=xend + adj,
                  adj=NULL)

  neg <- nn

  pn <- dplyr::left_join(pos,
                         neu %>%
                           dplyr::select(school, survey_question, adj=xend),
                         by = c("school", "survey_question")) %>%
    dplyr::mutate(x=x + adj,
                  xend=xend + adj,
                  adj=NULL
                  )

  pos <- pn

  hsr_plot_data <- rbind(pos, neg, neu) %>%
    dplyr::mutate(Measure.Names=factor(as.character(Measure.Names),
                                       levels=lick_levels, ordered=TRUE),
                  School=abbrev(school, ...),
                  School=factor(School, levels=rev(school_order)),
                  Sign=ifelse(grepl("Agree", Measure.Names),
                              "Strongly Agree",
                              NA),
                  Sign=ifelse(grepl("Disagree", Measure.Names),
                              "Strongly Disagree",
                              Sign),
                  Sign=ifelse(grepl("Neutral", Measure.Names),
                              "Neutral",
                              Sign),
                  SQ=stringr::str_wrap(survey_question,
                                       width=90)
    )

  hsr_plot_data
}


#' @title Abberviate school names with exception substition
#'
#' @description `abbrev` abbreviates
#'
#'
#' @param x character vector of school names to abbreviatioe
#' @param exceptions list of two vectors of exceptions (character vector labeled \code{old}) and their
#' new abbreviations (character vector labeled \code{new})

abbrev <- function (x,
                    exceptions = list(old=c("KAPS", "KCMS"),
                                      new=c("KAP", "KCCP"))) {
  x.out <- gsub(pattern = "(\\w)\\w*\\W*", replacement = "\\1",
                x = x)
  if (!is.null(exceptions)) {
    x.changed <- exceptions$new[match(x.out, exceptions$old)]
    x.changed[is.na(x.changed)] <- x.out[is.na(x.changed)]
    x.out <- x.changed
  }
  x.out
}
