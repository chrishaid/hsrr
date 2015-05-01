context("Create a Likert Plot")

data("ex_hsr")

p <- plot_likert(ex_hsr, topic=c("Instructional Leadership"),
                 school_order=c("KHS1", "KMS1", "KMS2", "KHS2", "KR", "KN")
                 )

test_that("plot_likert creates a ggplot2 object", {
  expect_true(is.ggplot(p))

})
