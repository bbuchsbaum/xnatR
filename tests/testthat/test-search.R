# Test search helper functions

test_that("search_criterion creates valid criteria", {
  skip_if_not_installed("xnatR")
  crit <- search_criterion("xnat:subjectData/gender", "EQUALS", "M")

  expect_type(crit, "list")
  expect_equal(crit$field, "xnat:subjectData/gender")
  expect_equal(crit$comparison, "EQUALS")
  expect_equal(crit$value, "M")
})

test_that("search_criterion normalizes comparison to uppercase", {
  skip_if_not_installed("xnatR")
  crit <- search_criterion("field", "equals", "value")
  expect_equal(crit$comparison, "EQUALS")

  crit <- search_criterion("field", "Like", "value")
  expect_equal(crit$comparison, "LIKE")
})

test_that("search_criterion rejects invalid comparisons", {
  skip_if_not_installed("xnatR")
  expect_error(search_criterion("field", "INVALID", "value"), "Invalid comparison")
})

test_that("xnat_search_builder creates builder object", {
  skip_if_not_installed("xnatR")
  builder <- xnat_search_builder("xnat:mrSessionData")

  expect_s3_class(builder, "xnat_search_builder")
  expect_equal(builder$root_type, "xnat:mrSessionData")
  expect_length(builder$fields, 0)
  expect_length(builder$criteria, 0)
})

test_that("search_select adds fields to builder", {
  skip_if_not_installed("xnatR")
  builder <- xnat_search_builder("xnat:mrSessionData") |>
    search_select("xnat:mrSessionData/ID", "xnat:mrSessionData/label")

  expect_length(builder$fields, 2)
  expect_equal(builder$fields[1], "xnat:mrSessionData/ID")
  expect_equal(builder$fields[2], "xnat:mrSessionData/label")
})

test_that("search_where adds criteria to builder", {
  skip_if_not_installed("xnatR")
  builder <- xnat_search_builder("xnat:mrSessionData") |>
    search_where("xnat:mrSessionData/project", "EQUALS", "MyProject")

  expect_length(builder$criteria, 1)
  expect_equal(builder$criteria[[1]]$field, "xnat:mrSessionData/project")
  expect_equal(builder$criteria[[1]]$comparison, "EQUALS")
  expect_equal(builder$criteria[[1]]$value, "MyProject")
})

test_that("search_execute requires fields", {
  skip_if_not_installed("xnatR")
  builder <- xnat_search_builder("xnat:mrSessionData")
  expect_error(search_execute(builder), "No fields selected")
})

test_that("build_search_xml generates valid XML structure", {
  skip_if_not_installed("xnatR")
  xml <- xnatR:::build_search_xml(
    root_type = "xnat:subjectData",
    fields = c("xnat:subjectData/ID"),
    criteria = list(
      list(field = "xnat:subjectData/project", comparison = "EQUALS", value = "TEST")
    )
  )

  expect_type(xml, "character")
  expect_match(xml, "xnat:subjectData")
  expect_match(xml, "<xdat:element_name>xnat:subjectData</xdat:element_name>", fixed = TRUE)
  expect_match(xml, "<xdat:field_ID>xnat:subjectData/ID</xdat:field_ID>", fixed = TRUE)
  expect_match(xml, "EQUALS")
  expect_match(xml, "TEST")

  # One <xdat:search_field> per requested field (no extra wrapper element).
  tag_positions <- gregexpr("<xdat:search_field>", xml, fixed = TRUE)[[1]]
  expect_equal(sum(tag_positions != -1), 1L)
})

test_that("build_search_xml escapes XML special characters", {
  skip_if_not_installed("xnatR")
  xml <- xnatR:::build_search_xml(
    root_type = "xnat:subjectData",
    fields = c("xnat:subjectData/ID"),
    criteria = list(
      list(field = "xnat:subjectData/label", comparison = "EQUALS", value = "A&B<C>")
    )
  )

  expect_match(xml, "A&amp;B&lt;C&gt;", fixed = TRUE)
})
