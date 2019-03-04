context("test-routing")

test_that("route produces a pathfinder object", {
  suppressMessages({
    kb <- konigsberg_graph(boston) %>%
                   all_bridges() %>%
                   mark_bridges()
  })
  expect_message(boston_path <- cross_all_bridges(kb, quiet = FALSE))
  expect_is(boston_path, "pathfinder_path")
})
