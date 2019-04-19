context("test-routing")

test_that("route produces a pathfinder object", {
  suppressMessages({
    kb <- konigsberg_graph(boston)
  })

  expect_message(boston_path <- cross_all_bridges(kb))
  expect_is(boston_path, "pathfinder_path")
  expect_error(cross_all_bridges(kb, starting_node = 61173523, quiet = FALSE))
})
