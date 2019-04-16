context("test-networking")

test_that("graph simplification returns a valid network", {
  konigsberg_boston <- konigsberg_graph(boston)
  undir_boston <- igraph::as.undirected(konigsberg_boston, mode = "collapse", edge.attr.comb = list("first"))

  simple_boston <- simplify_konigsbergr(konigsberg_boston, preserve_bridges = FALSE)
  preserved_simple_boston <- simplify_konigsbergr(konigsberg_boston, preserve_bridges = TRUE)

  expect_gt(vcount(undir_boston), vcount(simple_boston))
  expect_gt(vcount(undir_boston), vcount(preserved_simple_boston))
  expect_gt(ecount(undir_boston), ecount(simple_boston))
  expect_gt(ecount(undir_boston), ecount(preserved_simple_boston))
  expect_gt(ecount(preserved_simple_boston), ecount(simple_boston))
  expect_gt(vcount(preserved_simple_boston), vcount(simple_boston))

  expect_true(igraph::is.simple(simple_boston))
  expect_false(igraph::is.directed(simple_boston))

  expect_equal(sum(edge_attr(simple_boston, "distance")), sum(edge_attr(preserved_simple_boston, "distance")))
})
