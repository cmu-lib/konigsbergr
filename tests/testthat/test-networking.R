context("test-networking")

test_that("graph simplification returns a valid network", {
  konigsberg_boston <- konigsberg_graph(boston)
  undir_boston <- igraph::as.undirected(konigsberg_boston, "collapse", edge.attr.comb = list("first"))
  simple_boston <- simplify_konigsbergr(konigsberg_boston)
  expect_true(igraph::is.simple(simple_boston))
  expect_false(igraph::is.directed(simple_boston))
  expect_equal(sum(edge_attr(undir_boston, "distance")), sum(edge_attr(simple_boston, "distance")))
})
