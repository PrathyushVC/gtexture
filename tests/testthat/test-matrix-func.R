#setup

df <- data.frame(from = c("a", "b", "c", "d", "a", "e", "e", "b"),
                 to = c("b", "a", "a", "a", "e", "b", "c", "d"))
g_named = igraph::graph_from_data_frame(df)
vals_named = 1:5
names(vals_named) = letters[1:5]

test_that("co-occurence matrix calculation works with igraph obj with named nodes", {
 comat = get_comatrix(g_named, vals_named)
 expect_equal(colnames(comat), as.character(1:5)) #make sure cols are in the right order
 expect_equal(nrow(comat), ncol(comat)) #make sure its square
 expect_equal(sum(is.na(comat)), 0) #make sure no NAs snuck through
 expect_equal(sum(comat>=1), 0) #make sure it got normalized
})

g = igraph::sample_gnp(n=10, p=0.2)
vals = 1:10
test_that("co-occurence matrix calculation works with igraph obj with unnamed nodes", {
  comat = get_comatrix(g, vals)
  expect_equal(colnames(comat), as.character(1:10)) #make sure cols are in the right order
  expect_equal(nrow(comat), ncol(comat)) #make sure its square
  expect_equal(sum(is.na(comat)), 0) #make sure no NAs snuck through
  expect_equal(sum(comat>=1), 0) #make sure it got normalized
})

vals= 1:9
test_that("co-occurence matrix calculation breaks with unnamed nodes when not enough values are provided", {
  expect_error(get_comatrix(g, vals))
})


