groups_with_one_observation <- function() {
  #create test rsg_data() object
  rsg <- c(rep("a", 9), "b")
  var <- rnorm(10, .5, .01)

  df<-data.frame(rsg,var)
  rsg_name_symbol<-sym("rsg")
  rsg_name<-"rsg"
  response<-"var"
  meta<-mget(c("rsg_name","rsg_name_symbol","response"))
  rsg_data<-list(full=df,meta=meta)

}

test_that("xbar one observation", {

  c.chart<-create_xbar(groups_with_one_observation())
  num_groups<-dim(c.chart$data)[1]
  expect_equal(num_groups,1)

})
