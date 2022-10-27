test_that("Unknown file extensions are handled", {
  expect_error(loadDataSet("/a/a.ctv"))

})

test_that("handling Workbooks with multiple sheets", {

  wd<-getwd()
  print(wd)
  path<-paste(wd,"data/test.xlsx", sep="/")
  print(path)
  expect_equal(handling_excel(path), 5)

})
