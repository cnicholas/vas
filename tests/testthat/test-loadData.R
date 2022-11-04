local_test_data_path_excel <- function(env = parent.frame()) {

  wd<-getwd()

  path<-paste(wd,"data/test.xlsx", sep="/")
}
local_test_data_path_csv <- function(env = parent.frame()) {

  wd<-getwd()

  path<-paste(wd,"data/carloandata.csv", sep="/")
}

test_that("Unknown file extensions are handled", {
  expect_error(loadDataSet("/a/a.ctv"))

})

test_that("handling Workbooks with multiple sheets", {

  path<-local_test_data_path_excel()
  print(path)
  expect_equal(handling_excel(path), 5)

})
test_that("Excel files load with parameters",{
  path<- local_test_data_path_excel()

  data<-loadDataExcel(path)
  expect_equal(nrow(data),16)

  data<-loadDataExcel(path, sheet="Sheet1")
  expect_equal(nrow(data),9)
})

test_that("Text files load with parameters",{
 path<-local_test_data_path_csv()
 data<-loadDataText(path)
  print(nrow(data))
  expect_equal(nrow(data),100)
})
