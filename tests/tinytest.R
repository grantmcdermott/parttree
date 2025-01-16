
if ( requireNamespace("tinytest", quietly=TRUE) ){
  Sys.setenv("OMP_THREAD_LIMIT" = 2) # https://github.com/Rdatatable/data.table/issues/5658
  tinytest::test_package("parttree")
}

