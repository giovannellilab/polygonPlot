test_that(".get_lengths returns correct values", {
  
  df_coord = read.csv(test_path("testdata", "df_coord.csv"))
  
  df_result = data.frame()
  for (pol_shape in unique(df_coord$shape)) {
    shape_df = df_coord %>%
      dplyr::filter(shape == pol_shape) %>%
      select(-"shape")
    shape_df = data.frame(
      length=.get_lengths(shape_df),
      shape=pol_shape
    )
    df_result = df_result %>% bind_rows(shape_df)
  }
  
  df_expected = read.csv(test_path("testdata", "df_coord_lengths.csv"))
  
  expect_equal(
    object=df_result,
    expected=df_expected
  )
})
