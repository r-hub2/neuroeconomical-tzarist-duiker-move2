test_that("attributes of movebank in package",{
  aa<-movebank_download_study_info(
    license_type = "CC_0",
    i_have_download_access = TRUE,
    attributes="id"
  )  |> pull(id) |> head(20)


  res<-tibble::tibble()
  for(t in seq_along(aa))
  {
    cli_inform(t)
    ids<-movebank_retrieve(entity_type = "tag", study_id=aa[t]) |> pull(sensor_type_ids) |> unique()
    for(i in unique(na.omit(unlist(strsplit(ids,","))))){

      res<-dplyr::bind_rows(res,movebank_retrieve(entity_type = "study_attribute",
                                                  study_id=aa[t], sensor_type_id=i))
    }
  }
  ab<-res$short_name%in%names(move2:::mb_column_types_underscore)
  res[!ab,]
})
