

filter_annotated_bins <- function(bins_data,annotation_data,ignore_columns,number_of_decimal_places=4){

annotation_data <- annotation_data
bins_long_format <- tidyr::gather(bins_data,paste0("-",eval(ignore_columns)),key="bins",value="relative_intensity")

bins_long_format$bins <- round(as.numeric(bins_long_format$bins),number_of_decimal_places)

annotation_data$bins <- annotation_data[[2]]
bins_long_format <- dplyr::filter(bins_long_format,bins%in%annotation_data$bins)
bins_long_format <- dplyr::left_join(bins_long_format,annotation_data,by="bins")
bins_long_format$bins <- NULL
}


