#' Download iNaturalist images
#'
#' Download iNaturalist images
#'
#' @param dat data frame containing taxonomic list
#' @param size image size. Values accepted are "small", "medium", "large", "original". Deafault(medium).
#' @param outpath path to the output folder
#' @family image functions
#' @importFrom utils download.file
#' @examples
#' \dontrun{
#' downlaod_images(inatdata)
#' }
#' @export
downlaod_images <- function(dat,size="medium",outpath="."){
  for (i in 1:dim(dat)[1]){
    iurl <- dat$image_url[i]
    iurl <- gsub("medium",size,iurl)
    iname <- paste(outpath,dat$id[i]," ",dat$taxon_species_name[i]," ",dat$observed_on[i],".jpg",sep="")
    download.file(iurl,iname, mode = 'wb')
  }
}
