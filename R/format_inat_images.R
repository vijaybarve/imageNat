#' Download formated iNaturalist images
#'
#' Download iNaturalist images for a recod and format them with metadata
#'
#' @param obs_id iNaturalist id of the record
#' @param outpath path to the output folder
#' @param verbose print out comments while processing the images
#' @family image functions
#' @importFrom rinat get_inat_obs_id
#' @importFrom magick image_read
#' @importFrom magick image_info
#' @importFrom magick image_border
#' @importFrom magick image_annotate
#' @importFrom magick image_write
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' format_inat_images(inatrecid)
#' }
#' @export
format_inat_images <- function(obs_id,outpath=".",verbose=TRUE){
  t <- get_inat_obs_id(obs_id)
  cat(paste("\n",obs_id," :",sep = ""))
  photoby <- t$user$name
  medurl <- t$observation_photos$photo$medium_url
  scname <- t$taxon$name
  cname <- t$taxon$default_name$name
  photodate <- t$observed_on
  place <- t$place_guess
  url <- t$uri
  no_images <- t$observation_photos_count
  if(no_images>0){
    for (img_no in 1:no_images){
      myimg <- image_read(medurl[img_no])
      loc = paste("+",image_info(myimg)$width+22,"+",image_info(myimg)$height+50,sep="")
      if(scname==cname){
        cname <- ""
      }
      myimg <- image_read(medurl[img_no]) %>%
        image_border( "black", "20x50") %>%
        image_annotate(paste("\u00A9",photoby,"   ",sep=""), color = "white", size = 20,  gravity = "southeast") %>%
        image_annotate(paste(" ",scname,"\n ",cname,sep=""), color = "white", size = 20, gravity = "northwest") %>%
        image_annotate(paste(" ",place,"\n ",photodate,sep=""), color = "white", size = 20, gravity = "southwest") %>%
        image_annotate(paste("",url,sep=""), color = "white", size = 15, degrees = 270,location=loc)
      filename <- paste(obs_id,"_",img_no,".jpg",sep="")
      image_write(myimg,path = paste(outpath,filename,sep=""),format = "jpg")
      cat("+")
    }
  }
}
