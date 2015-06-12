#Analyze Instagram with R

#Author: Julian Hillebrand



#packages

require(httr)

require(rjson)

require(RCurl)





#Authentication



## getting callback URL

full_url <- oauth_callback()

full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")

#message <- paste("Copy and paste into Site URL on Instagram App Settings:", 

 #                full_url, "\nWhen done, press any key to continue...")



invisible(readline(message))



app_name <- "CesarEspitiaAPI"

client_id <- "f12416948dae432988869106bf024fe7"

client_secret <- "dee1d7ecc77d49c4b8879cbea9ae9bdf"

scope = "basic"







instagram <- oauth_endpoint(

  authorize = "https://api.instagram.com/oauth/authorize",

  access = "https://api.instagram.com/oauth/access_token")  

myapp <- oauth_app(app_name, client_id, client_secret)



#scope <- NULL

#ig_oauth <- oauth2.0_token(instagram, myapp,scope="basic",  type = "application/x-www-form-urlencoded",cache=FALSE)  

tmp <- strsplit(toString(names(ig_oauth$credentials)), '"')

token <- tmp[[1]][4]



########################################################



username <- "therock"
hashtag <- "teampearl"



#search for the username

user_info <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/search?q=',username,'&access_token=',token,sep="")),unexpected.escape = "keep")

#search for the hashtagterm

user_infoh <- fromJSON(getURL(paste('https://api.instagram.com/v1/tags/search?q=',hashtag,'&access_token=',token,sep="")),unexpected.escape = "keep")



received_profile <- user_info$data[[1]]
received_tags <- user_infoh$data[[1]]

#username GREPL sequence

if(grepl(received_profile$username,username))

{

  user_id <- received_profile$id

  #Get recent media (20 pictures)

  media <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/',user_id,'/media/recent?count=200&min_timestamp=0/?access_token=',token,sep="")))

  

  

  df = data.frame(no = 1:length(media$data))

  

  for(i in 1:length(media$data))

  {

    #comments

    df$comments[i] <-media$data[[i]]$comments$count

    

    #likes:

    df$likes[i] <- media$data[[i]]$likes$count

    

    #date

    df$date[i] <- toString(as.POSIXct(as.numeric(media$data[[i]]$created_time), origin="1970-01-01"))

  }
  }



#hashtag GREPL sequence

  if (grepl(received_tags$name, hashtag))

{

  tagname <- received_tags$name

  #Get recent media (20 pictures)

  mediah <- fromJSON(getURL(paste('https://api.instagram.com/v1/tags/',tagname,'/media/recent/?access_token=',token,sep="")))

  

  

  dfh = data.frame(no = 1:length(mediah$data))

  

  for(i in 1:length(mediah$data))

  {

    #comments

    dfh$comments[i] <-mediah$data[[i]]$comments$count
    #likes:
    dfh$likes[i] <- mediah$data[[i]]$likes$count
    #date

    dfh$date[i] <- toString(as.POSIXct(as.numeric(mediah$data[[i]]$created_time), origin="1970-01-01"))

  }
}
  

  #Visualization

  

  require(rCharts)

  

  m1 <- mPlot(x = "date", y = c("likes", "comments"), type = "Line", data = df)

  mtags <- mPlot(x = "date", y = c("likes", "comments"), type = "Line", data = dfh)

  








