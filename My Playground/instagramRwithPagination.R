#Analyze Instagram with R

#Author: Julian Hillebrand
#modified by Cesar Espitia


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

#first time authenticating IG
ig_oauth <- oauth2.0_token(instagram, myapp,scope="basic",  type = "application/x-www-form-urlencoded",cache=FALSE)  
tmp <- strsplit(toString(names(ig_oauth$credentials)), '"')
token <- tmp[[1]][4]

#if you have already authenticated store your token here
#token <- "1313657908.f124169.17f3cf26bdeb4f49be43197f03dd46c8"

########################################################

#what are you looking up?


username <- "pearliaison"

#function for username
#search for the username
user_info <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/search?q=',username,'&access_token=',token,sep="")),unexpected.escape = "keep")
received_profile <- user_info$data[[1]]
df = data.frame()
df = data.frame(no = 1:length(media$data))
store <- data.frame()

if(grepl(received_profile$username,username)){
        user_id <- received_profile$id
        #Get recent media (20 pictures)
        media <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/',user_id,'/media/recent/?access_token=',token,sep="")))
        datetest <- Sys.time()
        while(isTRUE(datetest < as.POSIXct(backintime)) == "FALSE"){
                for(i in 1:length(media$data)){
                        #comments
                        df$comments[i] <-media$data[[i]]$comments$count
                        #likes
                        df$likes[i] <- media$data[[i]]$likes$count
                        #datestring
                        df$date[i] <- toString(as.POSIXct(as.numeric(media$data[[i]]$created_time), origin="1970-01-01")) 
                        #datetrue
                        df$datetrue[i] <- as.POSIXct(as.numeric(media$data[[i]]$created_time), origin="1970-01-01")
                        datetest <- min(df$date)
                        #pagination
                        if(i==1){
                                newstream <- media$pagination$next_url
                        }
                        print(i)
                }
                media <- fromJSON(getURL(paste(newstream,sep="")))
                store <- rbind(store,df)
        }
}

#Visualization
require(rCharts)

ginger <- mPlot(x = "date", y = c("likes", "comments"), type = "Line", data = store)
pearl <- mPlot(x = "date", y = c("likes", "comments"), type = "Line", data = store)
violet <- mPlot(x = "date", y = c("likes", "comments"), type = "Line", data = store)


#search for the hashtagterm
hashtag <- "teampearl"

user_info <- fromJSON(getURL(paste('https://api.instagram.com/v1/tags/search?q=',hashtag,'&access_token=',token,sep="")),unexpected.escape = "keep")
received_profile <- user_info$data[[1]]
df = data.frame()
df = data.frame(no = 1:length(media$data))
store <- data.frame()

#username GREPL sequence
#hashtag GREPL sequence

if (grepl(received_tags$name, hashtag))
{
          tagname <- received_profile$name
  #Get recent media (20 pictures)
  media <- fromJSON(getURL(paste('https://api.instagram.com/v1/tags/',tagname,'/media/recent/?access_token=',token,sep="")))
  datetest <- Sys.time()
  while(isTRUE(datetest < as.POSIXct(backintime)) == "FALSE"){
          for(i in 1:length(media$data)){
                  #comments
                  df$comments[i] <-media$data[[i]]$comments$count
                  #likes
                  df$likes[i] <- media$data[[i]]$likes$count
                  #datestring
                  df$date[i] <- toString(as.POSIXct(as.numeric(media$data[[i]]$created_time), origin="1970-01-01")) 
                  #datetrue
                  df$datetrue[i] <- as.POSIXct(as.numeric(media$data[[i]]$created_time), origin="1970-01-01")
                  datetest <- min(df$date)
                  #pagination
                  if(i==1){
                          newstream <- media$pagination$next_url
                  }
          }
          media <- fromJSON(getURL(paste(newstream,sep="")))
          store <- rbind(store,df)
  }
}

  #Visualization
  require(rCharts)
  
ginger <- mPlot(x = "date", y = c("likes", "comments"), type = "Line", data = store)
pearl <- mPlot(x = "date", y = c("likes", "comments"), type = "Line", data = store)
violet <- mPlot(x = "date", y = c("likes", "comments"), type = "Line", data = store)


teampearl <- mPlot(x = "date", y = c("likes", "comments"), type = "Line", data = store)
teamginger <- mPlot(x = "date", y = c("likes", "comments"), type = "Line", data = store)
  








