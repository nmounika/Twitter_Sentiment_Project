# USER ANALYSIS

useranalysis <- function(mydf)
{
  users <- lookupUsers(as.character(mydf$screenName))
  n <- length(users)
  
  # initialize vectors
  user_names <- as.vector(rep("", n))
  user_followersCount <- as.vector(rep(0, n))
  user_friendsCount <- as.vector(rep(0, n))
  user_verified <- as.vector(rep(0,n))
  
  # extract user object data
  for (i in 1:n)
  {
    user_names[i] <- users[[i]]$screenName
    user_followersCount[i] <- users[[i]]$followersCount
    user_friendsCount[i] <- users[[i]]$friendsCount
    user_verified[i] <- users[[i]]$verified
  }
  
  # combine the vectors into a data frame
  userdf <- as.data.frame(cbind(user_names,  user_followersCount, user_friendsCount, user_verified))
  colnames(userdf) <- c("screenName", "followersCount", "friendsCount", "verified")
  userdf$followersCount <- as.numeric(as.character(userdf$followersCount))
  userdf$friendsCount <- as.numeric(as.character(userdf$friendsCount))
  userdf$verified <- as.numeric(as.character(userdf$verified))
  
  return(userdf)
}

# use function
userinfo <- useranalysis(df)
df_analysis <-left_join(df, userinfo, by="screenName")
