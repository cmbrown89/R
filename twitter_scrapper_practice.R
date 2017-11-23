library("rtweet")

setwd("/Users/AwesomeSauce/Desktop/Github_Repos/R") #In github folder

#Stream keywords
kw = "trump,terrible,hillary"

#Stream with 1 minute time-out (in seconds)
streamtime = 60

#Save data to json file
filename = "politics.json"

#Stream tweets
#Stream2 reconnects if disconnected 
tweets = stream_tweets2(q = kw, timeout = streamtime, file_name = filename)

#Parse json file; could also do this with parse = TRUE in above command
rt = parse_stream(filename)

#See users' data
users_data(rt)

#Plot time series of all tweets aggregated by second
ts_plot(rt, by = "secs")




