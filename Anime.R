#download file
anime.df <- read.csv(file = "/Users/ryancoulter/Desktop/Anime.csv")

#checking it out
summary(anime.df)

#removing columns that are unnecesary
anime.df <- anime.df[,-4] #japanese title, cannot understand it
which(colnames(anime.df) == "premiered")
anime.df <- anime.df [,-24]
which(colnames(anime.df) == "broadcast")
anime.df <- anime.df [,-24]
which(colnames(anime.df) == "background")
anime.df <- anime.df [,-23]
which(colnames(anime.df) == "related")
anime.df <- anime.df [,-23]

#new summary
summary(anime.df)

#top ten
anime.df.topten <- subset(anime.df, anime.df$rank < 11 & anime.df$rank != 0)
anime.df.topten <- anime.df.topten[order(anime.df.topten$rank),] 

#movies
anime.df.movies <- subset(anime.df, anime.df$type == "Movie")

#action and shounen
        anime.df.action <- subset(anime.df, grepl("Action", anime.df$genre))
        
        #all action and shounen
        anime.df.ryan <- subset(anime.df.action, grepl("Shounen", anime.df.action$genre))
        
        #tv only
        anime.df.ryan <- subset(anime.df.ryan, anime.df.ryan$type == "TV")
        
        #score > 8
        anime.df.ryan <- subset(anime.df.ryan, anime.df.ryan$score > 8)
        
        #less than 100 episodes
        anime.df.ryan <- subset(anime.df.ryan, anime.df.ryan$episodes < 100)
        
        
        #find new anime based on score and number of favorites
        library(ggplot2)
        e <- ggplot(anime.df.ryan, aes (score,favorites))
        e + geom_jitter(height = 2, width = 2)
        e + geom_label(aes(label = anime.df.ryan$name))
        
        #getting rid of fullmetal and aot becuase i have already seen them (and to make graph better)
        anime.df.ryan <- subset(anime.df.ryan, favorites < 50000)
        
        e <- ggplot(anime.df.ryan, aes (score,favorites))
        e + geom_jitter(height = 2, width = 2)
        e + geom_label(aes(label = anime.df.ryan$name))
        
        f <- ggplot(anime.df.ryan, aes(name,popularity))
        f + geom_col()

#Psychological
        anime.df.psycho <- subset(anime.df, grepl("Psychological", anime.df$genre))
        
        anime.df.psycho <- subset(anime.df.psycho, anime.df.psycho$type == "TV")
        
        anime.df.psycho <- subset(anime.df.psycho, anime.df.psycho$score > 8)
        
        anime.df.psycho <- subset(anime.df.psycho, anime.df.psycho$episodes < 100)
        
        anime.df.psycho <- subset(anime.df.psycho, favorites < 50000) #rid of death note, already seen
        
        e <- ggplot(anime.df.psycho, aes (score,favorites))
        e + geom_jitter(height = 2, width = 2)
        e + geom_label(aes(label = anime.df.psycho$name))


