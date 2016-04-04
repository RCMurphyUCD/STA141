install.packages("RSQLite")
library(RSQLite)
setwd("~/Downloads")
db = dbConnect(SQLite(), dbname = 'lean_imdbpy.db')


#1
Actors = dbGetQuery(db, 'SELECT COUNT (DISTINCT person_id) FROM cast_info WHERE role_id = 1 OR role_id = 2')


#2
Years = dbGetQuery(db, 'SELECT DISTINCT MIN(production_year), MAX(production_year)
                  FROM title')

#3 Gender
Male = dbGetQuery(db, 'SELECT COUNT (DISTINCT person_id) FROM cast_info WHERE role_id = 1')
Female = dbGetQuery(db, 'SELECT COUNT (DISTINCT person_id) FROM cast_info WHERE role_id = 2')

#4 #Pulled Movies (wasn't sure if TV movies or digital movies are somehow diff)
# And Pulled tv series for series
Movies = dbGetQuery(db, "SELECT COUNT(DISTINCT id) FROM title WHERE kind_id = 1")
Series = dbGetQuery(db, "SELECT COUNT(DISTINCT id) FROM title WHERE kind_id = 2")
#movie %
Movies/dbGetQuery(db, "SELECT COUNT(DISTINCT id) FROM title")
#series %
Series/dbGetQuery(db, "SELECT COUNT(DISTINCT id) FROM title")

#5
Genre = dbGetQuery(db, 'SELECT DISTINCT info FROM movie_info WHERE info_type_id =3')

#6 
Genre = dbGetQuery(db, 'SELECT info FROM movie_info WHERE info_type_id =3')
TopMovieGenre = dbGetQuery(db, 'SELECT info,
                  COUNT(title.id) FROM movie_info, 
                  title WHERE title.id = movie_info.movie_id
                  AND kind_id=1 AND info_type_id = 3 GROUP BY info
                  ORDER BY count(info) desc')
#7
#Find all movies with the keyword 'space'. How many are there?
#What are the years these were released? and who were the top 5 actors in each of these movies?
dbGetQuery(db, "SELECT DISTINCT keyword, id FROM keyword WHERE keyword = 'space'")
MovieSpaceNumber= dbGetQuery(db, " SELECT COUNT(title) FROM title WHERE kind_id = 1 AND (id) IN(SELECT movie_id FROM movie_keyword WHERE keyword_id = 9680)")
ProductionYears = dbGetQuery(db, " SELECT production_year FROM title WHERE kind_id = 1 AND (id) IN(SELECT movie_id FROM movie_keyword WHERE keyword_id = 9680)")
Cast = dbGetQuery(db, "SELECT DISTINCT(name), 
                   COUNT(name) FROM name, cast_info, 
                   title, movie_keyword WHERE title.kind_id = 1 AND 
                   cast_info.movie_id = title.id AND 
                   cast_info.person_id = name.id AND
                   title.id = movie_keyword.movie_id 
                    AND movie_keyword.keyword_id = 9680       
                  GROUP BY name ORDER BY COUNT(name) DESC LIMIT 20")

#8
test = dbGetQuery(db, 'SELECT info, production_year,
                  COUNT(title.id) FROM movie_info, 
                  title WHERE title.id = movie_info.movie_id
                  AND kind_id=1 AND info_type_id = 3 GROUP BY info, production_year')

newtest = test[!is.na(test$production_year),]
names(newtest) = c("info","year","count")
library(lattice)
xyplot(count~year|info, data = newtest, type = 'b',ylim = c(0,5000))
#9 Who are the actors that have been in the most movies? List the top 20.
Top20 = dbGetQuery(db, "SELECT DISTINCT(name), 
                   COUNT(name) FROM name, cast_info, 
                   title WHERE title.kind_id = 1 AND 
                   cast_info.movie_id = title.id AND 
                   cast_info.person_id = name.id GROUP BY 
                   name ORDER BY COUNT(name) DESC LIMIT 20")


#10



Top10Billing = dbGetQuery(db, "SELECT DISTINCT(name), 
                   COUNT(nr_order) FROM name, cast_info, 
                   title WHERE title.kind_id = 1 AND
                   cast_info.movie_id = title.id AND 
                   cast_info.person_id = name.id AND
                    nr_order <4
                    GROUP BY name, nr_order ORDER BY COUNT(nr_order) DESC LIMIT 10")

TopYears = dbGetQuery(db, "SELECT DISTINCT(name), 
                   COUNT(nr_order), title.production_year FROM name, cast_info, 
                   title WHERE title.kind_id = 1 AND
                   cast_info.movie_id = title.id AND 
                   cast_info.person_id = name.id AND
                    nr_order <4
                    GROUP BY name, nr_order, production_year ORDER BY COUNT(nr_order) DESC")

NTT = TopYears[TopYears[,1] %in% Top10Billing[,1],]
#11

#Who are the 10 actors that performed in the 
#most movies within any given year? 
#What are their names, the year they starred
#in these movies and the names of the movies?

zozo = dbGetQuery(db, "SELECT DISTINCT(name), 
                   COUNT(name), production_year FROM name, cast_info, 
                   title WHERE title.kind_id = 1 AND 
                   cast_info.movie_id = title.id AND 
                   cast_info.person_id = name.id GROUP BY 
                   name, production_year ORDER BY COUNT(name) DESC LIMI 20")
#12

TopAliases = dbGetQuery(db, "SELECT DISTINCT(name.name), 
                   COUNT(DISTINCT aka_name.name) FROM name, cast_info, aka_name 
                   WHERE cast_info.role_id=1 AND 
                   cast_info.person_id = name.id 
                  AND name.id = aka_name.id GROUP BY 
                   name.name ORDER BY COUNT(DISTINCT aka_name.name) DESC LIMIT 10")
#13
#Using Morgan Freeman as my actor
#since he would have an extreme network, I'm only taking the actors that have billing =1 to do this

MorganFreeManList = dbGetQuery(db, "SELECT name, movie_id FROM name,cast_info, title WHERE
title.kind_id = 1 AND cast_info.movie_id = title.id AND cast_info.nr_order = 1 AND
cast_info.person_id = name.id AND name != 'Freeman, Morgan' AND (cast_info.movie_id) 
IN(SELECT id FROM title WHERE kind_id = 1 AND (title.id) IN(SELECT DISTINCT movie_id 
           FROM cast_info, name WHERE name.id = 
      cast_info.person_id AND name = 'Freeman, Morgan')) GROUP BY name, movie_id")
Secondary = dbGetQuery(db, "SELECT name, movie_id FROM name,cast_info, title WHERE
title.kind_id = 1 AND cast_info.movie_id = title.id AND cast_info.nr_order = 1 AND
cast_info.person_id = name.id AND (cast_info.movie_id) IN(
SELECT DISTINCT movie_id FROM cast_info, name WHERE name.id =
cast_info.person_id AND (name) IN(
SELECT name FROM name,cast_info, title WHERE
title.kind_id = 1 AND cast_info.movie_id = title.id AND cast_info.nr_order = 1 AND
cast_info.person_id = name.id AND name != 'Freeman, Morgan' AND (cast_info.movie_id) 
IN(SELECT id FROM title WHERE kind_id = 1 AND (title.id) IN(SELECT DISTINCT movie_id 
           FROM cast_info, name WHERE name.id = 
      cast_info.person_id AND name = 'Freeman, Morgan')) GROUP BY name, movie_id
)) GROUP BY name, movie_id") 
MovieBridge = 
NEWDF = Secondary
LookToBridge = dbGetQuery(db, "SELECT name, movie_id FROM name, cast_info,title WHERE title.kind_id = 1
           AND cast_info.movie_id = title.id AND cast_info.nr_order = 1 AND cast_info.person_id = name.id")
Trial = LookToBridge[LookToBridge[,2]%in%NEWDF[,2] & LookToBridge[,1]%in%NEWDF[,1],]

tt = merge(Trial, NEWDF, by='movie_id')
tt = tt[,2:3]
tt2 = tt[tt[,1]!=tt[,2],]
tt3 = tt2[!duplicated(tt2),]
tt3$ORIGIN = "Freeman, Morgan"
tt4 = cbind(tt3[,3],tt3[,1], tt3[,2])
tt5 = tt4[tt4[,2]%in%MorganFreeManList[,1],]

install.packages("igraph")
library(igraph)

netty = graph.data.frame(tt5[1:100,], directed=F)
plot(netty)
