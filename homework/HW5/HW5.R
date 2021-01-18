library(ggplot2)
ggplot()+geom_point(data=mpg,mapping=aes(x=displ,y=hwy,color=class))+
  geom_smooth(data=mpg,mapping=aes(x=displ,y=hwy))

#inner_join(gender,covar,by=c('family_id','ind_id'))   364,9 342,3 342,10


library(tidyverse)
ggplot(data = mpg, aes(y=class,x=drv))+geom_point()

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color=displ,size=displ))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))+facet_wrap(~cty)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

?facet_wrap

p <- ggplot(mpg, aes(displ, hwy)) + geom_point()

# Use vars() to supply faceting variables:
p + facet_wrap(vars(class))
p + facet_wrap(vars(class), switch='y')

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(se=FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color=drv)) + 
  geom_point() + 
  geom_smooth(mapping=aes(group=drv),se=FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(aes(color=drv)) + 
  geom_smooth(se=FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(aes(color=drv)) + 
  geom_smooth(mapping=aes(linetype=drv),se=FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(color='white',size=5)+
  geom_point(aes(color=drv))

library(nycflights13)
library(tidyverse)

x=flights
filter(flights,arr_delay>=120)
filter(flights,dest %in% c('IAH','HOU'))
filter(flights,carrier %in% c('UA','AA','DL'))
filter(flights,month %in% c(7,8,9))
filter(flights,arr_delay>=120 & dep_delay<=0)
# Were delayed by at least an hour, but made up over 30 minutes in flight
filter(flights,dep_delay>=60 & dep_delay>arr_delay+30)
#Departed between midnight and 6am (inclusive)
filter(flights,dep_time==2400 | (dep_time>=0 & dep_time<=600))
filter(flights,is.na(dep_time))

arrange(flights,desc(is.na(dep_time)))

arrange(flights,desc(distance))
arrange(flights,distance)

flights_new=mutate(flights,dep_time_minutes = (dep_time %/% 100 * 60 + dep_time %% 100)%%1440,
       sched_dep_time_minutes = (sched_dep_time %/% 100 * 60 + sched_dep_time %% 100)%%1440)
select(flights_new,dep_time,sched_dep_time,dep_time_minutes,sched_dep_time_minutes)

flights %>% 
  group_by(tailnum) %>%
  filter(arr_delay>0) %>%
  summarize(mean_delay = mean(arr_delay)) %>%
  arrange(desc(mean_delay))

flights %>% 
  group_by(hour) %>%
  filter(arr_delay>0) %>%
  summarize(mean_delay = mean(arr_delay)) %>%
  arrange(mean_delay)


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size=displ))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color=displ,size=displ))

 ggplot(data = mpg) + 
   geom_point(mapping = aes(x = displ, y = hwy))+facet_wrap(~cty)

ggplot(data = mpg) + 
      geom_point(mapping = aes(x = displ, y = hwy)) + 
    facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
     geom_point(mapping = aes(x = drv, y = cyl)) 

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

 ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
     geom_point() + 
     geom_smooth(se=FALSE)

(1,2)
 ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
     geom_point() + 
     geom_smooth(mapping=aes(group=drv),se=FALSE)

(2,1)
 ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color=drv)) + 
     geom_point() + 
     geom_smooth(mapping=aes(group=drv),se=FALSE)

(2,2)
 ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
     geom_point(aes(color=drv)) + 
     geom_smooth(se=FALSE)

(3,1)
 ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
     geom_point(aes(color=drv)) + 
     geom_smooth(mapping=aes(linetype=drv),se=FALSE)

(3,2)
 ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
     geom_point(color='white',size=5)+
     geom_point(aes(color=drv))

A = matrix(c(1,2,3,4),2,2)
A^2 
