##Thanks to Le Tang for model solution

one.world.series <- function(p)
{
  number.A.wins <- 0
  number.B.wins <- 0
  series.over <- ifelse(max(number.A.wins,number.B.wins)==4, 1, 0)
  total.games <- number.A.wins + number.B.wins
  while(series.over == 0 ) {
    who.won <-sample(c('A','B'),1,prob = c(p,1-p))
    A.won <- ifelse(who.won=='A',1,0)
    B.won <- ifelse(who.won=='B',1,0)
    number.A.wins = number.A.wins + A.won
    number.B.wins = number.B.wins + B.won
    series.over <- ifelse(max(number.A.wins,number.B.wins)==4, 1, 0)
    total.games <- number.A.wins + number.B.wins
  }
  return(total.games)
}

#1. (5pts) 
sim.data <- data.frame(p.14 = replicate(1000,one.world.series(1/4)),
                       p.13 = replicate(1000,one.world.series(1/3)),
                       p.12 = replicate(1000,one.world.series(1/2)),
                       p.23 = replicate(1000,one.world.series(2/3)),
                       p.34 = replicate(1000,one.world.series(3/4)))

#2. (4pts)  
apply(sim.data,2,mean)

#3.  (5pts) 
theoretical.one.world.series <- function(p)
{
  q<-1-p
  y <- 0:3
  qy<-dnbinom(y,4,prob=q)
  py <- dnbinom(y,4,prob = p)
  pp<-qy+py
  return(pp)
}
sum(theoretical.one.world.series(1/4))
sum(theoretical.one.world.series(1/3))
sum(theoretical.one.world.series(1/2))
sum(theoretical.one.world.series(2/3))
sum(theoretical.one.world.series(3/4))

#4. (6pts) 
mean.theoretical <- function(p)
{
  x <- 4:7
  E <- sum(theoretical.one.world.series(p)*x)
  return(E)
}
p<- seq(0.01,0.99, by=0.01)
many.E<-lapply(p,mean.theoretical)

library(ggplot2)
p<- seq(0.01,0.99, by=0.01)
mydata<- data.frame(N=p,probs=unlist(many.E))
  ggplot(aes(x=N,y=probs),data=mydata)+geom_line()+ylab('E(Y)')+xlab('p')+
    ggtitle('function')
  
#For p = 0.5, E(Y) appear to be maximized because if team A and team B have the equal chance to win, 
#they are more likely to play more games.
