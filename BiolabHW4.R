library(ggplot2)
library(reshape2)

# question 1:

x1<-seq(-3,3,by=0.01)
x2<-x1+3
y<-dnorm(x1)
samp<-melt(data.frame(x1,x2,y),id="y")                    # set the sample data frame
p = ggplot() +
  geom_line(data = samp,
            aes(x=value,y=y,group=variable,colour=variable),
            show.legend = TRUE) +                        # draw line for the data
  geom_ribbon(data=samp[samp$variable=="x1"&samp$value>1.5,],
              aes(x=value,ymax=y,ymin=0,group=variable,colour=variable,fill=variable),
              alpha = 1) +                                # fill the type one error area
  geom_ribbon(data=samp[samp$variable=="x2"&samp$value<1.5,],
              aes(x=value,ymax=y,ymin=0,group=variable,colour=variable,fill=variable),
              alpha = 1) +                                # fill the type two error area
  geom_vline(xintercept = 1.5,linetype="dashed")+         # draw the middle dashed line
  annotate("text",x=0.85,y=0.02,label=expression(alpha),colour="black")+
  annotate("text",x=2.15,y=0.02,label=expression(beta),colour="white")+
  theme_classic()+
  theme(legend.position = c(0.15,1))+
  scale_color_manual(values = c("blue","red"))+
  scale_fill_manual(name="",values = c("blue","red"),breaks=c("x1","x2"),labels=c("Type I error","Type II error"))+
  labs(x=NULL,y=NULL)+
  scale_x_continuous(breaks = c(0,3),labels = c(expression(theta[0]),expression(theta[a])))
p
ggsave(filename = "HW4.png",plot = p,width = 8,height = 8)


# question 2:

CheckPrime<-function(x){
  if(x!=round(x))
  {
    return("Input must be an integer.")
  }
  else
  {
    if(x<=0)
    {
      return("Input must be larger than 0.")
    }
    else
    {
      if(x==1)
      {
        return("x is not a prime.")
      }
      else
      {
        i=2
        while(x/i!=round(x/i))
        {
          i=i+1
          if(i>x/2)
          {
            return("x is a prime")
          }
        }
        return("x is not a prime")
      }
    }
  }
}
