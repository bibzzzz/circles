library(grid)
library(ggplot2)

output_dir <- "/Users/amydonaldson/Documents/Habib/dev/Circles/"
angle <- 28
n_iter <- 3000
size <- 20

grid.circle(x=0.5, y=0.5, r=0.5, default.units="npc", name=NULL,
            gp=gpar(), draw=TRUE, vp=NULL)

circleFun <- function(center = c(0,0),diameter = 2, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
start_x = 0.3
angle=27
npoints=100

pathFun <- function(start_x = 1, angle = 27, npoints = 100){
  start_y <- sqrt(1-start_x^2)
  
  i <- 0
  xy_data <- data.frame(iter=0,x=start_x,y=start_y)
  x <- start_x
  y <- start_y
  while (i<npoints){
    if (i%%2==0){
      int <- y - (angle/45)*x
      root <- polyroot(c(int^2-1,2*(angle/45)*int,(angle^2/45^2)+1))
      new_x <- Re(root)[abs(Re(root)-x)==max(abs(Re(root)-x))]
      new_y <- int + (angle/45)*new_x
      x <- new_x
      y <- new_y
    }else{
      int <- y - (-angle/45)*x
      root <- polyroot(c(int^2-1,-2*(angle/45)*int,(angle^2/45^2)+1))
      new_x <- Re(root)[abs(Re(root)-x)==max(abs(Re(root)-x))]
      new_y <- int + (-angle/45)*new_x
      x <- new_x
      y <- new_y
    }
    i <- i + 1
    xy_data <- rbind(xy_data,c(i,x,y))
  }
  
  return(data.frame(xy_data))
}


xy_data <- pathFun(0,angle,n_iter)
# dat <- circleFun(c(0,0),2,npoints = 2000)
#geom_path will do open circles, geom_polygon will do filled circles
# ggplot(dat,aes(x,y)) + geom_path()

# ggplot(xy_data, aes(x, y)) + geom_path(aes(colour=iter))

plot <- ggplot(xy_data, aes(x, y)) + geom_path() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        #panel.background=element_rect(fill = 'black'),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
ggsave(paste0(output_dir,angle,"_",n_iter,"_",size,".png"), plot, height = size, width = size)

# ggplot(xy_data, aes(x, y,colour="red")) + geom_path() + 
#   #scale_color_manual(values=c("cyan")) +
#   theme(axis.line=element_blank(),
#                                                  axis.text.x=element_blank(),
#                                                  axis.text.y=element_blank(),
#                                                  axis.ticks=element_blank(),
#                                                  axis.title.x=element_blank(),
#                                                  axis.title.y=element_blank(),
#                                                  legend.position="none",
#                                                  #panel.background=element_rect(fill = 'black'),
#                                                  panel.border=element_blank(),
#                                                  panel.grid.major=element_blank(),
#                                                  panel.grid.minor=element_blank(),
#                                                  plot.background=element_blank())


#ggplot(xy_data, aes(x, y, fill=iter)) + geom_polygon()
