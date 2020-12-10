-------------------------------------------------------------------------------
  # Master of Science in Artificial Intelligence
  # Authors: Guillermo López
  # Date: 2020, November 3rd
------------------------------------------------------------------------------

chinese_checkers<-function(){
  #############################################################################
  ################################# FUNCTIONS #################################
  #############################################################################
  
  # ... to find whether the cell between is empty or not
  long_step_matrix <- function(x,y,state){
    ls <- matrix(1,nrow=length(x),ncol=length(x))
    for(i in 1:length(x)){
      for(j in 1:length(x)){
        x_m <- (x[i]+x[j])/2
        y_m <- (y[i]+y[j])/2
        p_m <- state[x==x_m & y==y_m]
        ls[i,j] <- ifelse(length(p_m)==0,1,ifelse(p_m!='black',1/16,1))
      }
    }
    return(ls)
  }
  
  # ... to recalculate distance matrix taking into acount restrictions and long steps.
  distance_matrix <- function(dist,state,ls,player){ 
    a <- state==player | state=='black'
    a_m <- matrix(0,nrow=length(state),ncol=length(state))
    diag(a_m) <- a
    c <- state == 'black'
    c_m <- matrix(0,nrow=length(state),ncol=length(state))
    diag(c_m) <- c
    final_dist <- crossprod(t(crossprod(a_m,dist)),c_m)
    final_dist <- ifelse(final_dist<=2*sqrt(5),final_dist,0)*ls
    final_dist <- ifelse(final_dist<=sqrt(5) & final_dist>0,final_dist,Inf)
    return(final_dist)
  }
  
  # ... to calculate minimal distance between cells
  floyd_warshall <- function(dist){
    n <- nrow(dist)
    for(i in 1:n){
      for(j in 1:n){
        for(k in 1:n){
          if(dist[i,j] > dist[i,k]+dist[k,j]){
            dist[i,j] <- dist[i,k]+dist[k,j]
          }
        }
      }
    }
    return(dist)
  }
  
  # ... provided play coordinates to validate if the play is legal
  validate_play <- function(coord,player,dist){
    c_ori <- cell[x==coord[1] & y==coord[2]]
    c_des <- cell[x==coord[3] & y==coord[4]]
    if(length(c_ori)==0 | length(c_des)==0){
      val <- 'invalid: cells'
    }else{
      if(state[c_ori]==player & state[c_des] == 'black'){
        if(dist[c_ori,c_des]<=sqrt(5)){
          val <- 'valid play'
        }else{
          val <- 'invalid: distance'
        }
      }else{
        val <- 'invalid: cells'
      }
    }
    return(val)
  }
  
  # ... artificial intelligence that select the best possible plays using random Lévy flights
  cuckoo_search<-function(x,y,cell,state,target,turn,player,dist,n=1000,v=20,p_rate=0.5){
    # n: nests quantity
    # v: number of random flights
    # p: desertion odds
    g_pla <- c('green','yellow','orange','red','purple','blue')
    g_x <- c(0,-12,-12,0,12,12)
    g_y <- c(-16,-8,8,16,8,-8)
    obj <- c(g_x[g_pla==player],g_y[g_pla==player])
    p_obj <- cbind(x[state=='black'],y[state=='black'])
    obj <- p_obj[which.min(sqrt(rowSums(t(t(p_obj)-obj)^2))),]
    # potenciales origenes
    p_ori <- cell[state==player]
    # potenciales destino
    p_des <- cell[state=='black']
    u_lim <- c(length(p_ori)-1,length(p_des)-1)
    # aceleraciones aleatorias
    alpha <- runif(2)
    # inicialización aleatoria de los nidos
    nests <- matrix(runif(n*2),ncol=2)
    creating_nests_cell <- function(nests,u_lim,p_ori,p_des){
      nests_cell <- round(t(t(nests)*u_lim)+1)
      nests_cell[,1] <- p_ori[nests_cell[,1]]
      nests_cell[,2] <- p_des[nests_cell[,2]]
      return(nests_cell)
    }
    nests_cell <- creating_nests_cell(nests,u_lim,p_ori,p_des)
    nests_coor <- matrix(c(x[nests_cell[,1]],y[nests_cell[,1]],x[nests_cell[,2]],
                           y[nests_cell[,2]]),ncol=4)
    fitness_function<-function(nest_coor,player,obj,min_distance){
      val <- apply(nests_coor,1,validate_play,player=player,dist=min_distance) == 'valid play'
      ori <- nests_coor[,1:2]
      des <- nests_coor[,3:4]
      pen <- ifelse(state[nests_cell[,1]]==target[nests_cell[,1]],1/turn,1)
      f_obj <- ((rowSums(t(t(ori)-obj)^2)-rowSums(t(t(des)-obj)^2))*val)*pen
      return(f_obj)
    }
    f_obj <- fitness_function(nest_coor,player,obj,min_distance)
    for(i in 1:v){
      # condición para desertar el nido
      cond <- quantile(f_obj,p_rate)
      levi <- matrix(runif(n*2)*2-1,ncol=2)*alpha
      # opera únicamente en los nidos que cumplan la condición de deserción
      levi <- levi*(f_obj==cond)
      nests <- nests+levi
      # los nuevos nidos solo pueden ser opciones validas
      nests <- ifelse(nests<0,0,ifelse(nests>1,1,nests))
      nests_cell <- creating_nests_cell(nests,u_lim,p_ori,p_des)
      nests_coor <- matrix(c(x[nests_cell[,1]],y[nests_cell[,1]],x[nests_cell[,2]],
                             y[nests_cell[,2]]),ncol=4)
      f_obj <- fitness_function(nest_coor,player,obj,min_distance)
    }
    best_play <- nests_coor[which.max(f_obj),]
    return(best_play)
  }
  
  # ... to plot the current state of the board
  plot_tabletop<-function(x,y,state){
    par(pin=c(3.5,3.5))
    plot(x,y,col=state,pch=20,cex=3,xlab=NULL,ylab=NULL)
    abline(h=seq(-16,16,2),lty=2,col='lightgray')
    abline(v=seq(-12,12),lty=2,col='lightgray')
  }
  
  #############################################################################
  ########################## INITIALIZING THE BOARD ###########################
  #############################################################################
  # chinesse checkers board has 121 cells
  cell<-seq(1:121)
  low_index<-c(0,-1,-2,-3,-12,-11,-10,-9,-8,-9,-10,-11,-12,-3,-2,-1,0)
  up_index<--low_index
  # horizontal tabletop coordinates
  x<-c()
  for(i in 1:length(low_index)){
    x<-c(x,seq(low_index[i],up_index[i],2))
  }
  rm(low_index,up_index)
  index<-seq(16,-16,-2)
  times<-c(1,2,3,4,13,12,11,10,9,10,11,12,13,4,3,2,1)
  # vertical tabletop coordinates
  y<-c()
  for(i in 1:length(index)){
    y<-c(y,rep(index[i],times[i]))
  }
  rm(index,times)
  # initial cells state
  state<-c(rep('green',10),rep('blue',4),rep('black',5),rep('yellow',4),rep('blue',3),
           rep('black',6),rep('yellow',3),rep('blue',2),rep('black',7),rep('yellow',2),'blue',
           rep('black',8),'yellow',rep('black',9),'purple',rep('black',8),'orange',rep('purple',2),
           rep('black',7),rep('orange',2),rep('purple',3),rep('black',6),rep('orange',3),
           rep('purple',4),rep('black',5),rep('orange',4),rep('red',10))
  # game finishes when all cells reach target color
  target<-c(rep('red',10),rep('orange',4),rep('blank',5),rep('purple',4),rep('orange',3),
           rep('blank',6),rep('purple',3),rep('orange',2),rep('blank',7),rep('purple',2),'orange',
           rep('blank',8),'purple',rep('blank',9),'yellow',rep('blank',8),'blue',rep('yellow',2),
           rep('blank',7),rep('blue',2),rep('yellow',3),rep('blank',6),rep('blue',3),
           rep('yellow',4),rep('blank',5),rep('blue',4),rep('green',10))
  # simple distances matrix
  dist <- matrix(0,nrow=length(x),ncol=length(x))
  for(i in 1:length(x)){
    for(j in 1:length(x)){
      x_d <- (x[i]-x[j])^2
      y_d <- (y[i]-y[j])^2
      dist[i,j] <- sqrt(x_d+y_d)
    }
  }
  active_players<-c('green','yellow','orange','red','purple','blue')
  #############################################################################
  ################################# PLAYING ###################################
  #############################################################################
  plot_tabletop(x,y,state)
  log <- c()
  turn <- 0
  movement <- 0
  n_active <- length(active_players)
  while(n_active>0){
    turn <- turn+1
    for(player in active_players){
      movement <- movement+1
      ls <- long_step_matrix(x,y,state)
      final_dist <- distance_matrix(dist,state,ls,player)
      min_distance <- floyd_warshall(final_dist)
      coord <- cuckoo_search(x,y,cell,state,target,turn,player,min_distance)
      val <- validate_play(coord,player,min_distance)
      if(val=='valid play'){
        # storing game log
        log <- c(log,turn,movement,state,coord)
        # changing tabletop status according to given play
        state[x==coord[1] & y==coord[2]] <- 'black'
        state[x==coord[3] & y==coord[4]] <- player
        plot_tabletop(x,y,state)
      }else{
        print(val)
      }
      # checking if an active player won in this turn
      if(min(state[state==player]==target[state==player])==1){
        active_players <- active_players[active_players!=player]
        n_active <- length(active_players)
        print(paste('Congratulations!',player,turn))
      }
    }
  }
  log <- matrix(log,ncol=127,byrow=T)
}
