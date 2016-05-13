#initializing matrices (Me,You,Turn)
V=array(NA,dim=c(99,99,105))
U=V

#boundary conditions

V[99,99,0]=(5/6) #when we're both at 99, whoever's turn it is will win as long as they don't roll a 1.
V[98,99,0]=(5/6) #when I'm at 98, I'll win as long as I don't roll a 1
V[99,98,0]=(5/6) #when they're at 98, I'll win as long as I don't a 1 

V[m,y,k]=max(1-V[y,m+k,0],
             (1/6)*
               (1-V[y,m,0]+
                  V[m,y,k+2]+
                  V[m,y,k+3]+
                  V[m,y,k+4]+
                  V[m,y,k+5]+
                  V[m,y,k+6]))

rS=function(m,y,k){ 
  if (!anyNA(V[m,y,k])){ #if there it already exists
    return(V[m,y,k])
  } else { #if it doesn't, compute it
    answer = max(1-rS(y,m+k,0),
                 (1/6)*
                   (1-rS(y,m,0)+
                      rS(m,y,k+2)+
                      rS(m,y,k+3)+
                      rS(m,y,k+4)+
                      rS(m,y,k+5)+
                      rS(m,y,k+6))) 
    return(answer)
  } 
}

gameStrategy=function(goal){
  V=array(NA,dim=c(goal-1,goal-1,goal+5)) 
  U=V
  V[goal-1,goal-1,0]=5/6
  V[goal-2,goal-1,0]=5/6
  V[goal-1,goal-2,0]=5/6  
}