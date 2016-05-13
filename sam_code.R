#initializing matrices (Me,You,Turn)
V=array(NA,dim=c(99,99,105))
U=V

#boundary conditions

V[99,99,0]=1 #when we're both at 99, i'm gonna win
V[98,99,0]=5/6 #when I'm at 98, I'll win as long as I don't roll a 1
V[99,98,0]=1 #when they're at 98, i'm gonna win regardless
V[99,99,2:6]=1

options(expressions=500000)

V[m,y,k]=max(1-V[y,m+k,0],
             (1/6)*
               (1-V[y,m,0]+
                  V[m,y,k+2]+
                  V[m,y,k+3]+
                  V[m,y,k+4]+
                  V[m,y,k+5]+
                  V[m,y,k+6]))

rS=function(m,y,k){ 
  if (!anyNA(V[m+1,y+1,k+1])){ #if there it already exists
    return(V[m+1,y+1,k+1])
  } else { #if it doesn't, compute it
    answer = max(1-rS(y,m+k,0),
                 (1/6)*
                   (1-rS(y,m+1,0)+
                      rS(m,y,k+2)+
                      rS(m,y,k+3)+
                      rS(m,y,k+4)+
                      rS(m,y,k+5)+
                      rS(m,y,k+6))) 
    V[m+1,y+1,k+1]=answer
    return(answer)
  } 
}

gameStrategy=function(goal){
  V=array(NA,dim=c(goal,goal,goal+6)) 
  U=V
  V[goal-1+1,goal-1+1,0+1]=1
  V[goal-2+1,goal-1+1,0+1]=5/6
  V[goal-1+1,goal-2+1,0+1]=1  
  #V[goal-1+1,goal-1+1,3:7]=1
  for (m in (goal-1):0){
    V[(m+1),,(goal-m+1):(goal+5+1)]=1 # I win as soon as I hit 100-i
    for (y in (goal-1):0){
      for (k in (goal-m-1):0){
        V[m+1,y+1,k+1]=max(1-V[y+1,m+k+1,1],
                           (1/6)*
                             (1-V[y+1,m+1,0+1]+
                                V[m+1,y+1,k+2+1]+
                                V[m+1,y+1,k+3+1]+
                                V[m+1,y+1,k+4+1]+
                                V[m+1,y+1,k+5+1]+
                                V[m+1,y+1,k+6+1]))
      }
        
    }
    return (V)
  }
}

gameStrategy(4)
