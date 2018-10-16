# Question 1
rowDie<-function(n)
{
  k=0
  for(i in 1:n)
  {
    if(sum(sample(1:6,4)==6)>0)
    {
      k=k+1
    }
  }
  paste("You win",k,"times, win proportion is",round(k/n,4))
}
  
rowDie(1000)
rowDie(10000)

# Question 2
findMin<-function(x)
{
  if(length(x)==1)     # Check the length of the vector, if it equals 1, simply output the result.
  {
    paste("The smallest element is",x[1],"and index is 1.")
  }
  else
  {
  k=rep(1,times=length(x))     # Since there can be multiple minimum elements, set a vector to store the indexs.
  j=2
  for(i in 2:length(x))
  {
    if(x[i]<x[k[1]])      # If the element is smaller than the former smallest element, set it as the new smallest element and restart storing the index.
    {
      k[1]=i
      j=1
    }
    else
    {
      if(x[i]==x[k[1]])      # If the element is the same as the former smallest element, store the index.
      {
        k[j]=i
        j=j+1
      }
    }
  }
  s<-paste("The smallest element is",x[k[1]],"and index is")
  for(i in 1:j-1)
  {
    s<-paste(s,k[i])
  }
  s
  }
}

a=c(3,5,9,3,8,4,6,8,3)      # Some trials.
findMin(a)
b=c(1)
findMin(b)
c=c(2,2,2,2,2,2,2,2)
findMin(c)
