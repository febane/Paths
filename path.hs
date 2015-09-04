{-
*********************************
Paths in a Matrix
@author Fernando Barbosa Neto
*********************************
21/07/2014
*********************************
-}

caminhos n (i,j) = if i<1 || j<1 || i>n || j>n
		   then 0
		   else if (i==1&&j==1) || (i==1&&j==n) || (i==n&&j==1) || (i==n&&j==n)
		   	then 2
		   	else if i==1 || j==1 || i==n || j==n
		   	     then 4
		   	     else 7

proximoscaminhos (x,y) (i,j) n p | x-i==(-1) && y-j==(-1) = possibilidades2 (i,j) (i+1,j+1) n (p-1) + possibilidades2 (i,j) (i+1,j) n (p-1) + possibilidades2 (i,j) (i+1,j-1) n (p-1) + possibilidades2 (i,j) (i,j+1) n (p-1) + possibilidades2 (i,j) (i,j-1) n (p-1) + possibilidades2 (i,j) (i-1,j+1) n (p-1) + possibilidades2 (i,j) (i-1,j) n (p-1)
				 | x-i==(-1) && y-j==0 = possibilidades2 (i,j) (i+1,j+1) n (p-1) + possibilidades2 (i,j) (i+1,j) n (p-1) + possibilidades2 (i,j) (i+1,j-1) n (p-1) + possibilidades2 (i,j) (i,j+1) n (p-1) + possibilidades2 (i,j) (i,j-1) n (p-1) + possibilidades2 (i,j) (i-1,j+1) n (p-1) + possibilidades2 (i,j) (i-1,j-1) n (p-1)
				 | x-i==(-1) && y-j==1 = possibilidades2 (i,j) (i+1,j+1) n (p-1) + possibilidades2 (i,j) (i+1,j) n (p-1) + possibilidades2 (i,j) (i+1,j-1) n (p-1) + possibilidades2 (i,j) (i,j+1) n (p-1) + possibilidades2 (i,j) (i,j-1) n (p-1) + possibilidades2 (i,j) (i-1,j) n (p-1) + possibilidades2 (i,j) (i-1,j-1) n (p-1)
				 | x-i==0 && y-j==(-1) = possibilidades2 (i,j) (i+1,j+1) n (p-1) + possibilidades2 (i,j) (i+1,j) n (p-1) + possibilidades2 (i,j) (i+1,j-1) n (p-1) + possibilidades2 (i,j) (i,j+1) n (p-1) + possibilidades2 (i,j) (i-1,j+1) n (p-1) + possibilidades2 (i,j) (i-1,j) n (p-1) + possibilidades2 (i,j) (i-1,j-1) n (p-1)
				 | x-i==0 && y-j==0 = possibilidades2 (i,j) (i+1,j+1) n (p-1) + possibilidades2 (i,j) (i+1,j) n (p-1) + possibilidades2 (i,j) (i+1,j-1) n (p-1) + possibilidades2 (i,j) (i,j+1) n (p-1) + possibilidades2 (i,j) (i,j-1) n (p-1) + possibilidades2 (i,j) (i-1,j+1) n (p-1) + possibilidades2 (i,j) (i-1,j) n (p-1) + possibilidades2 (i,j) (i-1,j-1) n (p-1)
				 | x-i==0 && y-j==1 = possibilidades2 (i,j) (i+1,j+1) n (p-1) + possibilidades2 (i,j) (i+1,j) n (p-1) + possibilidades2 (i,j) (i+1,j-1) n (p-1) + possibilidades2 (i,j) (i,j-1) n (p-1) + possibilidades2 (i,j) (i-1,j+1) n (p-1) + possibilidades2 (i,j) (i-1,j) n (p-1) + possibilidades2 (i,j) (i-1,j-1) n (p-1)
				 | x-i==1 && y-j==(-1) = possibilidades2 (i,j) (i+1,j+1) n (p-1) + possibilidades2 (i,j) (i+1,j) n (p-1) + possibilidades2 (i,j) (i,j+1) n (p-1) + possibilidades2 (i,j) (i,j-1) n (p-1) + possibilidades2 (i,j) (i-1,j+1) n (p-1) + possibilidades2 (i,j) (i-1,j) n (p-1) + possibilidades2 (i,j) (i-1,j-1) n (p-1)
				 | x-i==1 && y-j==0 = possibilidades2 (i,j) (i+1,j+1) n (p-1) + possibilidades2 (i,j) (i+1,j-1) n (p-1) + possibilidades2 (i,j) (i,j+1) n (p-1) + possibilidades2 (i,j) (i,j-1) n (p-1) + possibilidades2 (i,j) (i-1,j+1) n (p-1) + possibilidades2 (i,j) (i-1,j) n (p-1) + possibilidades2 (i,j) (i-1,j-1) n (p-1)
				 | x-i==1 && y-j==1 = possibilidades2 (i,j) (i+1,j) n (p-1) + possibilidades2 (i,j) (i+1,j-1) n (p-1) + possibilidades2 (i,j) (i,j+1) n (p-1) + possibilidades2 (i,j) (i,j-1) n (p-1) + possibilidades2 (i,j) (i-1,j+1) n (p-1) + possibilidades2 (i,j) (i-1,j) n (p-1) + possibilidades2 (i,j) (i-1,j-1) n (p-1)

			    
possibilidades2 (x,y) (i,j) n p = if p==0 || caminhos n (i,j) == 0
			    then 0
			    else if p==1
			    	 then caminhos n (i,j)
			    	 else proximoscaminhos (x,y) (i,j) n p

possibilidades n p = if n<=1 || p==0
		     then 0
		     else if p==1
		     	  then n*n + sum[possibilidades2 (i,j) (i,j) n p|i<-[1..n], j<-[1..n]]
		     	  else sum[possibilidades2 (i,j) (i,j) n p|i<-[1..n], j<-[1..n]]
