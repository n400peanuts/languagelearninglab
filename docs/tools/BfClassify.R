BfClassify<-function(B)
{
  if(B<=(1/3)){ "h0"}
  else if (B>=3) {"h1"}  
  else {"ambig"}
}