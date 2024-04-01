-module(cat1).
-export([ start/0, max/2, min/2, rule/2, t/0]).

start() -> 
  
  ["f(tin)", f(tin), "f(tout)", f(tout), "f(3.5)", f(3.5) ,
   calc_sub_sets(), " are a list of nested subsets"].  
 
f(tin) -> float;
f(tout) -> int;
f(Float) -> round(Float).

max(true , _X) -> true;
max(false, X) -> X.  

min(true, X)  -> X;
min(false, _X) -> false;
min(X, true)  -> X;
min(_X, false) -> false.

rule([X,eq,Y],[Y,eq,Z]) -> [X,eq,Z];
rule(max, [X,true]) -> X.
 
% unary and is def rule [max, [X,eq,X] ]
%  max / log[x,mul,y], xform, [log,x],sum,[log,y]]  

% unknown u or 0
% arctanh to convert to finite booleans 
% "x + 1" < inf.

% subset(nat, integer) -> true.

% hd ([[X,Y,W,Z] || X<-[nat], Y<-T, W<-T, Z<-[real], FF(X,Y), FF(Y,W), FF(W,Z)]).
% [nat,nat,integer,real]

calc_sub_sets() -> 
  Sub = 
    fun(A,B) -> 
      ((A==nat) and (B==integer)) or 
       (A==B) or 
       ((A==integer) and (B==real)) end,
  hd( 
    [[X,Y,W,Z] || X<-[nat], Y<-cat1:t(), W<-cat1:t(), 
      Z<-[real], Sub(X,Y), Sub(Y,W), Sub(W,Z)]).



 t() -> [nat, integer, real, complex]. 
