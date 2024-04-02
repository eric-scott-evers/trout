-module(cat1).
% -export([ start/0, max/2, min/2, rule/2, t/0]).
-compile(export_all).

start() -> 
  io:fwrite("subsets and types ~n"),  
  io:fwrite(" ~s ~w ~n",[" f(dom)     ",            f(tin) ]),   
  io:fwrite(" ~s ~w ~n",[" f(cod)     ",           f(tout) ]),   
  io:fwrite(" ~s ~w ~n",[" max(dom)   " ,    cat1:max(dom) ]),   
  io:fwrite(" ~s ~w ~n",[" max(cod)   ",     cat1:max(cod) ]),   
  io:fwrite(" ~s ~w ~n",[" f(3.5)     ",            f(3.5) ]),
  Subsets = calc_sub_sets(),
  io:fwrite(
  " is nat a subset of real?  options are nat, int, real , complex~n"),
  io:fwrite(" subsets of subsets ~w ~n", [ calc_sub_sets() ]),
  io:fwrite(" if map subset A = ~w ", [calc_sub_sets()] ),
  io:fwrite(" then is_subset( first(A), last(A)).  "), 
  io:fwrite(" ~w is subset of ~w ", 
     [ hd( Subsets), lists:last( Subsets)]), 
  io:fwrite(" \n "). 

f(tin)   -> float;
f(tout)  -> int;
f(Float) -> round(Float).

max(dom) -> nat_aug;
max(cod) -> nat_aug.

max(true , _X) -> true;
max(false, X)  -> X.  

min(true, X)    -> X;
min(false, _X)  -> false;
min(X,  true)   -> X;
min(_X, false)  -> false.

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
