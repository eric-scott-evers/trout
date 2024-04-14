-module(test).
-compile(export_all).

start() -> 
  Chain = [ffloor, llog],
  T1 = hd( Chain),
  T2 = hd( tl( Chain)),
  
  U = [1, 2, 3],
  V = [4, 5, 6],

  Output_Type = apply(?MODULE, T1, [ cod ]),
  io:fwrite("cod of ffloor: ~w ",  [ Output_Type ] ),

  Input_Type = apply(?MODULE, T2,  [ dom ]),
  io:fwrite("dom of llog: ~w ",  [ Input_Type ] ),
  io:fwrite("\n"),

  Is_Safe = set_tool:prove_is_subset(Output_Type, Input_Type),
  io:fwrite(" functions are compatible: ~w ",  [ Is_Safe ] ),
  io:fwrite("\n"),

  Arg = 5.5,
  Result = exec_chain( Arg, Chain),
  io:fwrite(" exec_chain, ~w on Arg, ~w ~n ", [Chain, Arg]),
  io:fwrite(" result is, ~w ", [Result]),
  io:fwrite("\n"),

  W = add( U, V),
  %
  io:fwrite(" add u and v, result is, ~w ", [ W ]),
  ok.

% type as a predicate
%   Type_Fun = [and reduce map is_integer over list].
%   type_predicate:
%   Type_p = fun(Arg) -> reduce(and, map(is_integer, Arg)) end.
%
%   four ways to describe a type
%   1) executable predicate function
%        is_real or subset_of_real 
%   2) chain of function that compose to create a type predicate 
%        Type_p_chain = [ map, "is_real", reduce, "and" ] 
%   3) strectual program representation
%        [ real ]
%   4) natural language of structure
%        array of reals  
%   
%      we could apply type analysis to the type_p_chain possibly 
%      strings are extra arguments

shape(dom) -> { type_chain, is_list, x, is_list };
shape(cod) -> { type_chain, is_list };
shape(L)   -> length(L). 

exec_chain(Arg, Chain) ->
  exec_chain(Arg, Chain, []).

exec_chain( Arg, [], []) -> Arg;
exec_chain( Arg, [F | Chain], []) -> 
  Out = apply(?MODULE, F, [Arg]),  
  exec_chain( Out, Chain, []).  

ffloor(dom) -> real;
ffloor(cod) -> integer;
ffloor(A)   -> floor(A).

llog(dom) -> real;
llog(cod) -> real;
llog(A)   -> math:log(A).

% input type is vec of real cross vec of real 
 
add( dom ) -> { set_notation, [real] , x , [real] };
add( cod ) -> [real].

% add two real vectors 
add( A, B) -> add( A, B, []).

add([], [], Output) -> Output;
add([H1|T1], [H2|T2], Output) ->  
  add( T1, T2, Output++[H1+H2]).

% composable structure types 

true_2() -> fun(_,_) -> true end.
true_1() -> fun(_) -> true end.

list() -> fun([A|_B], Leaf) -> 
            apply( Leaf, [A,true]) ;
            (_C, _Leaf) -> false end.

% dummy argument: 0

list_p() -> fun(TLeaf) -> 
              fun(A) -> 
                is_list(A) and apply(TLeaf, [hd(A), 0] )
              end
            end. 

list_p_1() -> fun(TLeaf) -> 
              fun(A) -> 
                is_list(A) and apply(TLeaf, [hd(A)] )
              end
            end. 

tuple_p() -> fun(TLeaf) -> 
               fun(A) -> 
                 is_tuple(A) and apply(TLeaf, [element(1,A), 0] )
               end
             end.

tuple_2() -> fun(A, TLeaf) -> 
                 is_tuple(A) and apply(TLeaf, [element(1,A), 0] )
              end.
 
integer_p() -> fun(A,_) -> 
            is_integer(A) end. 

-spec compose(fun((A) -> B), fun((B) -> C)) -> fun((A) -> C).

compose(F, G) -> fun(X) -> G(F(X)) end. 

start2() ->
  H = list(),
  H([3,4], true_2()),      
  H([3,4], integer_p()),
                                %  list_P/0
    L_1  = list_p(),            %  L_1/1 Arg = TLeaf or structure
    L_2  = L_1(integer_p()),    %  L_2/1 Arg = Structure or TLeaf
    L_3  = L_2([3,4]),
                                %
    L_10 = list_p(),
    L_11 = L_10(test:true_2()),
    L_12 = L_11([a,b]),

    L_20 = tuple_p(),
    L_21 = L_20(test:true_2()),
    L_22 = L_21({a,b}),

    T_30  = tuple_p(),             % tuple_p/0 
    T_31  = T_30(test:true_2()),   % T_30/1 Arg=TLeaf/2
                                   % T_32/1 Arg={a,b}
    L_40  = list_p_1(),            
    L_41  = L_40( T_31 ),        
    L_42  = L_41([{3,4},{4,5}]),

    L_50  = list_p_1(),                       
    L_51  = L_50( test:true_1()),
    L_52  = L_51( [a,7]),

    L_60  = list_p_1(),
    L_61  = L_60( test:true_1()),

    L_64  = test:true_1(),
    L_65  = compose( L_60, L_60),
    L_66  = compose( L_65, L_60),
    L_67  = compose( L_66, L_64),
    Data  = [[[a,5],[b,4]]],
    L_68  = L_67( Data ), 
    
    L_70  = list_p_1(),
    L_71  = L_70( L_61),
    L_72  = L_71( [[5,7],[8,4]] ),
  [
    " list of int ", L_3, 
    " list of any ", L_12,
    " tuple of any ", L_22,
    " ~n list of tuple of any ", L_42,
    " ~n simpler list of any ", L_52,
    " compose list of list of list of any ", L_68,
    " where data is ", Data, 
    " ~n list of list of any ", L_72
  ]. 

% H = fun(A, Leaf) -> 
%         apply( List(A, Sequence, Leaf );
%         (C, Leaf) -> false end.

% H1 = fun([A|B], Leaf) -> apply(Leaf,[A]) ;
% 8>       (C, Leaf) -> false end .
% #Fun<erl_eval.43.65746770>
% 9> TLeaf = fun(L) -> is_integer(L) end .
% #Fun<erl_eval.44.65746770>
% 10> H1([4,7], TLeaf).
% true
% 11> H1([a,7], TLeaf).
% false
% 12> H1([a,7], is_atom).
% ** exception error: bad function is_atom
% 13> H1([a,7], :is_atom).
% * 1:11: syntax error before: ':'
% 13> H1([a,7], {is_atom}).
% ** exception error: bad function {is_atom}
% 14> H1([a,7], TLeaf2=fun(A)-> is_atom(A) end  ).
% true
% 15> H1([5,7], TLeaf2=fun(A)-> is_atom(A) end  ).
% false

