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

% one argument tuple_p 

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

% In start2 we define various type check functions
%   L_1 accepts a list
%   L_2 accepts a list of integers


start2() ->
  H = list(),
  H([3,4], true_2()),      
  H([3,4], integer_p()),

    %  list_P/0

    S_1  = list_p(),            %  S_1  is Structure type
    L_1  = integer_p(),         %  L_1  is Leaf type
    T_1  = S_1(L_1),            %  T_1  is Type w/ structure and leaf

    D_1  = [3,4],               %  D_1  is sone data 
    TC_1 = T_1(D_1),            %  TC_1 is type check data 

    % ----------

    S_2  = list_p(),            %    is Structure type
    L_2  = test:true_2(),       %    is Leaf type
    T_2  = S_2(L_2),            %    is Type w/ structure and leaf

    D_2  = [3,4],               %    is sone data 
    TC_2 = T_2(D_2),            %    is type check of data 

    % ----------
    S_3  = tuple_p(),           %    is Structure type
    L_3  = test:true_2(),       %    is Leaf type
    T_3  = S_3(L_3),            %    is Type w/ structure and leaf

    D_3  = {3,4},               %    is sone data 
    TC_3 = T_3(D_3),            %    is type check of data 

    % ----------

    Tuple_of     = tuple_p(),             % tuple_p/0 
    Any          = test:true_2(),         % 
    List_of      = list_p_1(),            % list_p_1/1
            
    Type_4       = List_of( Tuple_of( Any )),        
    Data_4       = [{3,4},{4,5}],         % data 
    TC_4         = Type_4( Data_4),       % type check

    % ----------
 
    Any_1        = test:true_1(),               % true/1
            
    Type_5       = List_of( Any_1 ),           
    Data_5       = [a,7],                       % data 
    TC_5         = Type_5( Data_5 ),            % type check

    % ----------

    Chain_6 = [ List_of, List_of, List_of, Any_1 ],
    Chain_Type_6 = reduce( compose, Chain_6),

    Data  = [[[a,5],[b,4]]],
    TC_6  = Chain_Type_6( Data ), 

    Chain_type_7       = build_tree_of_depth( 6 ),
    Chain_build_type_7 = reduce( compose, Chain_type_7),

    D_7 = [[[[[[ok, cat]]]]]],
    TC_7   = Chain_build_type_7( D_7 ),

io:fwrite(" ~w is a dynamic tree of depth 6 type : ~w ~n ", [ D_7, TC_7]),

io:fwrite(" ~w is list of int: ~w ~n ",    [ D_1, TC_1]),  
io:fwrite(" ~w is list of any: ~w ~n ",    [ D_2, TC_2]),  
io:fwrite(" ~w is tuple of any: ~w ~n ",   [ D_3, TC_3]),
  
io:fwrite(" List_of( Tuple_of( Any )) ~n " ),  
io:fwrite(" ~w is list of tuple of any: ~w ~n ", [ Data_4, TC_4]),  

io:fwrite(" List_of(  Any_1 )) ~n " ),  
io:fwrite(" ~w is list of any: ~w ~n ", [ Data_5, TC_5]),  

  [
    " compose chain of types: list of list of list of any ", TC_6
  ]. 

% Build a structure dynamically
%  

  build_tree_of_depth( N ) -> 
      build_tree_of_depth( N, []).

  build_tree_of_depth( 0, Chain) -> 
      Any_1        = test:true_1(),               
      Chain ++ [ Any_1 ];

  build_tree_of_depth( N, Chain) -> 
      List_of = list_p_1(),
      build_tree_of_depth( N-1, [ List_of ] ++ Chain).

reduce( Func, [H|T] ) -> 
  reduce( Func, T, H).
  
reduce( _Func, [], Acc) -> Acc;
reduce( Func, [H|T], Acc) ->
  NAcc = apply( test, Func, [ Acc, H ]),
  reduce( Func, T, NAcc).

%  So we can have christmas tree, a tree with structure
%    undecorated christmas tree with 
%    no specified leaves. A shallow tree with required depth = 1
%
%    Any_1     = true_1().
%    Xmas_tree = list_of( Any_1 ). 
% 
%    leaves can be anything.

