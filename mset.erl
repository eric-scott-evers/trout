
-module(mset).
-compile(export_all).

test() -> 
  A = [
       {a,2},
       {b,3},
       {c,-4},
       {e,5}
      ],
  B = [
       {a,3},
       {b,2},
       {d,-4},
       {e,-5}
      ],
  % DA = dict:from_list(A),
  % DB = dict:from_list(B),
  
  % ------------
  [Pa, Pb] = plump_both( A, B),
  Union     = union( A,  B), 
  Intersect = intersect( A, B), 
  Add_union = add_union( A, B),
  Dom_1     = dom( add_union, 2),
%  Dom_2     = dom( intersect, 2),
   Type_info_1 = type_info( mset, 0),
   Type_info_2 = type_info( pair, 0),
  % ----- Output 
  
  [ a,A,b,B,   plump, [Pa, Pb], 
    union,     Union,
    intersect, Intersect,
    add_union, Add_union,
    dom, add_union, Dom_1,
    type_info, mset, Type_info_1,
    type_info, pair, Type_info_2
%    dom, add_union, Dom_2 
].

% ---------- addative_union ----------

% domain of fuction, F, with arity, N

dom( F , N ) -> 
  % M = list_to_atom(F),
  apply(?MODULE, F, [dom, N]).

% type info of type, T, of arity N
type_info( mset, 0) -> [pair];
type_info( pair, 0) -> {any, int}.

-type pair() :: {any(), integer()}.
% x-type mset() :: [pair()].
-spec add_union( [pair()], [pair()] ) -> pair().

add_union( dom, 2 ) -> [ mset, mset ]; 
add_union( A, B ) -> 
  [Pa, Pb] = plump_both( A, B),
  m_add_union( Pa, Pb, []).

m_add_union( [], [], Build) -> Build;
m_add_union( List_A, List_B, Build) ->
  [{Key, Val_A} | Tail_A] = List_A, 
  Front_of_list           = move_to_front( List_B, Key),  
  [{Key, Val_B} | Tail_B] = Front_of_list,
  Best_add_union_pair     = add_union_choose_value( Val_A, Val_B, Key),
  m_add_union( Tail_A, Tail_B, Build ++ Best_add_union_pair ).

add_union_choose_value( Val_A, Val_B, _Key ) 
  when Val_A+Val_B == 0 ->  
  []; 
add_union_choose_value( Val_A, Val_B, Key ) ->  
  [{Key, Val_A + Val_B}].   
  
% ---------- multi set intersection  

% intersect( dom, 2) -> [ mset, mset ];
intersect( A, B ) -> 
  [Pa, Pb] = plump_both( A, B),
  m_intersect( Pa, Pb, []).

m_intersect( [], [], Build) -> Build;
m_intersect( List_A, List_B, Build) ->
  [{Key, Val_A} | Tail_A] = List_A, 
  Front_of_list           = move_to_front( List_B, Key),  
  [{Key, Val_B} | Tail_B] = Front_of_list,
  Best_pair             = intersect_choose_value( Val_A, Val_B, Key),
  m_intersect( Tail_A, Tail_B, Build ++ Best_pair ).
   
intersect_choose_value( 0, _Val, _Key ) ->
  [];
intersect_choose_value( _Val, 0, _Key ) -> 
  [];
intersect_choose_value( Val_A, Val_B, Key ) ->
  Min_val = min( Val_A, Val_B ),
  [{Key, Min_val}]. 

% ---------- multi set union 

union( A, B ) -> 
  [Pa, Pb] = plump_both( A, B),
  m_union( Pa, Pb, []).

m_union( [], [], Build) -> Build;
m_union( List_A, List_B, Build) ->
  [{Key, Val_A} | Tail_A] = List_A, 
  Front_of_list           = move_to_front( List_B, Key),  
  [{Key, Val_B} | Tail_B] = Front_of_list,
  Best_val                = union_choose_value( Val_A, Val_B),
  m_union( Tail_A, Tail_B, Build ++ [{Key, Best_val}] ).
   
union_choose_value( 0, Val ) ->
  Val;
union_choose_value( Val, 0 ) -> 
  Val;
union_choose_value( Val_A, Val_B ) ->
  max( Val_A, Val_B ).

% ----- plump_both:  add extra keys both ways  

plump_both( A ,B) ->
  DA       = dict:from_list( A ),
  DB       = dict:from_list( B ),
  A_keys   = dict:fetch_keys( DA ),
  B_keys   = dict:fetch_keys( DB ),
  All_keys = lists:umerge( A_keys, B_keys),
  Plump_A  = plump( All_keys, A),
  Plump_B  = plump( All_keys, B),
  [Plump_A, Plump_B].

plump( All_keys, List_of_pairs) ->
  plump( All_keys, List_of_pairs, []).

plump( [], _, Build) -> Build;
plump( [Key|Keys], List_of_pairs, Build) ->  
  D_list     = dict:from_list( List_of_pairs ),
  The_keys   = dict:fetch_keys( D_list ),  
  Is_member  = lists:member( Key, The_keys ),
  Pair       = plump_key_is_found( Is_member, Key, List_of_pairs), 
  NBuild     = [Pair] ++ Build,
  plump( Keys, List_of_pairs, NBuild).

plump_key_is_found( true, Key, List_of_pairs) -> 
  [H|_] = move_to_front( List_of_pairs, Key),
  H;
plump_key_is_found( false, Key, _List_of_pairs) ->
  {Key, 0}.

% ---------- move_to_front

move_to_front( List, Key) ->
  move_to_front( List, Key, []).

move_to_front( [], _, Out) -> 
  Out;
move_to_front( [{Key,Val}|Tail], Key, Out) ->
  Output = Tail ++ Out, 
  [{Key,Val}]   ++ Output; 
move_to_front( [H|Tail], AKey, Out) -> 
  move_to_front( Tail, AKey, [H]++Out).

