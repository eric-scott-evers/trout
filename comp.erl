
-module(comp).
-compile(export_all).

% Experiment with type reflection at runtime in erlang, category diagrams, and contracts. 
%   Run with comp:start().
%     1) reflects input and output types of functions
%     2) generates simple category diagram for f, g and h 
%        {nat => nat}
%
%        Data.Maybe.mapMaybe :: (a -> Maybe b) -> [a] -> [b]
%        

start() -> 
        % chain of functions, f, g and h.

        Chain = [[a,f,b], [b,g,c], [c,h,d], [d,p,e]],

	Type_in    = lookup_type_in(Chain, ""),
	Type_out   = lookup_type_out(Chain, ""),
       
        % Type_guard 
	Guard_in    = [is_integer, { gt, 500}, { lt, 1500}],
        Guard_out   = [is_nat, { gt, 1065}, { lt, 1067}],
	Arg_in      = 1000,
        Arg_out     = 1066,
        Input_type_is_ok = execute_guard(  Guard_in,  Arg_in,  true),
        Output_type_is_ok = execute_guard( Guard_out, Arg_out, true),

	Char_1     = atom_to_list( hd( hd( Chain))),
	Output     = build_category(Chain, Char_1),
	
	% fist char gets erased each loop  
        io:fwrite(" ~s ~n ~s ~n ~n ", [Type_in, Type_out]),
	io:fwrite(" input type ~w of p is ok: ~w ~n ", 
           [Guard_in,   Input_type_is_ok] ),
        io:fwrite(" output type ~w of p is ok: ~w ~n ", 
           [Guard_out, Output_type_is_ok] ),
        io:fwrite(" ~n "),
	Output.

lookup_type_in([], Output) -> Output;

lookup_type_in( [Item|T], Output )->
        Info = " // input type of " ++ 
               atom_to_list( hd(tl(Item))) ++ " is ",
        Type_data    = apply( comp, hd(tl(Item)), [fin]),
        [Type,Extra] = fin_type_pattern(Type_data),
        NInfo        = Info ++ atom_to_list(Type) ++ " ",
        NNInfo       = NInfo ++ atom_to_list(Extra),
	New_Output   = Output ++ NNInfo,
        lookup_type_in(T, New_Output).
 

 fin_type_pattern( [Type, {Rel1,_},{_,_}] ) -> 
	[Type,Rel1];
 fin_type_pattern( Type ) ->
        [Type,none].

 lookup_type_out([], Output) -> Output;
 lookup_type_out([Item|T], Output) ->
        Info = " // output type of " ++
                atom_to_list( hd(tl(Item))) ++ " is " ++
                atom_to_list( apply(comp, hd(tl(Item)), [trout])),
        New_Output = Output ++ Info,
        lookup_type_out(T, New_Output).

build_category([], Output) -> Output;
build_category([Item|List], Output) ->
	A = hd(Item),
        B = hd(tl(Item)),
        C = hd(tl(tl(Item))),
        AA = atom_to_list(A),
        BB = atom_to_list(B),
        CC = atom_to_list(C),
        Arrow_start  = " --",
        Arrow_end    = "--> ",
	Build = AA ++ Arrow_start ++ BB ++ Arrow_end ++ CC,
	Trim_Output = lists:reverse(tl(lists:reverse(Output))),
	build_category(List, Trim_Output ++ Build).

% - ----------------------

 f(fin)   -> float;        % type in
 f(trout) -> int;          % type out
 f(Arg)   -> floor(Arg).   % function 

 g(fin)   -> int;
 g(trout) -> nat;
 g(Arg)   -> abs(Arg).

 h(fin)   -> nat;
 h(trout) -> nat;
 h(_Arg)  -> 1000.

%            [is_integer, {gt,1065   }, {lt,1067 }]; 
 p(fin)   -> [int, {gt, 500}, {lt, 1500}];
 p(trout) -> nat;
 p(_Arg)  -> 1066.

 q(fin)   -> [int, tand, [{gt, 10}, tor, {lt, 5}]];
 q(trout) -> real;	    
 q(Arg)   -> Arg*Arg. 			 
		  
		  
% ---------------------  

execute_guard([], _Arg, Output) -> Output;     % execute_guard/3

execute_guard([H|Chain], Arg, Output) ->  
  Out = execute_a_guard(H, Arg) and Output, 
  execute_guard(Chain, Arg, Out). 

% ---------------------  

% special cases: greater than, etc

execute_a_guard({gt, Num}, Arg) -> Arg > Num;
execute_a_guard({lt, Num}, Arg) -> Arg < Num;
execute_a_guard(is_nat, Arg)    -> is_integer(Arg) and (Arg > -1);

% default case, is_int, if_float    etc.  

execute_a_guard(Function, Arg) ->
    % simple native types in erlan
    Simple_types = [ is_atom,    is_binary,    is_bitstring,  
                     is_boolean, 
                     is_float,   is_function,       
                     is_integer, is_list,      is_map,    
                     is_number,  
                     is_pid,            
                     is_port,    is_reference, is_tuple ],
    
    Is_Simple = lists:member(Function, Simple_types),  
    io:fwrite(" ~n "),
    io:fwrite(" ~w is simple: ~w ~n ", [ Function,  Is_Simple] ),  
                    
    apply(erlang, Function, [ Arg ]).
  
% ---------------------  



% call it with:

% Result = execute_guard( Guard_in, Arg_in, true).

