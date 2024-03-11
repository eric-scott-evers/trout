-module(comp).
-compile(export_all).

% Experiment with type reflection at runtime in erlang, category diagrams, and contracts. 
%   Run with comp:start().
%     1) reflects input and output types of functions
%     2) generates simple category diagram for f, g and h 

start() -> 
        Chain = [[a,f,b], [b,g,c], [c,h,d]],

	Type_in    = lookup_type_in(Chain, ""),
	Type_out   = lookup_type_out(Chain, ""),
       
        % Type_guard 
	Guard_in    = [is_integer, { gt, 500}, { lt, 1500}],
        Guard_out   = [is_integer, { gt, 1065}, { lt, 1067}],
	Arg_in      = 1000,
        Arg_out     = 1066,
        Input_type_is_ok = execute_guard(  Guard_in,  Arg_in,  true),
        Output_type_is_ok = execute_guard( Guard_out, Arg_out, true),

	Char_1     = atom_to_list( hd( hd( Chain))),
	Output     = build_category(Chain, Char_1),
	
	% fist char gets erased each loop  
        io:fwrite(" ~s ~n ~s ~n ~n ", [Type_in, Type_out]),
	io:fwrite(" input type ~w of p is ok: ~w ~n ", [Guard_in,   Input_type_is_ok] ),
        io:fwrite(" output type ~w of p is ok: ~w ~n ", [Guard_out, Output_type_is_ok] ),
	Output.

lookup_type_in([], Output) -> Output;
lookup_type_in([Item|T], Output) ->
	Info = " // input type of " ++ 
		atom_to_list( hd(tl(Item))) ++ " is " ++ 
		atom_to_list( apply(comp, hd(tl(Item)), [fin])),
	New_Output = Output ++ Info, 
	lookup_type_in(T, New_Output).

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

f(fin)   -> float;
f(trout) -> int;
f(Arg)   -> floor(Arg).

g(fin)   -> int;
g(trout) -> nat;
g(Arg)   -> abs(Arg).

h(fin)   -> nat;
h(trout) -> bool;
h(_Arg)  -> false.

p(fin)   -> [is_integer, {gt, 500}, {lt, 1500}];
p(trout) -> [is_integer, {gt, 1065}, {lt, 1067}];
p(_Arg)   -> 1066.

% ---------------------  

execute_guard([], _Arg, Output) -> Output;                    % execute_guard/3

execute_guard([H|Chain], Arg, Output) ->  
  Out = execute_a_guard(H, Arg) and Output, 
  execute_guard(Chain, Arg, Out). 

% ---------------------  

% special cases: greater than, etc

execute_a_guard({gt, Num}, Arg) -> Arg > Num;
execute_a_guard({lt, Num}, Arg) -> Arg < Num;

% default case, is_int, if_float    etc.  

execute_a_guard(Function, Arg) ->
  apply(erlang, Function, [Arg]).
  
% ---------------------  

% Simple_types = [ is_atom,    is_binary,    is_bitstring,  is_boolean, is_float,  is_function       
%                  is_integer, is_list,      is_map,        is_number,  is_pid,            
%                  is_port,    is_reference, is_tuple ],

% call it with:

% Result = execute_guard( Guard_in, Arg_in, true).

