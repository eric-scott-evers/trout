-module(comp).
-compile(export_all).

% Experiment with type reflection at runtime in erlang, category diagrams, and contracts. 
%   Run with comp:start().
%     1) reflects input and output types of functions
%     2) generates simple category diagram for f, g and h 

start() -> 
        Chain = [[a,f,b], [b,g,c], [c,h,d]],

	Type_in   = lookup_type_in(Chain, ""),
	Type_out  = lookup_type_out(Chain, ""),

	Char_1    = atom_to_list( hd( hd( Chain))),
	Output    = build_category(Chain, Char_1),
	
	% fist char gets erased each loop
        io:fwrite(" ~s ~n ~s ~n ~n ", [Type_in, Type_out]),
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
h(_Arg)   -> false.
