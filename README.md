# trout
An experiment with category theory, types, contracts and erlang.

Let f be a function.
We let function tell us what type it accecpts.

f(fin) -> float;

And what type it returns.

f(trout) -> int;

Then we define the function.

f(Arg) -> floor(Arg).

we can ask it its inputs and output type.

f(fin).

>  float

output type
  
>  int

We do the same with function g.

g(fin) -> int;

Now if we try to compose f and g.

Arg = -4.5.
g(f(Arg)).

We can check the input and output type at runtime. 

f(trout) == f(fin).
>  true

Nice. 

If is_float(Arg) == true.
We know the composition should work before we run it. 
If is_float(Arg) == true.
And we can generate some simple category diagrams for our functions. 

    a -----> b -----> c -----> d
        f        g        h

----------------------------------------------------

Idea, we could make the type returned more detailed. 

y(fin) -> 
  {float, {inf, 500}, {sup, 1500}};

So the input domain is an float strictly between the values of 500 and 1500, exclusive. The dark ages. 

x(fin) ->
  {float, {max, 500}, {min, 1500}};
  
So the input domain is an float between the values of 500 and 1500 inclusive. Still the dark ages. 

Or return a function that acts as the guard to the function. 

Guard_in = [is_float, {gt, 500}, {lt, 1500}]. 

Then the input type is built from a composition of smaller building block functions in order. 

Type_in = Guard_in. 

We can execute the Guard in the following way.

Guard(Guard_in) -> 
  compose_funs(Guard_in).

Live_Guard_in = Guard(Guard_in).
if Live_Guard_in(Arg) -> do_stuff. 

So we return the list of building blocks as a complex type. 

w(fin) -> Guard_in =  [is_float, {gt, 500}, {lt, 1500}]. 

Of course we can have a Guard_out also. 

The Guard_in and Guard_out together become a contract for the function.

If the Guard_in for g matches the Guard_out for g, then they compose and communte.








