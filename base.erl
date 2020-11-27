-module(base).
-export([print/1,println/1,getln/0,getln/1,show/1]).

% Prints an arbitrary data structure
% The only restriction is, that lists of integers cannot always be printed.
print("") -> "";
print(Data) -> case is_list(Data) and all_char(Data) of
                  true -> io:fwrite("~s",[Data]);
                  false -> print(show(Data))
               end.

% As print, but with an additional line break.
println(Str) -> print(Str),io:nl().

% getLn() reads line from keyboard.
getln() -> getln("").

% getLn(Prompt) reads line from keyboard and prints Prompt in front.
getln(P) -> L = io:get_line(list_to_atom(P)),
	    {Res,_} = lists:split(length(L)-1,L),
	    Res.

show(X) when is_atom(X) -> atom_to_list(X);
show(X) when is_integer(X) -> integer_to_list(X);
show(X) when is_pid(X) -> pid_to_list(X);
show(X) when is_tuple(X) -> "{"++showList(tuple_to_list(X))++"}";
show(X) when is_list(X) -> case all_char(X) of
                             true -> "\""++X++"\"";
                             false -> "["++showList(X)++"]"
                           end;
show(X) when is_function(X) -> "FUN".

all_char([]) -> true;
all_char([X|Xs]) when is_integer(X), 0 < X, X < 256 -> all_char(Xs);
all_char(_) -> false.

showList([]) -> "";
showList([X|Xs]) -> show(X)++case Xs of
                               [] -> "";
                               _ -> ","++showList(Xs)
                             end.


