-module(smart2).
-author('thomasl_erlang@yahoo.com').
-export([parse_transform/2]).
-compile(export_all).


smart_expr(M, F, A, Line, Rsn, Expr) ->
    Exn_term = erl_parse:abstract({{M,F,A}, {line, Line}, Rsn}),
    mk_try([Expr], [],
	   [exn_handler(exit, 'Rsn', [mk_exit(Exn_term)]),
	    exn_handler(error, 'Rsn', [mk_error(Exn_term)])],
	   []).

mk_try(Exprs, Cases, Exn_handlers, After) ->
    {'try', -1, Exprs, Cases, Exn_handlers, After}.

exn_handler(Type, Var, Body) ->
    {clause, -1, 
     [{tuple, -1, [{atom, -1, Type}, {var, -1, Var}, {var, -1, '_'}]}],
     [],
     Body}.

mk_exit(Term) ->
    mk_nonlocal(exit, Term).

mk_error(Term) ->
    mk_nonlocal(error, Term).

mk_nonlocal(Ty, Term) ->
    mk_remote_call(erlang, Ty, [Term]).

%% UNFINISHED
%% - becomes (case Expr of Pat -> ok ; Val -> EXIT)
%%   where Val must export all the variables in Pat ...
%%     use free_vars(Pat, []) + generate matches

smart_match(M, F, A, Line, Pat, Expr) ->
    X = new_var(),
    FVs = vars_of(Pat),
    AbsMatch = {tuple, -1, [{atom, -1, match}, X]},
    {'case', -1, Expr,
     [{clause, -1, [{match,-1,Pat,X}], [], [X]},
      {clause, -1, [X], [], 
       [smart_exit(M, F, A, Line, AbsMatch)] ++
       [ {match, -1, Y, {atom, -1, nyi}} || Y <- FVs ]}]}.

%% M, F, A, Line, Rsn are concrete; Expr and Clss are abstract
%% Note: the Expr is already rewritten
%%
%% Add extra clause
%%   X -> exit({{M,F,A},{line, L}, {case_clause, X}})

smart_case(M, F, A, Line, Expr, Clss) ->
    X = new_var(),
    Case_term = {tuple, -1, [{atom,-1,case_clause}, X]},
    Term = exn_term({M, F, A}, {line, Line}, Case_term),
    {'case', -1, Expr,
     Clss ++
     [{clause, -1, [X], [], [mk_exit(Term)]}]}.

%% M, F, A, Line, Rsn are concrete; Expr and Clss are abstract
%% Note: the Expr is already rewritten
%%
%% Add extra clause: 
%%    true -> exit({{M,F,A},{line,Line},if_clause})

smart_if(M, F, A, Line, Clss) ->
    Term = erl_parse:abstract({{M,F,A}, {line, Line}, if_clause}),
    {'if', -1,
     Clss ++
     [{clause, -1, [], [],
       [mk_exit(Term)]}]}.

%% fun(P1,...,Pk) -> B end
%% add extra clause:
%%    (X1,...,Xn) -> exit({{M,F,A},{line,L},{fun_clause,X1,...,Xn}})

smart_fun(M, F, A, Line, Clss) ->
    Arity = clauses_arity(Clss),
    Xs = new_vars(Arity),
    Fun_args = {tuple, -1, [{atom, -1, fun_clause}] ++ Xs},
    Term = exn_term({M, F, A}, {line, Line}, Fun_args),
    {'fun', -1,
     Clss ++
     [{clause, -1, Xs, [],
       [mk_exit(Term)]}]}.

%% Same as above, but preserves Info field too
%%
%% UNFINISHED
%% - is the Info field properly preserved? e.g., we're adding variables

smart_fun(M, F, A, Line, Clss, Info) ->
    Arity = clauses_arity(Clss),
    Xs = new_vars(Arity),
    Fun_args = {tuple, -1, [{atom, -1, fun_clause}, cons_list(Xs)]},
    Term = exn_term({M, F, A}, {line, Line}, Fun_args),
    {'fun', -1,
     Clss ++
     [{clause, -1, Xs, [],
       [mk_exit(Term)]}], 
     Info}.

%% F(P1,...,Pk) -> B end
%% add extra clause:
%%    (X1,...,Xn) -> exit({{M,F,A},{line,L},{fun_clause,X1,...,Xn}})

smart_function(M, F, A, Line, Clss) ->
    Arity = clauses_arity(Clss),
    Xs = new_vars(Arity),
    Fun_args = {tuple, -1, [{atom, -1, fun_clause}, cons_list(Xs)]},
    Term = exn_term({M, F, A}, {line, Line}, Fun_args),
    {function, Line, F, A,
     Clss ++
     [{clause, -1, Xs, [],
       [mk_exit(Term)]}]}.

%% M, F, A, Line, Rsn are concrete; F and [A1,..,An] are abstract
%% A call F(E1,..,En) generates {{M,F,A},{line,L},[E1,..,En],Rsn}
%%
%% Rewrite to
%%   X1 = E1, ..., Xn = En,
%%   try F(X1,...,Xn)
%%   catch exit:Rsn -> exit({{M,F,A},{line,L},{bif, F, Xs}})
%%         exit:Rsn -> error({{M,F,A},{line,L},{bif, F, Xs}})
%%   end
%%
%% UNFINISHED - 
%% - 'Rsn' seems amateurish
%% - Exn_term contains the abstract Xs

smart_bif(M, F, A, Line, Mod, Func, Arity, Args) ->
    Xs = new_vars(Arity),
    Evals = [ {match, -1, X, Arg} || {X, Arg} <- lists:zip(Xs, Args) ],
    Bif = {tuple, -1, [{atom, -1, bif}, {atom, -1, F}, cons_list(Xs)]},
    Exn_term = exn_term({M, F, A}, {line, Line}, Bif),
    {block, -1,
     Evals ++
     [mk_try([mk_remote_call(Mod, Func, Xs)],
	     [],
	     [exn_handler(exit, 'Rsn', [mk_exit(Exn_term)]),
	      exn_handler(error, 'Rsn', [mk_error(Exn_term)])],
	     [])]}.

mk_remote_call(M, F, Xs) ->
    {call, -1, {remote, -1, {atom, -1, M}, {atom, -1, F}}, Xs}.

%% Rewrite to
%%   X1 = E1, X2 = E2,
%%   try X1 Binop X2 
%%   catch exit:Rsn -> exit({{M,F,A},{line,L},{Binop, X1, X2}})
%%         exit:Rsn -> error({{M,F,A},{line,L},{Binop, X1, X2}})
%%   end
%%
%% UNFINISHED
%% - 'Rsn' seems amateurish
%% - Exn_term seems weird, where are X1,X2 ...

smart_binop(M, F, A, Line, Op, E1, E2) ->
    X1 = new_var(),
    X2 = new_var(),
    Exn_term = exn_term({M, F, A}, {line, Line}, {var, -1, 'Rsn'}),
    {block, -1,
     [{match, -1, X1, E1},
      {match, -1, X2, E2},
      mk_try([mk_binop(Op, X1, X2)], [],
	     [exn_handler(exit, 'Rsn', [mk_exit(Exn_term)]),
	      exn_handler(error, 'Rsn', [mk_error(Exn_term)])],
	     [])
      ]}.

%% UNFINISHED

mk_binop(Op, X1, X2) ->
    {op, -1, Op, X1, X2}.

%% UNFINISHED

mk_unop(Op, X1) ->
    {op, -1, Op, X1}.

%% Rewrite to
%%   X1 = E1, 
%%   try Unop(X1)
%%   catch exit:Rsn -> exit({{M,F,A},{line,L},{Unop, X1}})
%%         exit:Rsn -> error({{M,F,A},{line,L},{Unop, X1}})
%%   end
%%
%% UNFINISHED
%% - 'Rsn' seems amateurish

smart_unop(M, F, A, Line, Op, Expr) ->
    X = new_var(),
    Exn_term = exn_term({M, F, A}, {line, Line}, {var,-1,'Rsn'}),
    {block, -1,
     [{match, -1, X, Expr},
      mk_try([mk_unop(Op, X)], [],
	     [exn_handler(exit, 'Rsn', [mk_exit(Exn_term)]),
	      exn_handler(error, 'Rsn', [mk_error(Exn_term)])],
	     [])
      ]}.

%% Rewrite to exit({{M,F,A},{line, L}, Rsn})
%% UNFINISHED
%% - is Rsn abstract or concrete??

smart_exit(M, F, A, Line, Rsn) ->
    Term = erl_parse:abstract({{M, F, A}, {line, Line}, Rsn}),
    mk_remote_call(erlang, exit, [Term]).

%% Rewrite to exit({{M,F,A},{line, L}, Rsn})
%% UNFINISHED

smart_fault(M, F, A, Line, Rsn) ->
    Term = erl_parse:abstract({{M, F, A}, {line, Line}, Rsn}),
    mk_remote_call(erlang, fault, [Term]).

%% Rewrite to error({{M,F,A},{line, L}, Rsn})
%% UNFINISHED

smart_error(M, F, A, Line, Rsn) ->
    Term = erl_parse:abstract({{M, F, A}, {line, Line}, Rsn}),
    mk_remote_call(erlang, error, [Term]).

exn_term(T1, T2, AbsT) ->
    Abs_T1 = erl_parse:abstract(T1),
    Abs_T2 = erl_parse:abstract(T2),
    {tuple, -1, [Abs_T1, Abs_T2, AbsT]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clauses_arity([{clause, _, H, G, B}|_]) ->
    length(H).

%%

new_vars(N) ->
    new_vars(1,N).

new_vars(M,N) ->
    if
	M > N ->
	    [];
	true ->
	    [new_var()|new_vars(M+1,N)]
    end.

%%

new_var() ->
    K = counter('exc var counter'),
    {var,0,list_to_atom("_" ++ integer_to_list(K))}.

%% from ap_util.erl

counter(Name) ->
    Ix =
	case get(Name) of
	    N when integer(N) ->
		N;
	    undefined ->
		0
	end,
    put(Name,Ix+1),
    Ix.

%%

get_module_name([{attribute,Lm,module,M}|Xs]) ->
    M;
get_module_name([_|Xs]) ->
    get_module_name(Xs).

%%

function_name({function, Lf, F, A, Clss}) ->
    {F, A};
function_name(Other) ->
    {not_a_function, no_name}.

%%

zip([X|Xs], [Y|Ys]) ->
    [{X,Y} | zip(Xs, Ys)];
zip([], []) ->
    [].

%%

cons_list([X|Xs]) ->
    {cons, 0, X, cons_list(Xs)};
cons_list([]) ->
    {nil, 0}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Simple version of mapform.erl
%%
%% NOTES
%% - constant/1 has apparently been deprecated by some fool

mapform0(F, {clause, Lc, H, G, B}) ->
    F({clause, Lc, H, G, mapform0(F, B)});
mapform0(F, {match, Lc, P, E}) ->
    F({match, Lc, P, mapform0(F, E)});
mapform0(F, {lc, Llc, E, GQs}) ->
    F({lc, Llc, mapform0(F, E), [ mapform1(F, GQ) || GQ <- GQs ]});
mapform0(F, T) when tuple(T) ->
    F(list_to_tuple([ mapform0(F, Tsub) || Tsub <- tuple_to_list(T) ]));
mapform0(F, Xs) when list(Xs) ->
    [ mapform0(F, X) || X <- Xs ];
mapform0(F, C) when atom(C) ; number(C) ->
    C.

%% detect + elide pattern in qualifier

mapform1(F, {generate, Lg, P, E}) ->
    {generate, Lg, P, mapform0(F, E)};
mapform1(F, Qual) ->
    mapform0(F, Qual).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Compute the variables occurring in the term.
%%  Variables are abstract vars, {var, _, X} where X is not '_'
%%  while Term can be any term (possibly containing some variables
%%  per above), but is probably an abstract syntax tree of some sort.
%%
%% Note: No handling of scoping, etc.
%%
%% This is just a heuristic to make the erlang compiler shut up. (Perhaps
%% not needed anymore?)

vars_of(AbsTerm) ->
    FVs0 = vars_of(AbsTerm, []),
    FVs = sets:to_list(sets:from_list(FVs0)),
    FVs.

vars_of({var, _, '_'}, Vs) ->
    Vs;
vars_of({var, _, X}=V, Vs) ->
    [V|Vs];
vars_of(T, Vs0) when tuple(T) ->
    lists:foldl(
      fun(Term, Vs) ->
	      vars_of(Term, Vs)
      end,
      Vs0,
      tuple_to_list(T));
vars_of([X|Xs], Vs) ->
    vars_of(Xs, vars_of(X, Vs));
vars_of([], Vs) ->
    Vs;
vars_of(X, Vs) ->
    %% cannot be a variable or contain a variable
    Vs.
