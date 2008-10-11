%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @copyright Copyright(C) 2003-2005 Thomas Lindgren <thomasl_erlang@yahoo.com>.
%% @license
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met: 
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer. 
%% 2. Redistributions in binary form must reproduce the above
%%    copyright notice, this list of conditions and the following
%%    disclaimer in the documentation and/or other materials provided
%%    with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
%% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
%%
%%			   SMART EXCEPTIONS
%%
%% Author: Thomas Lindgren (030414-; 051016; 081004)
%%
%% A simplified version of the earlier smart_exceptions, twice.
%%
%% USAGE
%%   erlc +'{parse_transform, smart_exceptions}' file.erl
%%
%% As given, the code generates a "smart exit", an exit with more info
%% than is usual. Uncomment the -define(exn_handler,...) to instead
%% invoke ?default_exn_handler_mod:* when there is an exit.
%%
%% PURPOSE:
%%
%% Rather than generating a terse exception 'badarg', this preprocessor
%% rewrites exceptions (apart from 'function undefined') to do one of:
%%  - invoke an exception handler, smart_exc_rt.erl (or user defined)
%%    * includes giving some BIF context
%%  - generate a 'big exit' with module, function, line, reason
%%
%% The generated code looks awful (lots of redundant code) but the beam
%% compiler gets rid of this.
%%
%% Third version of smart_exceptions: here, we get rid of the Handler
%% argument and the R9/R10 stuff (we always use try ... end) as well
%% as the use of the mapform module (which is proprietary).
%%
%% UNFINISHED
%% - some handlers missing or bad
%% - (P = E) must extract all variables X1..Xn in P and generate
%%      exit(EXIT), X1=nyi, ..., Xn = nyi
%%   to satisfy the compiler
%% - untested ...

-module(smart_exceptions).
-author('thomasl_erlang@yahoo.com').
-export([parse_transform/2]).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Current typical usage:
%%   erlc -pa $PATH +'{parse_transform, smart_exceptions}' $ERLFILE
%%
%% PATH must provide smart_exceptions.beam to enable the parse transform.
%%
%% If you want to see what the transform produces, provide the flag '-E'.

parse_transform(Forms, Opts) ->
    %% Opts = compiler options
    M = get_module_name(Forms),
    forms(M, Forms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file(File, Opts) ->
    Forms = parse:file(File, Opts),
    NewForms = parse_transform(Forms, Opts),
    parse:print(parse:reattribute(NewForms)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

forms(M, Forms0) when atom(M) ->
    Forms = simple_resolve_imports(Forms0),
    [ form(M, Form) || Form <- Forms ].

form(M, Form) ->
    {F, A} = function_name(Form),
    mapform0(
      fun({function, Lf, F1, A1, []}=Form0) ->
	      Form0;
	 ({function, Lf, F1, A1, Clss}) ->
	      smart_function(M, F1, A1, Lf, Clss);
	 ({match, Lm, P, E}=Expr) ->
	      smart_match(M, F, A, Lm, P, E);
	 ({'case',Lc,E,Clss}) ->
	      smart_case(M, F, A, Lc, E, Clss);
	 ({'if',Li,Clss}) ->
	      smart_if(M, F, A, Li, Clss);
	 ({'fun',Lf,{clauses,Clss}}) ->
	      smart_fun(M, F, A, Lf, Clss);
	 ({'fun',Lf,{clauses,Clss}, Info}) ->
	      smart_fun(M, F, A, Lf, Clss, Info);
	 ({op,Lo,Op,E1,E2}=E) ->
	      smart_binop(M, F, A, Lo, Op, E1, E2);
	 ({op,Lo,Op,E1}=E) ->
	      smart_unop(M, F, A, Lo, Op, E1);
	 ({call,Lc,{remote,Lr,{atom,Lm,erlang},{atom,Lf,exit}},[Rsn]}=E) ->
	      smart_exit_abs(M, F, A, Lc, Rsn);
	 ({call,Lc,{remote,Lr,{atom,Lm,erlang},{atom,Lf,fault}},[Rsn]}=E) ->
	      smart_fault(M, F, A, Lc, Rsn);
	 ({call,Lc,{remote,Lr,{atom,Lm,erlang},{atom,Lf,error}},[Rsn]}=E) ->
	      smart_error(M, F, A, Lc, Rsn);
	 ({call,Lc,{atom,Lf,exit},[Rsn]}=E) ->
	      smart_exit_abs(M, F, A, Lc, Rsn);
	 ({call,Lc,{remote,Lr,{atom,Lm,Mod},{atom,Lf,Fn}},As}=E) ->
	      case erlang:is_builtin(Mod, Fn, length(As)) of
		  true ->
		      smart_bif(M, F, A, Lc, 
				Mod, Fn, length(As), As);
		  false ->
		      E
	      end;
	 (E) ->
	      E
      end,
      Form
     ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Resolve local calls to primitive operations as follows:
%% 1. Collect all function names defined in module, including imports
%% 2. Walk each form, replacing local calls with remote ones
%%
%% As a side effect, we also resolve imported functions into remote
%% calls at this point.

simple_resolve_imports(Forms) ->
    Imps_and_funcs = func_defs(Forms),
    resolve_calls(Forms, Imps_and_funcs).

func_defs([{attribute, La, import, {M, FAs}}|Forms]) ->
    [ {{F,A}, {import, M}} || {F,A} <- FAs ] ++ func_defs(Forms);
func_defs([{function,Lf, F, A, Clss}|Forms]) ->
    [ {{F,A}, local} | func_defs(Forms) ];
func_defs([_|Forms]) ->
    func_defs(Forms);
func_defs([]) ->
    [].

resolve_calls(Forms, FuncDefs) ->
    [ resolve_form(Form, FuncDefs) || Form <- Forms ].

%% Resolve imports to remote calls. Note that we FIRST check whether
%% a local is a call to an erlang BIF. If so, the call is made to
%% erlang:f(...). This is deliberate, since Erlang itself behaves that way.
%% (Doing so also makes a mockery of scoping.)

resolve_form({function, _Lf, Fn, Ar, _Clss} = Form, FuncDefs) ->
    mapform0(
      fun({call, Lc, {atom, Lf, F}, As}=Expr) ->
	      FA = {F, A = length(As)},
	      case erlang:is_builtin(erlang,F,A) of
		  true ->
		      %% if ALSO defined locally, should warn
		      %% ?msg("Looking up ~p -> bif\n", [FA]),
		      Lm = Lc,
		      mk_remote_call(erlang, F, As);
		  false ->
		      case lists:keysearch(FA, 1, FuncDefs) of
			  {value, {_, local}} ->
			      %% ?msg("Looking up ~p -> local\n", [FA]),
			      Expr;
			  {value, {_, {import, M}}} ->
			      %% ?msg("Looking up ~p -> import\n", [FA]),
			      mk_remote_call(M, F, As);
			  false ->
			      %% ?msg("Looking up ~p -> undefined\n", [FA]),
			      Expr
		      end
	      end;
	 (Syntax) ->
	      Syntax
      end,
      Form
     );
resolve_form(Form, FuncDefs) ->
    Form.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% [R12B4]

%% Exprs = [Expr]
%% Cases = [Clause]
%% Exn_handlers = [Exn_clause]
%% After = [Expr]
%%
%% The Exn_handler clause looks like
%%   {clause,_,[{atom,_,Type},Pat,{var,_,'_'}], Guard, Body}
%% where Type = exit | error | throw

%% Here is a simplistic handler. The main problem is, Expr does
%% not export vars. I think. (Uhh, the docs sorta suggested it.)
%%
%% What we want to do is convert this macro into rewrite rules
%% that we can apply to all the relevant locations.

mk_try(Exprs, Cases, Exn_handlers, After) ->
    {'try', -1, Exprs, Cases, Exn_handlers, After}.

%% Type of exception is the atom Type.
%% The exception reason is caught as Rsn.
%% Body is what we do with it (should involve Rsn).

exn_handler(Type, Rsn, Body) ->
    {clause, -1, 
     [{tuple, -1, [{atom, -1, Type}, Rsn, {var, -1, '_'}]}],
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
       [smart_exit_abs(M, F, A, Line, AbsMatch)] ++
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
     {clauses, 
      Clss ++ [{clause, -1, Xs, [], [mk_exit(Term)]}]}}.

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
     {clauses,
      Clss ++ [{clause, -1, Xs, [], [mk_exit(Term)]}]}, 
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
    Rsn = new_var(),
    Bif = {tuple, -1,
	   [{tuple, -1, [{atom, -1, bif}, {atom, -1, F}, cons_list(Xs)]},
	    Rsn]},
    Exn_term = exn_term({M, F, A}, {line, Line}, Bif),
    {block, -1,
     Evals ++
     [mk_try([mk_remote_call(Mod, Func, Xs)],
	     [],
	     [exn_handler(exit, Rsn, [mk_exit(Exn_term)]),
	      exn_handler(error, Rsn, [mk_error(Exn_term)])],
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
    Rsn = new_var(),
    Exn_term = exn_term({M, F, A}, {line, Line}, Rsn),
    {block, -1,
     [{match, -1, X1, E1},
      {match, -1, X2, E2},
      mk_try([mk_binop(Op, X1, X2)], [],
	     [exn_handler(exit, Rsn, [mk_exit(Exn_term)]),
	      exn_handler(error, Rsn, [mk_error(Exn_term)])],
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

smart_unop(M, F, A, Line, Op, Expr) ->
    X = new_var(),
    Rsn = new_var(),
    Exn_term = exn_term({M, F, A}, {line, Line}, Rsn),
    {block, -1,
     [{match, -1, X, Expr},
      mk_try([mk_unop(Op, X)], [],
	     [exn_handler(exit, Rsn, [mk_exit(Exn_term)]),
	      exn_handler(error, Rsn, [mk_error(Exn_term)])],
	     [])
      ]}.

%% Rewrite to exit({{M,F,A},{line, L}, Rsn})
%% UNFINISHED
%% - is Rsn abstract or concrete??

smart_exit(M, F, A, Line, Rsn) ->
    Term = erl_parse:abstract({{M, F, A}, {line, Line}, Rsn}),
    mk_remote_call(erlang, exit, [Term]).

smart_exit_abs(M, F, A, Line, AbsRsn) ->
    T1 = erl_parse:abstract({M, F, A}),
    T2 = erl_parse:abstract({line, Line}),
    Term = {tuple, -1, [T1, T2, AbsRsn]},
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
