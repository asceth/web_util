-module(web_util).

-export([floor/1, ceiling/1]).
-export([marshal_dump/1, marshal_load/1]).
-export([contains/2]).
-export([correct_time/1]).
-export([join/2, join_tuple_list/3, quoted_join/2]).
-export([term_to_list/1, quoted_term_to_list/1]).
-export([uuid/0, uuid/1]).
-export([short_encrypt/1, short_encrypt/2, encrypt/2]).
-export([bin2hex/1]).
-export([build_binding/2, build_set_binding/3]).
-export([pagination/4]).


-import(lists, [nth/2, map/2, foldl/3]).
-import(erlang,[integer_to_list/2, list_to_integer/2]).


%% RFC 2616 separators (called tspecials in RFC 2068) + a single quote
-define(IS_SEPARATOR(C),
        (C < 32 orelse
         C =:= $\s orelse C =:= $\t orelse
         C =:= $( orelse C =:= $) orelse C =:= $< orelse C =:= $> orelse
         C =:= $@ orelse C =:= $, orelse C =:= $; orelse C =:= $: orelse
         C =:= $\\ orelse C =:= $\" orelse C =:= $/ orelse
         C =:= $[ orelse C =:= $] orelse C =:= $? orelse C =:= $= orelse
         C =:= ${ orelse C =:= $})).


floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

%% Dumps a term to a base64 encoded string using erlang term binary format
marshal_dump(Term) ->
  base64:encode_to_string(term_to_binary(Term)).

%% Loads a term from a base64 encoded string using erlang term binary format
marshal_load(Base64) ->
  binary_to_term(list_to_binary(base64:mime_decode_to_string(Base64))).

%% @spec contains(any(), list())
contains(Key, []) ->
  false;
contains(Key, List) ->
  lists:any(fun(X) -> Key == X end, List).

%% Corrects time
correct_time({{Year, Month, Day}, {Hour, Minutes, Seconds}}) when Seconds > 60 ->
  AddMin = Seconds div 60,
  LeftSeconds = Seconds rem 60,
  correct_time({{Year, Month, Day}, {Hour, Minutes + AddMin, LeftSeconds}});
correct_time({{Year, Month, Day}, {Hour, Minutes, Seconds}}) when Minutes > 60 ->
  AddHour = Minutes div 60,
  LeftMinutes = Minutes rem 60,
  correct_time({{Year, Month, Day}, {Hour + AddHour, LeftMinutes, Seconds}});
correct_time({{Year, Month, Day}, {Hour, Minutes, Seconds}}) when Hour > 24 ->
  AddDay = Hour div 24,
  LeftHour = Hour rem 24,
  correct_time({{Year, Month, Day + AddDay}, {LeftHour, Minutes, Seconds}});
correct_time({{Year, Month, Day}, {Hour, Minutes, Seconds}}) ->
  {{Year, Month, Day}, {Hour, Minutes, Seconds}}.


%% @spec join([string()], Separator) -> string()
%% @doc Join a list of terms together with the given separator
%%      string or char.
join([], _Separator) ->
    [];
join([S], _Separator) when is_binary(S) ->
  S;
join([S], _Separator) ->
    lists:flatten(S);
join(Strings, Separator) ->
    lists:flatten(lists:reverse(revjoin(Strings, Separator, []))).

revjoin([], _Separator, Acc) ->
    Acc;
revjoin([S | Rest], Separator, []) ->
    revjoin(Rest, Separator, [term_to_list(S)]);
revjoin([S | Rest], Separator, Acc) ->
    revjoin(Rest, Separator, [term_to_list(S), Separator | Acc]).

%% @spec join([string()], Separator) -> string()
%% @doc Join a list of terms together with the given separator
%%      string or char.
quoted_join([], _Separator) ->
    [];
quoted_join([S], _Separator) ->
    lists:flatten(S);
quoted_join(Strings, Separator) ->
    lists:flatten(quoted_revjoin(Strings, Separator, [])).

quoted_revjoin([], _Separator, Acc) ->
    Acc;
quoted_revjoin([S | Rest], Separator, []) ->
    quoted_revjoin(Rest, Separator, [quoted_term_to_list(S)]);
quoted_revjoin([S | Rest], Separator, Acc) ->
    quoted_revjoin(Rest, Separator, [quoted_term_to_list(S), Separator | Acc]).


%% Joins a list of tuples with a list separator and a tuple separator
join_tuple_list([], _ListSeparator, _TupleSeparator) ->
  [];
join_tuple_list([S], _ListSeparator, _TupleSeparator) ->
  lists:flatten(S);
join_tuple_list(List, ListSeparator, TupleSeparator) ->
  lists:flatten(join_tuple_list(List, ListSeparator, TupleSeparator, [])).

join_tuple_list([], _ListSeparator, _TupleSeparator, Acc) ->
  Acc;
join_tuple_list([{Key, Value}|Rest], ListSeparator, TupleSeparator, Acc) ->
  join_tuple_list(Rest, ListSeparator, TupleSeparator, [term_to_list(Key), TupleSeparator, quoted_term_to_list(Value), ListSeparator | Acc]).

term_to_list(Term) when is_list(Term) ->
  Term;
term_to_list(Term) when is_number(Term) ->
  integer_to_list(Term);
term_to_list(Term) when is_atom(Term) ->
  atom_to_list(Term);
term_to_list(Term) when is_binary(Term) ->
  binary_to_list(Term);
term_to_list(Term) when is_float(Term) ->
  float_to_list(Term).

quoted_term_to_list(Term) when is_list(Term) ->
  quote(Term);
quoted_term_to_list(Term) when is_number(Term) ->
  integer_to_list(Term);
quoted_term_to_list(Term) when is_atom(Term) ->
  quote(atom_to_list(Term));
quoted_term_to_list(Term) when is_binary(Term) ->
  quote(binary_to_list(Term));
quoted_term_to_list(Term) when is_float(Term) ->
  float_to_list(Term).

quote(String) when is_list(String) ->
    [39 | lists:reverse([39 | quote(String, [])])];	%% 39 is $'
quote(Bin) when is_binary(Bin) ->
    list_to_binary(quote(binary_to_list(Bin))).

quote([], Acc) ->
    Acc;
quote([0 | Rest], Acc) ->
    quote(Rest, [$0, $\\ | Acc]);
quote([10 | Rest], Acc) ->
    quote(Rest, [$n, $\\ | Acc]);
quote([13 | Rest], Acc) ->
    quote(Rest, [$r, $\\ | Acc]);
quote([$\\ | Rest], Acc) ->
    quote(Rest, [$\\ , $\\ | Acc]);
quote([39 | Rest], Acc) ->          %% 39 is $'
    quote(Rest, [39, $\\ | Acc]);   %% 39 is $'
quote([34 | Rest], Acc) ->          %% 34 is $"
    quote(Rest, [34, $\\ | Acc]);   %% 34 is $"
quote([26 | Rest], Acc) ->
    quote(Rest, [$Z, $\\ | Acc]);
quote([C | Rest], Acc) ->
    quote(Rest, [C | Acc]).


build_binding(From, To) ->
  lists:reverse(build_bind(lists:seq(From, To, 1), [])).

build_bind([Num|[]], Acc) ->
  [integer_to_list(Num), $$|Acc];
build_bind([Num|T], Acc) ->
  build_bind(T, [$\s, 44, integer_to_list(Num), $$|Acc]).

build_set_binding(From, To, Columns) ->
  lists:reverse(build_set_bind(lists:seq(From, To, 1), Columns, [])).

build_set_bind(_Nums, [], Acc) ->
  Acc;
build_set_bind([], _Columns, Acc) ->
  Acc;
build_set_bind([Num|[]], [Column|[]], Acc) ->
  [integer_to_list(Num), $$, " = ", term_to_list(Column)|Acc];
build_set_bind([Num|T], [Column|T1], Acc) ->
  build_set_bind(T, T1, [$\s, 44, integer_to_list(Num), $$, " = ", term_to_list(Column)|Acc]).

short_encrypt(Data) ->
  bin2hex(adv_crypto:sha(Data)).
short_encrypt(Salt, Data) ->
  bin2hex(adv_crypto:sha("--" ++ Salt ++ "--" ++ Data ++ "--")).
encrypt(Salt, Data) ->
  bin2hex(adv_crypto:sha512("--" ++ Salt ++ "--" ++ Data ++ "--")).

bin2hex(B) ->
  L = binary_to_list(B),
  LH0 = map(fun(X)->integer_to_list(X,16) end, L),
  LH = map(fun([X,Y])->[X,Y];([X])->[$0,X] end, LH0), % add zeros
  lists:flatten(LH).

uuid() ->
  generate_uuid(32).
uuid(Length) ->
  generate_uuid(Length).

%% Generates a string of Length characters
generate_uuid(Length) ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  lists:filter(fun(X) -> not ?IS_SEPARATOR(X) end, lists:flatten(lists:foldl(fun(_X,AccIn) ->
      [random:uniform(90) + 32|AccIn] end,
      [], lists:seq(1,Length)))).


pagination(TotalItems, PerPage, PreUrl, CurrentPage) ->
  windowed_paginator(web_util:ceiling(TotalItems / PerPage), PreUrl, CurrentPage).

windowed_paginator(TotalPages, PreUrl, CurrentPage) ->
  InnerWindow = 4,
    OuterWindow = 1,
    WindowFrom = CurrentPage - InnerWindow,
    WindowTo = CurrentPage + InnerWindow,

    if
      WindowTo > TotalPages ->
        WindowFrom1 = WindowFrom - (WindowTo - TotalPages),
        WindowTo1 = TotalPages;
      WindowFrom < 1 ->
        WindowTo1 = WindowTo + (1 - WindowFrom),
        WindowFrom1 = 1;
      true ->
        WindowTo1 = WindowTo,
        WindowFrom1 = WindowFrom
    end,
    LeftGap = 2 + OuterWindow,
    RightGap = WindowTo1 + 1,
    Visible = lists:seq(1, TotalPages),
    Visible1 = if
      (WindowFrom1 - LeftGap) > 1 ->
        {List1, List2} = lists:split(LeftGap - 1, Visible),
        List1 ++ [-1] ++ lists:subtract(List2, lists:seq(LeftGap, WindowFrom1));
      true ->
        Visible
    end,
    Visible2 = if
      ((TotalPages - 1) - RightGap) > 1 ->
        {List3, List4} = lists:split(RightGap - (WindowFrom1 - LeftGap + 1), Visible1),
        List3 ++ [-1] ++ lists:subtract(List4, lists:seq(RightGap, TotalPages - 2));
      true ->
        Visible1
    end,
  do_paginate(Visible2, PreUrl, CurrentPage, TotalPages).

do_paginate(Visible, PreUrl, 1, TotalPages) ->
  Previous = [[<<"<span class=\"disabled\">&laquo; Previous</span>  ">>]],
  do_paginate(Visible, PreUrl, 1, TotalPages, Previous);
do_paginate(Visible, PreUrl, CurrentPage, TotalPages) ->
  Previous = [[<<"<a href=\"">>, PreUrl, <<"?page=">>, integer_to_list(CurrentPage - 1), <<"\">&laquo; Previous</a>  ">>]],
  do_paginate(Visible, PreUrl, CurrentPage, TotalPages, Previous).

do_paginate([], _PreUrl, TotalPages, TotalPages, Acc) ->
  Next = [<<"  <span class=\"disabled\">Next &raquo;</span>">>],
  lists:reverse([Next|Acc]);
do_paginate([], PreUrl, CurrentPage, _TotalPages, Acc) ->
  Next = [<<"  <a href=\"">>, PreUrl, <<"?page=">>, integer_to_list(CurrentPage + 1), <<"\">Next &raquo;</a>">>],
  lists:reverse([Next|Acc]);
do_paginate([H|T], PreUrl, H, TotalPages, Acc) ->
  Link = [<<"<span class=\"current\">">>, integer_to_list(H), <<"</span>">>],
  do_paginate(T, PreUrl, H, TotalPages, [" ", Link|Acc]);
do_paginate([-1|T], PreUrl, CurrentPage, TotalPages, Acc) ->
  Link = [<<"...">>],
  do_paginate(T, PreUrl, CurrentPage, TotalPages, [" ", Link|Acc]);
do_paginate([H|T], PreUrl, CurrentPage, TotalPages, Acc) ->
  Link = [<<"<a href=\"">>, PreUrl, <<"?page=">>, integer_to_list(H), <<"\">">>, integer_to_list(H), <<"</a>">>],
  do_paginate(T, PreUrl, CurrentPage, TotalPages, [" ", Link|Acc]).
