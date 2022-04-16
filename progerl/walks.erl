-module(walks).
-export([plan_route/2]).

-spec plan_route(From::point(), To::point()) -> route().
-type direction() :: north | south | east | west.
-type point()     :: {integer(), integer()}.
-type route()     :: [{go, direction(), integer()}].

-type angle()     :: {Degress::0..360, Minutes::0..60, Seconds::0..60}.
-type position()  :: {latitude | longitude, angle()}.
-spec plan_route1(From::position(), To::position()) -> route().

T1 :: A | B | C ...

Type :: any() | none() | pid() | port() | reference() | []
      | Atom | binary() | float() | Fun | Integer | [Type]
      | Tuple | Union | UserDefined

Union :: Type1 | Type2 | ...

Atom :: atom() | Erlang_Atom

Integer :: integer() | Min .. Max

Fun :: fun() | fun((...) -> Type)

Tuple :: tuple() | {T1, T2, ..., Tn}

-type NewTypeName(TVar1, TVar2, ..., TVarN) :: Type.

-type onOff()        :: on | off.
-type person()       :: {person, name(), age()}.
-type people()       :: [person()].
-type name()         :: {firstname, string()}.
-type age()          :: integer().
-type dict(Key,Val)  :: [{Key,Val}].


%% predefined type aliases
-type term()   :: any().
-type boolean()   :: true | false.
-type byte()      :: 0..255.
-type char()      :: 0..16#10ffff.
-type number()    :: integer() | float().
-type list()      :: [any()].
-type maybe_improper_list()   :: maybe_improper_list(any(), any()).
-type maybe_impropert_list(T) :: maybe_improper_list(T, any()).
-type string()    :: [char()].
-type nonempty_string() :: [char(), ...].
-type iolist()    :: maybe_improper_list(byte() | binary() | iolist(), binary() | []).
-type module()    :: atom().
-type mfa()       :: {atom(), atom(), atom()}.
-type node()      :: atom().
-type timeout()   :: infinity | non_neg_integer().
-type no_return() :: none().

% maybe_improper_list(), list whose ultimate tail is non-nil
% non_neg_integer(), pos_integer(), neg_integer()
% [X,...] non-empty list of type X


-spec file:open(FileName, Modes) -> {ok, Handle} | {error, Why} when
   FileName :: string(),
   Modes    :: [Mode],
   Mode     :: read | write | ...
   Handle   :: file_handle(),
   Why      :: error_term().

-spec lists:map(fun((A) -> B), [A]) -> [B].
-spec lists:filter(fun((X) -> bool()), [X]) -> [X].
