%% mod_sunshine.erl
%% 
%% This module ends the session for people who send presence storms.
%%
%% A presence storm is defined as resending the same presence stanza
%% more than 10 times in 60 seconds without any intermediate different
%% presence stanzas.

-module(mod_sunshine).
-author("Jack Moffitt <jack@chesspark.com>").

-behavior(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").

-export([start/2, stop/1, on_presence/4]).

-record(sunshine, {usr, packet, start, count}).

start(Host, Opts) ->
    ?INFO_MSG("mod_sunshine starting", []),
    mnesia:create_table(sunshine, [{attributes, record_info(fields, sunshine)}]),
    mnesia:clear_table(sunshine),
    ejabberd_hooks:add(set_presence_hook, Host, ?MODULE, on_presence, 50),
    ok.

stop(Host) ->
    ?INFO_MSG("mod_sunshine stopping", []),
    ejabberd_hooks:delete(set_presence_hook, Host, ?MODULE, on_presence, 50),
    ok.

on_presence(User, Server, Resource, Packet) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nodeprep(Server),
    {MegaSecs, Secs, _MicroSecs} = now(),
    TimeStamp = MegaSecs * 1000000 + Secs,

    %% get options
    StormCount = gen_mod:get_module_opt(Server, ?MODULE, count, 10),
    TimeInterval = gen_mod:get_module_opt(Server, ?MODULE, interval, 60),

    case catch mnesia:dirty_read(sunshine, {LUser, LServer, Resource}) of
        [] ->
            %% no record for this key, so make a new one
            F = fun() ->
                mnesia:write(#sunshine{usr = {LUser, LServer, Resource},
                packet = Packet,
                start = TimeStamp,
                count = 1})
            end,
            mnesia:transaction(F);
        [#sunshine{usr = {LUser, LServer, Resource},
            packet = Packet, start = TimeStart, count = Count}] ->
            %% record for this key and packet exists, check if we're
            %% within TimeInterval seconds, and whether the StormCount is
            %% high enough.  or else just increment the count.
            if TimeStamp - TimeStart > TimeInterval ->
                F = fun() ->
                    mnesia:write(#sunshine{usr = {LUser, LServer, Resource},
                    packet = Packet,
                    start = TimeStamp,
                    count = 1})
                end,
                mnesia:transaction(F);
                Count =:= StormCount ->
                    SID = ejabberd_sm:get_session_pid(LUser, LServer, Resource),
                    SID ! disconnect,
                    F = fun() ->
                        mnesia:delete({sunshine, {LUser, LServer, Resource}})
                    end,
                    mnesia:transaction(F);
                true ->
                    F = fun() ->
                        mnesia:write(#sunshine{usr = {LUser, LServer, Resource},
                        packet = Packet,
                        start = TimeStamp,
                        count = Count + 1})
                    end,
                    mnesia:transaction(F)
            end;
        [#sunshine{usr = {LUser, LServer, Resource},
            packet = _OtherPacket, count = _OtherCount}] ->
            %% a record for this key was found, but for another packet,
            %% so we replace it with a new record.
            F = fun() ->
                mnesia:write(#sunshine{usr = {LUser, LServer, Resource},
                packet = Packet,
                start = TimeStamp,
                count = 1})
            end,
            mnesia:transaction(F)
    end,
    none.
