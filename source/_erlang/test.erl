on_presence(User, Server, Resource, Packet) ->
    %% get options
    StormCount = gen_mod:get_module_opt(Server, ?MODULE, count, 10),
    TimeInterval = gen_mod:get_module_opt(Server, ?MODULE, interval, 60),
    LUser = jlib:nodeprep(User),
    LServer = jlib:nodeprep(Server),
    {MegaSecs, Secs, _MicroSecs} = now(),
    TimeStamp = MegaSecs * 1000000 + Secs,
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
            if
                TimeStamp - TimeStart > TimeInterval ->
                    F = fun() ->
                        mnesia:write(#sunshine{usr = {LUser,
                            LServer,
                            Resource},
                        packet = Packet,
                        start = TimeStamp,
                        count = 1})
                    end,
                    mnesia:transaction(F);
                Count =:= StormCount ->
                    %% TODO: disconnect user
                    F = fun() ->
                        mnesia:delete({sunshine, {LUser, LServer,
                            Resource}})
                    end,
                    mnesia:transaction(F);
                true ->
                    F = fun() ->
                        mnesia:write(#sunshine{usr = {LUser, LServer,
                            Resource},
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