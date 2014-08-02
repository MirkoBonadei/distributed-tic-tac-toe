-define(assertProcessDownAfter(Pid, Timeout, Callback),
        begin
        ((fun () ->
            Ref = erlang:monitor(process, Pid),
            Callback(),
            receive
              {'DOWN', Ref, process, Pid, normal} ->
                ok
            after
              Timeout ->
                erlang:error({assertion_failed,
                              [{module, ?MODULE},
                               {line, ?LINE},
                               {expression, (??Callback)},
                               {expected, process_terminated},
                               {value, process_is_still_alive}]})
            end
          end)())
        end).
