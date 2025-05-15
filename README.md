Overview
--------------

Spillway is an Erlang OTP application used for load shedding. The idea behind spillway is to use it
to be able to limit the number of in flight concurrent calls to a section of code.

Some examples:
 - As a server you can use spillway to limit the number of concurrent running requests to a service. You can
 determine when to throw away some requests by considering each request type and its weight.
 A request weight is measured by the cost fo performing the work (CPU/MEMORY) and the cost to not perform
 the work (BUSSINESS IMPACT).
 - As a client, when you face a failing server you might choose to retry a request. Spillway will let you
 implement a simple controlled-in-size-buffer mechanism that will allow you to retry some of the requests without
 running out of memory or resources.

Example of use
----------------

A process about to execute a named section of code whose maximum parallelism
should be limited will call `spillway:enter/2/3` with the name, the weight, and limit.

If the return value is the 2-tuple `{true, TotalWeight}`, the process may enter the section of code
 (there now being TotalWeight in use concurrently-executing accesses), and otherwise not.

If the process entered the section of code, it should call `spillway:leave/2` with the name and weight
after completion.

No special arrangement is made to handle process exits.  If a process dies without
calling `spillway:leave/1`, the counter will be inaccurate.  This is intentional,
and callers should make arrangements to mitigate this occurrence.

```
case spillway:enter(running_requests, Weight, Limit) of
 {true, Value} ->
        try
           continue_executing(Something);
        after
            spillway:leave(running_requests, Weight)
        end;
 false ->
     discard(Something)

end.

```

Setup
-------

- Add the application to your rebar3 dependencies and start the application normally.
Alternatively you can also attach the supervision tree directly to the main supervisor of your
application.

Implementation
----------------
Spillway is implemented based on ETS-based bounded named counters.


Build
-----

```shell
  make
  make ct
```

1.x Changelog
-------------
1.1 2018-07-13
 * Remove the need to initialize counters
1.0.0 2018-07-11
 * Add initial implementation
