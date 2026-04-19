(** Retry a flaky operation with exponential backoff.

    Used to make network-bound steps (git clone, opam fetch) tolerant of
    transient DNS/connection failures and rate limits without users
    having to re-run the whole command. *)

val with_attempts :
  label:string ->
  ?max_attempts:int ->
  ?initial_delay_s:float ->
  (unit -> 'a) ->
  'a
(** [with_attempts ~label ?max_attempts ?initial_delay_s f] runs [f ()]
    and returns its result. If it raises, sleeps for the current delay
    and retries; the delay doubles each time. After [max_attempts]
    consecutive failures the last exception is re-raised so the caller
    can surface a real error. Each retry logs a warning under the
    [oi.retry] log source citing [label] so operators can see what
    flaked.

    Defaults: [max_attempts = 3], [initial_delay_s = 1.0] (so the wait
    schedule is 1s, then 2s, then give up after the third attempt). *)
