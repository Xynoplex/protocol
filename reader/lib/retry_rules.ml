(* retry/rules.json states what each retry rule retries, as sets, and
   retry/cases.json's rows are examples of them. Each set's statuses are in
   ascending order, and its [statusFrom] is above all of them. Every row says
   what the sets say for its status, method and key, and every status a set
   names is retried by some row of its rule, so the rows reach every member. *)

let file = "retry/rules.json"
let cases = "retry/cases.json"
let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token

let refuse file pointer fmt =
  Printf.ksprintf (fun message -> [ { Decode.file; pointer; message } ]) fmt

(* A row of retry/cases.json: its rule, the status it states, what it says,
   and what retry/rules.json says. *)
type row = { rule : string; status : int option; says : bool; sets : bool }

let retries_status ?from statuses status =
  List.mem status statuses
  || match from with Some from -> status >= from | None -> false

let row (rules : Tables.Retry_rules.t) : Tables.Retry_cases.rules_item -> row =
  function
  | Boot row ->
      {
        rule = "boot";
        status = Some row.status;
        says = row.retries;
        sets = retries_status rules.boot.statuses row.status;
      }
  | Job row ->
      {
        rule = "job";
        status = Some row.status;
        says = row.retries;
        sets =
          retries_status ~from:rules.job.status_from rules.job.statuses
            row.status;
      }
  | Transport row ->
      let repeatable =
        row.keyed = Some true
        || Option.fold ~none:false
             ~some:(fun method_ ->
               List.mem
                 (String.uppercase_ascii method_)
                 rules.transport.idempotent_methods)
             row.method_
      in
      {
        rule = "transport";
        status = row.status;
        says = row.retries;
        sets =
          repeatable
          && Option.fold ~none:false
               ~some:(retries_status rules.transport.statuses)
               row.status;
      }
  | Refresh row ->
      {
        rule = "refresh";
        status = row.status;
        says = row.retries;
        sets =
          Option.fold ~none:rules.refresh.no_response
            ~some:
              (retries_status ~from:rules.refresh.status_from
                 rules.refresh.statuses)
            row.status;
      }

let rec ascending = function
  | first :: (second :: _ as rest) -> first < second && ascending rest
  | _ -> true

(* The pattern names every set, so a set retry/rules.json gains fails the
   build until it is read here. *)
let refusals (rules : Tables.Retry_rules.t) (table : Tables.Retry_cases.t) =
  let { Tables.Retry_rules.comment = _; boot; job; transport; refresh } =
    rules
  in
  let rows = List.map (row rules) table.rules in
  let ordered rule statuses =
    if ascending statuses then []
    else refuse file ("" / rule / "statuses") "is not in ascending order"
  in
  let above rule statuses from =
    match List.filter (fun status -> status >= from) statuses with
    | [] -> []
    | covered ->
        refuse file
          ("" / rule / "statusFrom")
          "is %d, and %s names %s, which it covers" from
          ("" / rule / "statuses")
          (String.concat ", " (List.map string_of_int covered))
  in
  let disagreeing =
    List.concat
      (List.mapi
         (fun index row ->
           if row.says = row.sets then []
           else
             refuse cases
               ("" / "rules" / string_of_int index / "retries")
               "is %b, and %s's %s set says %b" row.says file row.rule row.sets)
         rows)
  in
  let reached rule pointer status =
    if
      List.exists
        (fun row -> row.rule = rule && row.says && row.status = Some status)
        rows
    then []
    else
      refuse file pointer "is %d, and no row of %s's %s rule retries it" status
        cases rule
  in
  let members rule statuses =
    List.concat
      (List.mapi
         (fun index status ->
           reached rule ("" / rule / "statuses" / string_of_int index) status)
         statuses)
  in
  ordered "boot" boot.statuses
  @ ordered "job" job.statuses
  @ ordered "transport" transport.statuses
  @ ordered "refresh" refresh.statuses
  @ above "job" job.statuses job.status_from
  @ above "refresh" refresh.statuses refresh.status_from
  @ disagreeing
  @ members "boot" boot.statuses
  @ members "job" job.statuses
  @ reached "job" ("" / "job" / "statusFrom") job.status_from
  @ members "transport" transport.statuses
  @ members "refresh" refresh.statuses
  @ reached "refresh" ("" / "refresh" / "statusFrom") refresh.status_from
