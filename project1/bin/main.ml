open Dream

(* Define a type to represent guestbook entries *)
type entry = {
  name : string;
  check_in : string;
  check_out : string;
  rating : int;
  comment : string option;
}

(* File to store guestbook entries *)
let guestbook_file = "guestbook_entries.txt"

(* Load entries from file *)
let load_entries () =
  if Sys.file_exists guestbook_file then
    let ic = open_in guestbook_file in
    let rec read_entries acc =
      try
        let line = input_line ic in
        match String.split_on_char ';' line with
        | [name; check_in; check_out; rating; comment] ->
            let entry = {
              name;
              check_in;
              check_out;
              rating = int_of_string rating;
              comment = if comment = "" then None else Some comment;
            } in
            read_entries (entry :: acc)
        | _ -> read_entries acc
      with End_of_file ->
        close_in ic;
        List.rev acc
    in
    read_entries []
  else []

(* Save entries to file *)
let save_entry entry =
  let oc = open_out_gen [Open_append; Open_creat] 0o666 guestbook_file in
  Printf.fprintf oc "%s;%s;%s;%d;%s\n"
    entry.name entry.check_in entry.check_out entry.rating
    (match entry.comment with Some c -> c | None -> "");
  close_out oc

(* Temporary storage for guestbook entries *)
let guestbook = ref (load_entries ())

(* Render the guestbook form *)
let render_form _req =
  html
    "<form method='POST' action='/add'>
      Name: <input type='text' name='name' required><br>
      Check-in: <input type='datetime-local' name='check_in' required><br>
      Check-out: <input type='datetime-local' name='check_out' required><br>
      Rating (out of 5): <input type='number' name='rating' min='1' max='5' required><br>
      Comment: <textarea name='comment'></textarea><br>
      <button type='submit'>Submit</button>
     </form>"

(* Handle form submission *)
let add_entry req =
  let name = Dream.param req "name" in
  let check_in = Dream.param req "check_in" in
  let check_out = Dream.param req "check_out" in
  let rating = int_of_string (Dream.param req "rating") in
  let comment = Dream.param req "comment" in
  (* Create a new entry and add it to guestbook *)
  let entry = { name; check_in; check_out; rating; comment = Some comment } in
  guestbook := entry :: !guestbook;
  save_entry entry;
  Dream.html "Entry added successfully! <a href='/'>Back to form</a>"

(* Render the guestbook entries *)
let view_entries _req =
  let entries_html =
    !guestbook
    |> List.map (fun entry ->
           Printf.sprintf
             "<p>Name: %s<br>Check-in: %s<br>Check-out: %s<br>Rating: %d<br>Comment: %s</p>"
             entry.name entry.check_in entry.check_out entry.rating
             (match entry.comment with Some c -> c | None -> "No comment"))
    |> String.concat "\n"
  in
  Dream.html (Printf.sprintf "<h1>Guestbook Entries</h1>%s" entries_html)

(* Define the routes *)
let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router
       [ Dream.get "/" render_form;
         Dream.post "/add" add_entry;
         Dream.get "/entries" view_entries ]
