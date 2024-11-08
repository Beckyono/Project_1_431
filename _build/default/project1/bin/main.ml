(*Author:   Rebecca Ostrander*)
(*Purpose:  the main file :P*)
(*Filename: main.ml*)

(*The Library*)
open Dream

(**)
type entry = {
  check_in : string;
  check_out : string;
  rating : int;
  comment : string option;
}

(**)
let guestbook = ref []

(**)
let render_form _req =
  html
    "<form method='POST' action='/add'>
      Check-in: <input type='datetime-local' name='check_in'><br>
      Check-out: <input type='datetime-local' name='check_out'><br>
      Rating (out of 5): <input type='number' name='rating' min='1' max='5'><br>
      Comment: <textarea name='comment'></textarea><br>
      <button type='submit'>Submit</button>
     </form>"

(**)
let add_entry req =
  let check_in = Dream.param req "check_in" in
  let check_out = Dream.param req "check_out" in
  let rating = int_of_string (Dream.param req "rating") in
  let comment = Dream.param req "comment" in
  let entry = { check_in; check_out; rating; comment = Some comment } in
  guestbook := entry :: !guestbook;
  Dream.html "Entry added successfully! <a href='/'>Back to form</a>"

(**)
let view_entries _req =
  let entries_html =
    !guestbook
    |> List.map (fun entry ->
           Printf.sprintf
             "<p>Check-in: %s<br>Check-out: %s<br>Rating: %d<br>Comment: %s</p>"
             entry.check_in entry.check_out entry.rating
             (match entry.comment with Some c -> c | None -> "No comment"))
    |> String.concat "\n"
  in
  Dream.html (Printf.sprintf "<h1>Guestbook Entries</h1>%s" entries_html)

(**)
let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router
       [ Dream.get "/" render_form;
         Dream.post "/add" add_entry;
         Dream.get "/entries" view_entries ]