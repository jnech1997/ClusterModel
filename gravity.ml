open GMain
open GdkKeysyms

exception Break

(** [button_table] is a hashtable of all the coordinates (int*int)
 * in the table (keys) and the buttons that correspond to them (values) *)
let button_table = (Hashtbl.create 64)

(** [x] initializes the seed for Random *)
let x = Random.init 100

(** [size] x [size] is the size of the gui grid *)
let size : int ref = ref 50

(** [e] is Euler's number *)
let e : float = 2.7182818284590452353602874713527

(** [alpha] is the maxwell boltzman constant *)
let alpha : float = (*~-. 100.*) 5. (*0.*)

(** [sum_total] is sum of e to the minus alpha for all the tiles in the gui grid *)
let sum_total : float ref = ref ((float_of_int ((!size + 1) * (!size +1))) *. e ** ((~-. 1.) *. alpha))

(** [is_filled] returns true if GButton [b] has an image in it *)
let is_filled b : bool = try (b#child;true) with | Gpointer.Null -> false

(** [xpm_label_box] creates a new pixmap with an image packed into it
 * and packs it into the [button]. Taken from LablGtk tutorial:
 * http://tinyurl.com/zskf6jt *)
let xpm_label_box ~file button () : unit =
  if not (Sys.file_exists file) then failwith (file ^ "does not exist");
  (* create box for image and label and pack *)
  let pixmap = GDraw.pixmap_from_xpm ~file () in
  GMisc.pixmap pixmap ~packing:(button#add) ~width: 20 ~height: 20 (); ()

(** [is_bounded] calculates if float [r] is bounded by the interval
 * given by lower bound and upper bound [(lb,ub) *)
let is_bounded (r: float) ((lb,ub): float*float) : bool =
  if r >= lb && r < ub then true else false

(** [is_on_grid] calculates if the coordinate given by [(x,y)]
 * is on the gui grid *)
let is_on_grid (x,y) =
  if x >= 0 && x <= !size && y >= 0 && y <= !size then true else false

(** [filled_neighbors] calculates the number of neighbors adjacent to the
 * coordinate given by [(x,y)] that are filled with an image *)
let filled_neighbors tbl (x,y) =
  let count = ref 0 in
  (for dy = -1 to 1 do
    for dx = -1 to 1 do
      if (dy = 0 && dx = 0) then ()
      else
        let tempx = ref (x + dx) in
        let tempy = ref (y + dy) in
        if (not (is_on_grid (!tempx,!tempy))) then ()
        else
          if is_filled (fst (Hashtbl.find tbl (!tempx,!tempy))) then
            count := !count + 1
          else ()
        done
      done);
      !count

(** [prob_i_j] calculates the probability that a tile is filled
 * with an image using the maxwell boltzman equation *)
let prob_i_j tbl (x,y) =
  let numerator =
    match filled_neighbors tbl (x,y) with
    | 0 -> e ** ((~-. 1.) *. alpha)
    | _ -> 1. in
  let denominator =
    !sum_total in
  numerator /. denominator

(** [ranger] returns the coordinates of the tile
 * in hashtable [tbl] whose probability range contains [r] *)
let ranger r tbl =
  let coo = ref (0,0) in
  try (
    for x = 0 to !size do
      for y = 0 to !size do
        let v = Hashtbl.find tbl (x,y) in
        let range = !(snd v) in
        match is_bounded r range with
        | true -> coo := (x,y); failwith "found it" (* throws exception to break loop *)
        | false ->  ()
      done
    done; !coo
  ) with
  | Failure s -> !coo

(** [co_before] returns the coordinate on the grid directly before
 * [(x,y)] as if the tiles in the gui grid were a list with each row of tiles concatenated
 * on to the end of the list. Precondition: x & y >= 0 && x & y <= size *)
let co_before (x,y) =
  if y = 0 && x = 0 then failwith "nothing before (0,0)"
  else if y > 0 then (x, y -1)
  else (x - 1, !size)

(** [re_update] reassigns an updated probability to each coordinate value in hashtable
 * [tbl] which corresponds to each tile in the gui grid. This allows for the
 * cluster modeling this program is designed to highlight *)
let re_update tbl =
  for x = 0 to !size do
    for y = 0 to !size do
      match (x,y) with
      | (0,0) ->
        let v = Hashtbl.find tbl (0,0) in
        let p = prob_i_j tbl (0,0) in
        snd v := (0.,p)
      | _ ->
        let v = Hashtbl.find tbl (x,y) in
        let p = prob_i_j tbl (x,y) in
        let v_before = Hashtbl.find tbl (co_before (x,y)) in
        let r_before = !(snd v_before) in
        let ub_before = snd r_before in
        snd v := (ub_before, ub_before +. p)
      done
    done

(* initialize the GUI Main loop *)
let locale = GtkMain.Main.init ()

(* ------------- CREATES THE GUI AND RUNS THE PROGRAM --------------*)
let main () =
  (* window on which the GUI is made *)
  let window =
    GWindow.window ~width:1000 ~height:700 ~title:"Gravity Conglomeration" () in
  (* vbox necessary for packing GUI and turn button into window *)
  let vbox = GPack.vbox ~packing:window#add () in
  window#connect#destroy ~callback:GMain.quit;
  (* Table *)
  let hbox_table = GPack.hbox ~packing:vbox#add () in
  let table = GPack.table ~homogeneous:true ~width: 475 ~height: 475
    ~packing:hbox_table#add () in
  (* Creates Grid of button listeners *)
  for x = 0 to !size do
    for y = 0 to !size do
      let button = GButton.button () in
      let b_num = ((float_of_int !size) +. 1.) *. (float_of_int x) +. (float_of_int y) in
      let num_squares = float_of_int ((!size + 1) * (!size + 1)) in
      Hashtbl.add button_table (x,y)
        (button, ref (b_num/.num_squares,(b_num +. 1.)/.num_squares));
      table#attach ~left:x ~top:y ~expand:`BOTH (button#coerce);
     (* called when user presses button on gui *)
     button#connect#pressed ~callback: (fun () ->
        for x = 0 to 10 do
          let r = Random.float 1. in
          let co_to_fill = ranger r button_table in
          let b = fst (Hashtbl.find button_table co_to_fill) in
          (match is_filled b with
          | true -> ()
          | false ->
            xpm_label_box ~file:"newton.xpm" b ();
            let num_adj = filled_neighbors button_table co_to_fill in
            sum_total := (!sum_total -. (float_of_int num_adj) *.
              (e ** ((~-. 1.) *. alpha)) +. (float_of_int num_adj))
            );
          re_update button_table; ()
        done)
    done
  done;
(* Display the windows *)
window#show ();
GMain.main ()

(* runs the GUI main loop *)
let () = main ()

