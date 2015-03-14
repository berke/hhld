(* Copyright (C) 2005 Henri Binsztok
   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
*)

(** Hidden Hidden Designer
    @author Henri Binsztok <Henri.Binsztok@lip6.fr>
    @licence GPL Version 2.0 <http://www.gnu.org>
    @todo 
    - indiquer que card n'est pas modifié tant qu'on ne fait pas un nouveau modèle
    - intégrer l'edit_vector dans une troisième hbox en haut ?
    - transitions à la souris en cliquant entre états
    - chargement et éditions de plusieurs modèles si possibles
    - possibilité de lier les sous-modèles entre eux
    - création d'états par drag 'n drop (ala Clementine)
    - ajouter automatiquement l'extension .hhmm aux fichiers sauvés
    - add scrolling to image
    - afficher les Hhmm of string en popup ?
*)

open Hhmm

type data = { mutable h : I.h 
	    ; p : GData.adjustment
	    ; i : GData.adjustment
	    ; j : GData.adjustment
	    ; f : GData.adjustment
	    ; test : GData.adjustment
	    ; pi : GData.adjustment
	    ; card : GData.adjustment }

(** save into file *)
let save f s =
  let ff = open_out f in
  output_string ff s ;
  close_out ff

let edit_vector ~callback v =
  let l = Array.length v in
  let w = GWindow.window ~width:(64*l+64) ~title:"Emission p.d.f." ~modal:true () in
  let vbox = GPack.vbox ~spacing:5 ~packing:w#add () in
  let hbox = GPack.hbox ~spacing:5 ~packing:(vbox#pack ~expand:false) () in
  let vv = Array.mapi (
    fun i x -> 
      let r = GEdit.entry ~text:(string_of_float x) ~width:32 ~packing:hbox#add () in
      ignore (r#connect#changed ~callback:(fun () -> v.(i) <- float_of_string (r#text))) ;
      r
  ) v in
  let hbox = GPack.hbox ~spacing:5 ~packing:(vbox#pack ~expand:false) () in
  let cancel = GButton.button ~stock:`CANCEL ~packing:hbox#add () in
  let ok = GButton.button ~stock:`OK ~packing:hbox#add () in
  (* callbacks *)
  ignore (cancel#connect#clicked ~callback:w#destroy) ;
  ignore (ok#connect#clicked ~callback:(fun () -> callback v ; w#destroy ())) ;
  w#show ()

let convert image data =
  let d = to_dot [`trans] data.h 
  and upper = float_of_int (pred (I.size data.h)) in
  save "/tmp/hhd.dot" d ;
  assert (Sys.command "dot -T png -o /tmp/hhd.png /tmp/hhd.dot" = 0) ;
  image#set_file "/tmp/hhd.png" ;
  List.iter (fun (x:GData.adjustment) -> x#set_bounds ~upper ()) [data.p ; data.i ; data.j ; data.test]

let new_model image data () =
  data.h <- I.root (int_of_float data.card#value) ; 
  convert image data
  
let add_internal image data (context:GMisc.statusbar_context) () =
  let p = int_of_float data.p#value 
  and pi = data.pi#value in
  try 
    data.h <- fst (I.add_internal data.h ~p ~pi) ;
    convert image data
  with
  | Hhmm s -> ignore (context#push ("Error: " ^ s))
  | _ -> ignore (context#push "Error: add_internal failed")

let add_normal image data (context:GMisc.statusbar_context) () =
  let p = int_of_float data.p#value
  and card = I.card data.h in
  let pi = data.pi#value 
  and a = I.empty_transitions
  and b = Array.make card (1. /. float_of_int card) in
  let callback b =
    try 
      data.h <- fst (I.add_normal data.h ~p ~pi ~a ~b) ;
      convert image data 
    with
    | Hhmm s -> ignore (context#push ("Error: " ^ s)) 
    | _ -> ignore (context#push "Error: add_normal failed") in
  edit_vector ~callback b

let add_transition image data (context:GMisc.statusbar_context) () =
  let i = int_of_float data.i#value
  and j = int_of_float data.j#value
  and f = data.f#value in
  try 
    data.h <- add_transition data.h i j f ;
    convert image data
  with
  | Hhmm s -> ignore (context#push ("Error: " ^ s)) 
  | _ -> ignore (context#push "Error: add_transition failed")

let conversion image data () =
  let del = int_of_float data.test#value in
  data.h <- I.convert_intern_state data.h del ;
  convert image data

let flatten_model image data () =
  data.h <- flatten data.h ;
  convert image data

(** make generic save window *)
let make_save_win title parent (context:GMisc.statusbar_context) filters func () =
  let savewin = GWindow.file_chooser_dialog ~action:`SAVE ~title ~parent () in
  savewin#add_button_stock `CANCEL `CANCEL ;
  savewin#add_button_stock `SAVE `SAVE ;
  List.iter savewin#add_filter filters ;
  if savewin#run () = `SAVE then
    begin match savewin#filename with
    | Some f -> ignore (context#push (func f))
    | _ -> () end ;
  savewin#destroy ()

let hhmm_filter = 
  let f = GFile.filter ~name:"Hierarchical HMM" () in
  List.iter f#add_pattern ["*.hhmm"] ; f

let dot_filter =
  let f = GFile.filter ~name:"Graphviz Dot" () in
  List.iter f#add_pattern ["*.dot"] ; f

open Hmm
let hmm_dot_f data f =
  let hmm = to_hmm (flatten data.h) in
  let dot = Hmm.D.to_dot [Add_first;Trans] hmm in
  save f dot ;
  Printf.sprintf "Dot graph saved to %s" f

let hhmm_dot_f data f =
  save f (to_dot [`trans] data.h) ;
  Printf.sprintf "Dot graph saved to %s" f

let hhmm_value_f data f =
  let ff = open_out f in
  output_value ff data.h ;
  close_out ff ;
  Printf.sprintf "Model saved to %s" f

let read_data f =
  let ff = open_in f in
  let r = input_value ff in
  close_in ff ; r
  
let open_file image data parent () =
  let openwin = GWindow.file_chooser_dialog ~action:`OPEN ~title:"Open File" ~parent () in
  openwin#add_button_stock `CANCEL `CANCEL ;
  openwin#add_button_stock `OPEN `OPEN ;
  openwin#add_filter (hhmm_filter) ;
  if openwin#run () = `OPEN then
    begin match openwin#filename with
    | Some f -> 
	data.h <- read_data f ;
	convert image data
    | _ -> () end ;
  openwin#destroy ()

let labelspin text (hbox:GPack.box) adjustment t =
  ignore (GMisc.label ~justify:`RIGHT ~text ~packing:(hbox#pack ~expand:false) ()) ;
  ignore (GEdit.spin_button ~adjustment ~digits:(if t=`int then 0 else 2) 
	    ~width:(if t=`int then 50 else 60) ~packing:(hbox#pack ~expand:false) ())

let make_win () =  
  let data = 
    let h = match Sys.argv with
    | [| _; f |] -> read_data f
    | _ -> I.root 1 in
    let card = float_of_int (I.card h) in
    { h = h
    ; p = GData.adjustment ~value:0. ~lower:0. ~upper:0. ~step_incr:1. ()
    ; i = GData.adjustment ~value:0. ~lower:0. ~upper:0. ~step_incr:1. ()
    ; j = GData.adjustment ~value:0. ~lower:0. ~upper:0. ~step_incr:1. ()
    ; f = GData.adjustment ~value:1. ~lower:0. ~upper:1. ~step_incr:0.05 ()
    ; pi = GData.adjustment ~value:1. ~lower:0. ~upper:1. ~step_incr:0.05 ()
    ; card = GData.adjustment ~value:card ~lower:1. ~upper:2048. ~step_incr:1. ()
    ; test = GData.adjustment ~value:0. ~lower:0. ~upper:0. ~step_incr:1. () } in
  let title = ".::hidden h.d._n designer::." in
  let window = GWindow.window ~width:860 ~height:660 ~title () in
  ignore (window#connect#destroy ~callback:GMain.quit) ;
  let vbox = GPack.vbox ~spacing:5 ~packing:window#add () in
  let hbox = GPack.hbox ~spacing:5 ~packing:(vbox#pack ~expand:false) () in
  labelspin "Card:" hbox data.card `int ;
  let b_new = GButton.button ~stock:`NEW ~packing:hbox#add () in
  let b_open = GButton.button ~stock:`OPEN ~packing:hbox#add () in
  let b_save = GButton.button ~stock:`SAVE ~packing:hbox#add () in
  labelspin "Add state to:" hbox data.p `int ;
  labelspin "pi:" hbox data.pi `float ;
  let b_internal = GButton.button ~label:"Internal" ~packing:hbox#add () in
  let b_normal = GButton.button ~label:"Normal" ~packing:hbox#add () in
  let b_dot = GButton.button ~label:"Save Dot (HHMM)" ~packing:hbox#add () in
  let hbox2 = GPack.hbox ~spacing:5 ~packing:(vbox#pack ~expand:false) () in
  labelspin "Transition from:" hbox2 data.i `int ;
  labelspin "to:" hbox2 data.j `int ;
  labelspin "probability:" hbox2 data.f `float ;
  let b_make = GButton.button ~label:"Make" ~packing:hbox2#add () in
  labelspin "Convert internal:" hbox2 data.test `int ;
  let b_convert = GButton.button ~label:"Perform" ~packing:hbox2#add () in
  let b_flatten = GButton.button ~label:"Flatten" ~packing:hbox2#add () in
  let b_hmmdot = GButton.button ~label:"Save Dot (HMM)" ~packing:hbox2#add () in  
  (* let _ = GMisc.separator ~packing:vbox#add `HORIZONTAL in *)
  let image = GMisc.image ~file:"/tmp/hhd.png" ~packing:vbox#add () in
  let status = GMisc.statusbar ~packing:(vbox#pack ~expand:false) () in
  let context = status#new_context ~name:"main" in
  ignore (context#push "(c) Henri Binsztok, 1999-2005") ;
  ignore (b_internal#connect#clicked ~callback:(add_internal image data context)) ;
  ignore (b_normal#connect#clicked ~callback:(add_normal image data context)) ;
  ignore (b_new#connect#clicked ~callback:(new_model image data));
  ignore (b_open#connect#clicked ~callback:(open_file image data window)) ;
  ignore (b_save#connect#clicked 
	    ~callback:(make_save_win "Save Model" window context [hhmm_filter] (hhmm_value_f data))) ;
  ignore (b_dot#connect#clicked 
	    ~callback:(make_save_win "Save Dot" window context [dot_filter] (hhmm_dot_f data))) ;
  ignore (b_make#connect#clicked ~callback:(add_transition image data context)) ;
  ignore (b_convert#connect#clicked ~callback:(conversion image data)) ;
  ignore (b_flatten#connect#clicked ~callback:(flatten_model image data)) ;
  ignore (b_hmmdot#connect#clicked 
	    ~callback:(make_save_win "Save Dot" window context [dot_filter] (hmm_dot_f data))) ;
  (* display *)
  convert image data ;
  window#show () ;
  GMain.main ()

let () =
  let locale = GtkMain.Main.init () in
  make_win ()
