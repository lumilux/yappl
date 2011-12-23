open Builtin
open Hashtbl

let _ =
let rec yappl_flip yappl_bias unit = 
 ( Builtin.rand  () ) <= ( yappl_bias ) 
in
let rec yappl_fflip  unit = 
 yappl_flip ( 0.5 ) () 
in
let rec yappl_geom yappl_q unit = 
 let rec yappl_geom_helper yappl_orig_q yappl_i unit = 
 if ( ( Builtin.rand  () ) < ( yappl_orig_q ) ) then ( yappl_i ) else ( yappl_geom_helper ( yappl_orig_q ) ( ( yappl_i ) + ( 1 ) ) () ) 
in
yappl_geom_helper ( yappl_q ) ( 1 ) () 
in
let rec yappl_beta yappl_a yappl_b unit = 
 Builtin.rand  () 
in
let rec yappl_pickastick yappl_sticks yappl_j unit = 
 if ( ( Builtin.rand  () ) < ( yappl_sticks ( yappl_j ) () ) ) then ( yappl_j ) else ( yappl_pickastick ( yappl_sticks ) ( ( yappl_j ) + ( 1 ) ) () ) 
in
let rec yappl_DP yappl_alpha yappl_proc unit = 
 let rec table_yappl_sticks tabl yappl_x unit = 
 let rec no_mem_yappl_sticks yappl_x unit = 
 yappl_beta ( 1. ) ( yappl_alpha ) ()
in
try Hashtbl.find tabl ( yappl_x ) with Not_found ->
let result = no_mem_yappl_sticks yappl_x () in 
Hashtbl.add tabl yappl_x result; result
in
let hash_table_for_yappl_sticks = Hashtbl.create 50 in
let yappl_sticks = table_yappl_sticks hash_table_for_yappl_sticks in
let rec table_yappl_atoms tabl yappl_x unit = 
 let rec no_mem_yappl_atoms yappl_x unit = 
 yappl_proc  ()
in
try Hashtbl.find tabl ( yappl_x ) with Not_found ->
let result = no_mem_yappl_atoms yappl_x () in 
Hashtbl.add tabl yappl_x result; result
in
let hash_table_for_yappl_atoms = Hashtbl.create 50 in
let yappl_atoms = table_yappl_atoms hash_table_for_yappl_atoms in
let rec yappl_f  unit = 
 yappl_atoms ( yappl_pickastick ( yappl_sticks ) ( 1 ) () ) () 
in
yappl_f 
in
let rec yappl_DPmem yappl_alpha yappl_proc unit = 
 let rec table_yappl_dps tabl yappl_arg unit = 
 let rec no_mem_yappl_dps yappl_arg unit = 
 let rec yappl_apply  unit = 
 yappl_proc ( yappl_arg ) () 
in
yappl_DP ( yappl_alpha ) ( yappl_apply ) ()
in
try Hashtbl.find tabl ( yappl_arg ) with Not_found ->
let result = no_mem_yappl_dps yappl_arg () in 
Hashtbl.add tabl yappl_arg result; result
in
let hash_table_for_yappl_dps = Hashtbl.create 50 in
let yappl_dps = table_yappl_dps hash_table_for_yappl_dps in
let rec yappl_dp yappl_arg unit = 
 yappl_dps ( yappl_arg ) () 
in
yappl_dp 
in
 
 let yappl_geom_dp = yappl_DPmem ( 1. ) ( yappl_geom ) () in 
 (  
 let yappl_mydraw = yappl_geom_dp ( 0.2 ) () in 
 ( let rec yappl_loop yappl_i unit = 
 (ignore ( print_int ( yappl_mydraw  () ); print_char ' '; true ));
if ( ( yappl_i ) > ( 0 ) ) then ( yappl_loop ( ( yappl_i ) - ( 1 ) ) () ) else ( true ) 
in
(ignore ( Builtin.seed  () ));
(ignore ( yappl_loop ( 30 ) () ));
ignore ( print_int ( yappl_mydraw  () ); print_char ' '; true );
 print_newline (); true ) )
