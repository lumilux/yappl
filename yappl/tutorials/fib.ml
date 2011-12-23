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
let rec yappl_fib yappl_n unit = 
 if ( ( yappl_n ) <= ( 1 ) ) then ( yappl_n ) else ( ( yappl_fib ( ( yappl_n ) - ( 1 ) ) () ) + ( yappl_fib ( ( yappl_n ) - ( 2 ) ) () ) ) 
in
let rec table_yappl_fib_memo tabl yappl_n unit = 
 let rec no_mem_yappl_fib_memo yappl_n unit = 
 if ( ( yappl_n ) <= ( 1 ) ) then ( yappl_n ) else ( ( table_yappl_fib_memo tabl ( ( yappl_n ) - ( 1 ) ) () ) + ( table_yappl_fib_memo tabl ( ( yappl_n ) - ( 2 ) ) () ) )
in
try Hashtbl.find tabl ( yappl_n ) with Not_found ->
let result = no_mem_yappl_fib_memo yappl_n () in 
Hashtbl.add tabl yappl_n result; result
in
let hash_table_for_yappl_fib_memo = Hashtbl.create 50 in
let yappl_fib_memo = table_yappl_fib_memo hash_table_for_yappl_fib_memo in
(ignore ( ignore ( print_int ( yappl_fib ( 5 ) () ); print_char ' '; true );
 print_newline (); true ));
(ignore ( ignore ( print_int ( yappl_fib ( 20 ) () ); print_char ' '; true );
 print_newline (); true ));
(ignore ( ignore ( print_int ( yappl_fib ( 35 ) () ); print_char ' '; true );
 print_newline (); true ));
(ignore ( ignore ( print_int ( yappl_fib ( 40 ) () ); print_char ' '; true );
 print_newline (); true ));
(ignore ( ignore ( print_int ( yappl_fib_memo ( 5 ) () ); print_char ' '; true );
 print_newline (); true ));
(ignore ( ignore ( print_int ( yappl_fib_memo ( 20 ) () ); print_char ' '; true );
 print_newline (); true ));
(ignore ( ignore ( print_int ( yappl_fib_memo ( 35 ) () ); print_char ' '; true );
 print_newline (); true ));
ignore ( print_int ( yappl_fib_memo ( 40 ) () ); print_char ' '; true );
 print_newline (); true
