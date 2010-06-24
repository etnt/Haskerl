--
-- Created: 9 Jul 2007 by tobbe@tornkvist.org
--

module Ebif2 where

--import Parser2
import Bkeep2

type BifSpec = (String,String,Arity)


isBif :: Name -> Maybe BifSpec
isBif "+"   = Just ("erlang","+",2)
isBif "-"   = Just ("erlang","-",2)
isBif "*"   = Just ("erlang","*",2)
isBif "/"   = Just ("erlang","/",2)
isBif ">"   = Just ("erlang",">",2)
isBif "<"   = Just ("erlang","<",2)
isBif ">="  = Just ("erlang",">=",2)
isBif "<="  = Just ("erlang","=<",2)
isBif "=:=" = Just ("erlang","=:=",2)
isBif "=="  = Just ("erlang","==",2)
isBif "=/=" = Just ("erlang","=/=",2)
isBif "/="  = Just ("erlang","/=",2)
isBif "&&"  = Just ("erlang","and",2)
isBif "||"  = Just ("erlang","or",2)
isBif "error"  = Just ("erlang","error",1)
isBif _     = Nothing


{-

 More Erlang BIFs, but perhaps it is better to
 require that they always are prefixed with the
 erlang module prefix ?

ubif erlang:abs/1
bif erlang:apply/3
bif erlang:atom_to_list/1
bif erlang:binary_to_list/1
bif erlang:binary_to_list/3
bif erlang:binary_to_term/1	
bif erlang:check_process_code/2
bif erlang:date/0
bif erlang:delete_module/1
bif erlang:display/1
bif erlang:display_string/1
bif erlang:display_nl/0
ubif erlang:element/2
bif erlang:erase/0
bif erlang:erase/1
bif erlang:exit/1
bif erlang:exit/2
bif erlang:external_size/1
ubif erlang:float/1
bif erlang:float_to_list/1
bif erlang:fun_info/2
bif erlang:garbage_collect/0
bif erlang:garbage_collect/1
bif erlang:garbage_collect_message_area/0
bif erlang:get/0
bif erlang:get/1
bif erlang:get_keys/1
bif erlang:group_leader/0
bif erlang:group_leader/2
bif erlang:halt/0
bif erlang:halt/1
bif erlang:phash/2
bif erlang:phash2/1
bif erlang:phash2/2
ubif erlang:hd/1
bif erlang:integer_to_list/1
bif erlang:is_alive/0
ubif erlang:length/1
bif erlang:link/1
bif erlang:list_to_atom/1
bif erlang:list_to_binary/1
bif erlang:list_to_float/1
bif erlang:list_to_integer/1
bif erlang:list_to_pid/1
bif erlang:list_to_tuple/1
bif erlang:load_module/2
bif erlang:loaded/0
bif erlang:localtime/0
bif erlang:localtime_to_universaltime/2
bif erlang:make_ref/0
bif erlang:md5/1
bif erlang:md5_init/0
bif erlang:md5_update/2
bif erlang:md5_final/1
bif erlang:module_loaded/1
bif erlang:function_exported/3
bif erlang:monitor_node/2
bif erlang:monitor_node/3
ubif erlang:node/1
ubif erlang:node/0
bif erlang:nodes/1
bif erlang:now/0
bif erlang:open_port/2
bif erlang:pid_to_list/1
bif erlang:port_info/1
bif erlang:port_info/2
bif erlang:ports/0
bif erlang:pre_loaded/0
bif erlang:process_flag/2
bif erlang:process_flag/3
bif erlang:process_info/1
bif erlang:process_info/2
bif erlang:processes/0
bif erlang:purge_module/1
bif erlang:put/2
bif erlang:register/2
bif erlang:registered/0
ubif erlang:round/1
ubif erlang:self/0
bif erlang:setelement/3
ubif erlang:size/1
ubif erlang:bitsize/1
bif erlang:spawn/3
bif erlang:spawn_link/3
bif erlang:split_binary/2
bif erlang:statistics/1
bif erlang:term_to_binary/1
bif erlang:term_to_binary/2
bif erlang:throw/1
bif erlang:time/0
ubif erlang:tl/1
ubif erlang:trunc/1
bif erlang:tuple_to_list/1
bif erlang:universaltime/0
bif erlang:universaltime_to_localtime/1
bif erlang:unlink/1
bif erlang:unregister/1
bif erlang:whereis/1
bif erlang:spawn_opt/1
bif erlang:setnode/2
bif erlang:setnode/3
bif erlang:dist_exit/3
bif erlang:port_call/2
bif erlang:port_call/3
bif erlang:port_command/2
bif erlang:port_control/3
bif erlang:port_close/1
bif erlang:port_connect/2
bif erlang:port_set_data/2
bif erlang:port_get_data/1
bif erlang:trace_pattern/2
bif erlang:trace_pattern/3
bif erlang:trace/3
bif erlang:trace_info/2
bif erlang:trace_delivered/1
bif erlang:seq_trace/2
bif erlang:seq_trace_info/1
bif erlang:seq_trace_print/1
bif erlang:seq_trace_print/2
bif erlang:suspend_process/1
bif erlang:resume_process/1
bif erlang:process_display/2
# Used to implemented in the erlang:info/1 BIF.
bif erlang:bump_reductions/1
bif erlang:start_timer/3
bif erlang:send_after/3
bif erlang:cancel_timer/1
bif erlang:read_timer/1
bif erlang:make_tuple/2
bif erlang:append_element/2
bif erlang:system_flag/2
bif erlang:system_info/1
bif erlang:system_monitor/0
bif erlang:system_monitor/1
bif erlang:system_monitor/2
bif erlang:ref_to_list/1
bif erlang:port_to_list/1
bif erlang:fun_to_list/1
bif erlang:monitor/2
bif erlang:demonitor/1
bif erlang:is_process_alive/1
bif erlang:error/1		error_1
bif erlang:fault/1		fault_1
bif erlang:error/2		error_2
bif erlang:fault/2		fault_2
bif erlang:raise/3		raise_3
bif erlang:get_stacktrace/0
bif erlang:is_builtin/3
# erlang:send/2, erlang:append/2 and erlang:subtract/2 are now also
bif erlang:send/2
bif erlang:send/3
bif erlang:append/2
bif erlang:subtract/2
ubif erlang:is_atom/1
ubif erlang:is_list/1
ubif erlang:is_tuple/1
ubif erlang:is_constant/1
ubif erlang:is_float/1
ubif erlang:is_integer/1
ubif erlang:is_number/1
ubif erlang:is_pid/1
ubif erlang:is_port/1
ubif erlang:is_reference/1
ubif erlang:is_binary/1
ubif erlang:is_bitstr/1
ubif erlang:is_function/1
ubif erlang:is_function/2
ubif erlang:is_record/2
ubif erlang:is_record/3
bif erlang:match_spec_test/3
bif erlang:blocking_read_file/1
bif erlang:hibernate/3
bif erlang:get_module_info/1
bif erlang:get_module_info/2
ubif erlang:is_boolean/1
bif erlang:make_fun/3
bif erlang:iolist_size/1
bif erlang:iolist_to_binary/1
bif erlang:list_to_existing_atom/1
bif erlang:list_to_bitstr/1
bif erlang:bitstr_to_list/1
bif erlang:hash/2
bif erlang:old_binary_to_term/1
#bif erlang:concat_binary/1
#bif erlang:info/1

-}
