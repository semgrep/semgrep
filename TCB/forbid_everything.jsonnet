// Rules to enforce the use of the TCB and forbid the use of the standard
// library or any non-vetted external libraries.
//
// related work:
//  - ppx_base_lint

local forbid_exit = import 'forbid_exit.jsonnet';
local forbid_network = import 'forbid_network.jsonnet';
local forbid_exec = import 'forbid_exec.jsonnet';
//TODO: forbid_fs, forbid_process, etc.
local forbid_tmp = import "forbid_tmp.jsonnet";
local forbid_misc = import 'forbid_misc.jsonnet';

{ rules:
    forbid_exit.rules +
    forbid_network.rules +
    forbid_exec.rules +
    forbid_tmp.rules +
    forbid_misc.rules
}
