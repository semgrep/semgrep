(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(**
   A plugin is an alternate function or piece of data provided by the user of
   a library, replacing the original value.

   In Semgrep, this is intended to provide proprietary implementations
   of some functionality used by the common engine.

   This module provides a generic container [t] representing the slot
   into which a plugin can be loaded.

   A plugin can only be loaded once. It cannot be unloaded, reloaded, or reset.

   See also Hook.mli for setting global values temporarily.
*)

type 'a t
(** The type of a plugin slot, the object that initially holds
    a placeholder for the plugin. *)

val create_slot : name:string -> 'a -> 'a t
(** Create a plugin slot with a placeholder or a default value.
    If a default doesn't make sense, use an option type and provide
    [None] as the default.
*)

val get : 'a t -> 'a
(** Get the current value for the plugin. This returns either the default
    placeholder or the actual plugin if it was loaded. *)

val load : 'a t -> 'a -> unit
(** Load the plugin. At most one plugin may ever be loaded into a plugin slot.
    Once loaded the plugin may not be replaced by another plugin or
    a fatal exception is raised.

    Loading the same plugin multiple times is however tolerated for the
    time being so as to allow the same initialization code to run
    multiple times which can be useful in tests sharing the same process.
**)
