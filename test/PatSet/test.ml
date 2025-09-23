(******************************************************************************)
(*                                                                            *)
(*                                  Bitsets                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2025--2025 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

module T =
  Setup.Make(struct
    include Bitsets.PatSet
    let name = "Bitsets.PatSet"
    (* We must choose a bound for testing. *)
    let bound = 16 * Bitsets.WordBitSet.bound
  end)
