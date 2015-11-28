
open Ctypes

type picosat = unit ptr
let picosat : picosat typ = ptr void

open Foreign

let init =
  foreign "picosat_init" (void @-> returning picosat)

let add =
  foreign "picosat_add" (picosat @-> int @-> returning int)

let sat psat limit =
  foreign "picosat_sat" (picosat @-> int @-> returning int)

let deref =
  foreign "picosat_deref" (picosat @-> int @-> returning int)

