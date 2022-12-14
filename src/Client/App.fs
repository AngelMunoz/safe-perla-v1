module App

open Elmish
open Sutil


Program.mountElement "elmish-app" (Index.view ())