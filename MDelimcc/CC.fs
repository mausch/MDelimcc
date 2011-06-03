module MDelimcc

// monadic delimited continuations
// from http://okmij.org/ftp/continuations/cc-monad.ml

type 'a promptFP = PromptFP of (bool * (unit -> 'a)) ref
type internal 'a cc1 = ('a -> unit) -> unit
type internal hfp = 
    | HFP of (unit cc1 -> hfp cc1) * ((unit cc1 -> unit cc1) -> unit cc1)
    | HVFP of unit
type internal promptF0 = hfp ref

let internal the_p0 = ref (HVFP ())(* Just avoid the Reader monad and use one P0 *)

let internal (>>=) m f = fun k -> m (fun a -> f a k)

type ContinuationBuilder() = 
    member x.Return a = fun k -> k a
    member x.ReturnFrom a = a
    member x.Bind(m,f) = m >>= f
    member x.Delay f = f()

let internal ccc = ContinuationBuilder()

(*
    member x.CallCC f = 
        fun k -> id <| (f (fun a _ -> k a)) k
*)

let reset e = fun k -> k (e id)
let shift e = fun k -> (e (fun v -> fun c -> c (k v))) id

(* reading and writing of references lifted to the Cont monad *)
let internal lget p = fun k -> k !p
let internal lset p v = fun k -> (p := v; k ())
let internal lnewref a = fun k -> k (ref a)
let map f m = fun k -> m (fun v -> k (f v))

(* Our prompts and their primitive operations *)

let internal setprompt (PromptFP p) v = lset p (false, fun () -> v)
let internal getprompt (PromptFP p) = map (fun x -> snd x ()) (lget p)
let internal checkprompt (PromptFP p) = 
    ccc {
        let! mark,v = lget p
        if mark
            then 
                do! lset p (false,v)
                return true
            else
                return false
    }

let internal setmark (PromptFP p) =
    ccc {
        let! mark,v = lget p
        do! lset p (true,v)
    }

(* Object-level single-prompt shift-reset with enough polymorphism *)
let internal oReset (p0:promptF0) (m: hfp cc1) =
    let r = 
        ccc {
            let! z = m
            do! lset p0 z
        }
    ccc {
        do! reset r
        return! lget p0
    }

let internal oShift (p0:promptF0) f =
    ccc {
        let! v = shift (fun g -> 
                            ccc {
                                let! x = f (fun v -> 
                                                ccc {
                                                    do! g v
                                                    return! lget p0
                                                })
                                do! lset p0 x
                            })
        return! v
    }

let newP() =
    ccc {
        let! p = lnewref (false, fun () -> failwith "undefined")
        return PromptFP p
    }
        
let rec internal pushPFP' p0 p (m: unit cc1) : unit cc1 =
        hrStopF p0 p 
            (oReset p0 (ccc {
                            let! v = m
                            return HVFP v
                        }))
    and internal hrStopF p0 p (m: hfp cc1) = m >>= hrStopF' p0 p
    and internal hrStopF' p0 p: hfp -> unit cc1 = 
        function 
        | HVFP v -> ccc.Return v 
        | HFP (f, c) ->
            let hrf = f >> hrStopF p0 p
            let handle = pushPFP' p0 p (c hrf)
            let relay = 
                let ss g = ccc.Return (HFP (hrf >> g, c))
                oShift p0 ss
            checkprompt p >>= fun v -> if v then handle else relay

let internal shiftFP p0 p (f: (('b cc1) -> 'a cc1) -> 'a cc1) : 'b cc1 =
    ccc {
        let! ans = lnewref (fun () -> failwith "undefined")
        let f' k =
            let k1 m =
                ccc {
                    do! k (ccc {
                        let! v = m
                        do! lset ans (fun () -> v)
                    })
                    return! getprompt p
                }
            ccc {
                let! v = f k1
                do! setprompt p v
            }
        do! setmark p
        do! oShift p0 (fun k -> ccc { return HFP (k, f') })
        return! (ccc {
            let! vc = lget ans
            return vc()
        })
    }

let pushP p (m: 'a cc1) : 'a cc1 = 
    ccc {
        do! pushPFP' the_p0 p (ccc {
            let! v = m
            do! setprompt p v
        })
        return! getprompt p
    }

let shiftP p f = shiftFP the_p0 p f

let abortP p e = shiftP p (fun _ -> e)

let runP (m: 'a cc1) : 'a =
    let ans = ref (fun () -> failwith "no prompt set")
    m (fun v -> ans := (fun () -> v))
    !ans ()
            