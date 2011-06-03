module Tests

open Xunit
open MDelimcc

let cc = ContinuationBuilder()

[<Fact>]
let test2() =
    let k = 
        cc {
            let! p = newP()
            let! pp = pushP p (pushP p (cc { return 5 }))
            return 4 + pp
        }
    Assert.Equal(9, runP k)

[<Fact>]
let test3() =
    let k = 
        cc {
            let! p = newP()
            let! v = pushP p (cc {
                let! v = abortP p (cc { return 5 })
                return 6 + v
            })
            return 4 + v
        }
    Assert.Equal(9, runP k)

[<Fact>]
let test5() =
    let k = 
        cc {
            let! p0 = newP()
            let! p1 = newP() // not really used
            let! v = pushP p0 (cc {
                let! v = shiftP p0 (fun sk -> cc {
                    let! v = sk (sk (cc { printfn "ret 3"; return 3 }))
                    printfn "%d + 100" v
                    return v + 100
                })
                printfn "%d + 2" v
                return v + 2
            })
            printfn "%d + 10" v
            return v + 10
        } // 3 + 2 + 2 + 100 + 10
    Assert.Equal(117, runP k)
    ()

[<Fact>]
let ``test5'``() =
    let k = 
        cc {
            let! p0 = newP()
            let! p1 = newP() // not really used
            let! v = pushP p0 (cc {
                let! v = shiftP p0 (fun sk -> cc {
                    let! v = sk (cc { printfn "ret 3"; return 3 })
                    printfn "%d + 100" v
                    return v + 100
                })
                printfn "%d + 2" v
                return v + 2
            })
            printfn "%d + 10" v
            return v + 10
        } // 3 + 2 + 100 + 10
    Assert.Equal(115, runP k)

[<Fact>]
let ``test5''``() =
    let k =
        cc {
            let! p0 = newP()
            let! p1 = newP()
            let! v = pushP p0 (cc {
                let! v = shiftP p0 (fun sk -> cc {
                    let! v = sk (pushP p1 (sk (abortP p1 (cc { printfn "ret 3"; return 3 }))))
                    printfn "%d + 100" v
                    return v + 100
                })
                printfn "%d + 2" v
                return v + 2
            })
            printfn "%d + 10" v
            return v + 10
        } // 3 + 2 + 100 + 10
    Assert.Equal(115, runP k)

[<Fact>]
let test6() =
    let k = 
        cc {
            let! p1 = newP()
            let! p2 = newP()
            let pushtwice sk = sk (sk (cc { printfn "ret 3"; return 3 }))
            let! v = pushP p1 (pushP p2 (cc {
                let! v = shiftP p1 pushtwice
                printfn "%d + 1" v
                return v + 1
            }))
            printfn "%d + 10" v
            return v + 10
        } // 3 + 1 + 1 + 10 (3 in p2 discarded)
    Assert.Equal(15, runP k)

[<Fact>]
let test7() =
    let k =
        cc {
            let! p1 = newP()
            let! p2 = newP()
            let! p3 = newP()
            let pushtwice sk = sk (sk (cc { printfn "ret 3"; return 3 }))
            let pushtwice sk = sk (sk (shiftP p2 pushtwice))
            let! v = pushP p1 (cc {
                let! v = pushP p2 (cc { 
                    let! v = pushP p3 (shiftP p1 pushtwice)
                    printfn "%d + 10" v
                    return v + 10 })
                printfn "%d + 1" v
                return v + 1
            })
            printfn "%d + 100" v
            return v + 100
        } // 3 + 10 + 10 + 1 + 10 + 1 + 100
    Assert.Equal(135, runP k)

open Microsoft.FSharp.Collections

//[<Fact>]
let test7parallel() =
    {0..10000000} |> PSeq.iter (ignore >> test7)

