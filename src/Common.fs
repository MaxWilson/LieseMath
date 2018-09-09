module Common

let r = System.Random()
let chooseRandom (lst: _ []) =
    lst.[r.Next(lst.Length)]
let delay1 f x _ = f x
