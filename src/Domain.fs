module Domain

#if INTERACTIVE
#load "Packrat.fs"
#endif
module Equation =
    type Number = Number of numerator: int * denominator: int option
    type Element = Constant of Number | Variable of symbolName: string * coefficient: Number
    type Equation = Equation of left: Element list * right: Element list
    let rec simplify = function
        | Number(n, Some d) when n < 0 && d < 0 -> Number(-n, Some -d) |> simplify
        | Number(n, Some d) ->
            let bound = (min (abs n) (abs d))
            let mutable n, d = n, d
            for x in 2..bound do // brute force simplification method
                while n % x = 0 && d % x = 0 && (min (abs n) (abs d)) > 1 do
                    printfn "Dividing by %d" x
                    n <- n / x
                    d <- d / x
            Number(n, Some d)
        | n -> n
    let negate = fun (Number(n,d)) -> Number(-n, d) |> simplify
    let negateElement =
        function
        | Constant c -> Constant(negate c)
        | Variable(v, n) -> Variable(v, negate n)
    let renderEquation =
        let rec renderElements isFirstTerm elements =
            let (|Positive|Negative|) = function
                | Number(n, Some d) when (n >= 0) && (d > 0) || (n < 0) && (d < 0) -> Positive
                | Number(n, None) when n >= 0 -> Positive
                | _ -> Negative
            let prefix =
                match elements with
                | (Constant Positive::_ | Variable(_, Positive)::_) when not isFirstTerm -> " + "
                | Constant Negative::_ | Variable(_, Negative)::_ -> if isFirstTerm then "-" else " - "
                | _ -> ""
            let renderNumber = function
                | Number(n, Some d) -> sprintf "%d/%d" (abs n) (abs d)
                | Number(n, None) -> (abs n).ToString()
            let renderVariable variable = function
                | Number(n, Some d) when n = d -> variable
                | Number((1 | -1), None) -> variable
                | n -> renderNumber n + variable
            match elements with
            | Variable(variable, n)::t -> prefix + (renderVariable variable n) + (renderElements false t)
            | Constant(n)::t -> prefix + (renderNumber n) + (renderElements false t)
            | [] -> ""
        function
        | Equation(lhs, rhs) -> sprintf "%s = %s" (renderElements true lhs) (renderElements true rhs)

#nowarn "40" // it's not an issue, we're not doing anything dangerous like calling code during initialization
module Parse =
    open Equation
    open Packrat
    let (|Number|_|) =
        let (|N|_|) = function
            | Int(n, Str "." (Int(digits, rest))) when digits < 10000 ->
                let frac = if digits < 10 then 10 elif digits < 100 then 100 elif digits < 1000 then 1000 else 10000
                Some(Equation.Number((n * frac) + digits, Some frac) |> Equation.simplify, rest)
            | Int(n, Str "/" (Int(d, rest))) -> Some(Equation.Number(n, Some d) |> Equation.simplify, rest)
            | Int(n, rest) -> Some(Equation.Number(n, None), rest)
            | _ -> None
        function
        | N(n, rest) -> Some(n, rest)
        | Str "-" (N(n, rest)) -> Some(negate n, rest)
        | _ -> None
    let (|Constant|_|) = function
        | Number(n, rest) -> Some(Equation.Constant(n), rest)
        | _ -> None
    let (|Variable|_|) = function
        | Number(n, (Chars alpha (variableName, rest))) -> Some(Equation.Variable(variableName, n), rest)
        | (Chars alpha (variableName, rest)) -> Some(Equation.Variable(variableName, Number (1, None)), rest)
        | _ -> None
    let (|Plus|_|) = function
        | OWS(Str "+" (OWS rest)) -> Some(rest)
        | _ -> None
    let (|Minus|_|) = function
        | OWS(Str "-" (OWS rest)) -> Some(rest)
        | _ -> None
    let (|Element|_|) = function
        | Variable(t, rest) -> Some(t, rest)
        | Constant(c, rest) -> Some(c, rest)
        | _ -> None
    let rec (|Elements|_|) = pack <| function
        | Elements(elements, Plus(Element(e, rest))) -> Some(elements@[e], rest)
        | Elements(elements, Minus(Element(e, rest))) -> Some(elements@[negateElement e], rest)
        | Element(e, rest) -> Some([e], rest)
        | _ -> None
    let (|Equation|_|) =
        function
            | OWS(Elements(lhs, OWS (Str "=" (OWS (Elements(rhs, rest)))))) -> Some(Equation.Equation(lhs, rhs), rest) | _ -> None
    let tryParse (str: string) =
        match ParseArgs.Init(str) with
        | Equation(e, End) -> Some e
        | _ -> None
