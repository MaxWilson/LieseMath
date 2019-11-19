module Domain

#if INTERACTIVE
#load "Packrat.fs"
#endif
module Equation =
    type Number = Number of numerator: int * denominator: int option
    type Element = Constant of Number | Variable of symbolName: string * coefficient: Number
    type Equation = Equation of left: Element list * right: Element list
    let rec simplify = function
        | Number(0, Some _) -> Number(0, None) |> simplify
        | Number(n, Some d) when n < 0 && d < 0 -> Number(-n, Some -d) |> simplify
        | Number(n, Some d) when d < 0 && n >= 0 -> Number(-n, Some -d) |> simplify // put negative numbers on top
        | Number(n, Some 1) -> Number(n, None) |> simplify
        | Number(n, Some d) ->
            let bound = (min (abs n) (abs d))
            let mutable n, d = n, d
            let mutable simplified = false
            for x in 2..bound do // brute force simplification method
                while n % x = 0 && d % x = 0 && (min (abs n) (abs d)) > 1 do
                    n <- n / x
                    d <- d / x
                    simplified <- true
            Number(n, Some d) |> (if simplified then simplify else id)
        | n -> n
    let negate = fun (Number(n,d)) -> Number(-n, d) |> simplify
    let negateElement =
        function
        | Constant c -> Constant(negate c)
        | Variable(v, n) -> Variable(v, negate n)
    let renderNumber = simplify >> function
        | Number(n, Some d) -> sprintf "%d/%d" n d
        | Number(n, None) -> n.ToString()
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
        let renderNumberMagnitude = function
            | Number(n, Some d) -> sprintf "%d/%d" (abs n) (abs d)
            | Number(n, None) -> (abs n).ToString()
        let renderVariable variable = function
            | Number(n, Some d) when n = d -> variable
            | Number((1 | -1), None) -> variable
            | n -> renderNumberMagnitude n + variable
        match elements with
        | Variable(variable, n)::t -> prefix + (renderVariable variable n) + (renderElements false t)
        | Constant(n)::t -> prefix + (renderNumberMagnitude n) + (renderElements false t)
        | [] -> ""
    let renderEquation = function
        | Equation(lhs, rhs) -> sprintf "%s = %s" (renderElements true lhs) (renderElements true rhs)
    let (|Denominator|) = function None -> 1 | Some v -> v
    let add lhs rhs =
        match lhs, rhs with
        | Number(n1, (None | Some 1)), Number(n2, (None | Some 1)) -> Number(n1 + n2, None)
        | Number(n1, Denominator d1), Number(n2, Denominator d2) -> Number((n1 * d2 + n2 * d1), Some(d1 * d2)) |> simplify
    let subtract lhs rhs =
        match lhs, rhs with
        | Number(n1, (None | Some 1)), Number(n2, (None | Some 1)) -> Number(n1 + n2, None)
        | Number(n1, Denominator d1), Number(n2, Denominator d2) -> Number((n1 * d2 - n2 * d1), Some(d1 * d2)) |> simplify
    let multiply lhs rhs =
        match lhs, rhs with
        | Number(n1, Denominator d1), Number(n2, Denominator d2) -> Number(n1 * n2, Some (d1 * d2)) |> simplify
    let reciprocal n =
        match n with
        | Number(n1, Denominator d1) -> Number(d1, Some n1) |> simplify
    let evaluateElements (valueLookup: string -> Number) (elements: Element list) =
        elements
        |> List.map (function
            | Constant n -> n
            | Variable(v, n) -> multiply n (valueLookup v)
            )
        |> List.reduce add

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
        | Str "-" (Chars alpha (variableName, rest)) -> Some(Equation.Variable(variableName, Number(-1, None)), rest)
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
    let tryParseNumber (str: string) =
        match ParseArgs.Init(str) with
        | Number(n, End) -> Some n
        | _ -> None

open Equation
let solveFor variableName (Equation(lhs, rhs) as original) =
    let rec containsVariable = function
        | Variable(v, _)::t when variableName = v -> true
        | _:: t -> containsVariable t
        | [] -> false
    let mutable lhs, rhs = lhs, rhs
    if(containsVariable rhs && not (containsVariable lhs)) then
        let (l, r) = (rhs, lhs)
        lhs <- l
        rhs <- r
    // consolidate variable references
    let partition lst = lst |> List.partition(function Variable(v, _) when v = variableName -> true | _ -> false)
    let addTerm term lst =
        let mutable replaced = false
        let lst' =
            lst |>
                List.fold(
                    fun acc e ->
                        match term, e with
                        | Variable(v1, n1), Variable(v2, n2) when v1 = v2 ->
                            replaced <- true
                            if n1 = (negate n2) then acc
                            else Variable(v1, Equation.add n1 n2)::acc
                        | Constant(n1), Constant(n2) ->
                            replaced <- true
                            if n1 = (negate n2) then acc
                            else Constant(Equation.add n1 n2)::acc
                        | _, v -> v::acc
                    ) [] |> List.rev
        if replaced then lst'
        else term::lst'
    let consolidate lst =
        lst |> List.fold (fun lst arg -> addTerm arg lst) []
    let mapTerm f = function
        | Variable(v,n) -> Variable(v, f n)
        | Constant(n) -> Constant(f n)
    lhs <- lhs |> consolidate
    let keep, move = lhs |> partition
    lhs <- keep
    rhs <- ((move |> List.map (mapTerm Equation.negate))@rhs) |> consolidate
    let move, keep = rhs |> partition
    rhs <- keep
    lhs <- ((move |> List.map (mapTerm Equation.negate))@lhs) |> consolidate
    match lhs with
    | Variable(v, Number(0, _))::[] ->
        // Can't simplify this case
        original
    | Variable(v, n)::[] ->
        lhs <- [Variable(v, Number(1, None))]
        rhs <- rhs |> List.map (mapTerm (Equation.multiply (reciprocal n)))
        Equation(lhs, rhs)
    | _ -> original // can't simplify this case
