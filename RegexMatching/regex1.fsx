open System.IO

exception SYNTAX_ERROR of string

// type EType = 
//     | character = 0
//     | dot = 1
//     | brackets = 2
//     | other = 3

// type Element = {Type: EType; mutable repeat: bool; mutable isopen: bool; elements: Element[][]; depth: int; value : char}

// let mutable depth = 0
// let mutable current = 0
// let mutable baseE: Element array = Array.empty
// let mutable targetChars = Array.empty

// let rec closeBrackets (e: Element, depth: int, repeat:bool) =
//     if e.Type.Equals EType.brackets then
//         if e.depth.Equals depth then
//             e.isopen <- false
//             e.repeat <- repeat
//         else 
//             let lastValue = e.elements.[0].[e.elements.[0].Length - 1]
//             closeBrackets(lastValue,depth,repeat)
//     else 
//         let inArray = e.elements.[e.elements.Length-1]
//         closeBrackets(inArray.[inArray.Length-1], depth,repeat)
       
// let isNested: bool = 
//     if baseE.Length > 0 then baseE.[baseE.Length-1].isopen else false

// let addOther(e:Element) =
//     printfn "TODO??"

// // // let run (expression, target) = null

// let rec addElement (outerElement: Element, e: Element) =
//     if e.Type.Equals EType.brackets then
//         if outerElement.elements.[0].[outerElement.elements.[0].Length - 1].isopen then 
//             addElement(outerElement.elements.[0].[outerElement.elements.[0].Length - 1],e)
//         else
//             outerElement.elements.[0].[outerElement.elements.[0].Length] <- e
//     else 
//         let lastValue = outerElement.elements.[outerElement.elements.[0].Length - 1]
//         if lastValue.[lastValue.Length - 1].isopen then
//             addElement(lastValue.[lastValue.Length - 1],e)
//         else
//             lastValue.[lastValue.Length] <- e

// let evaluateDot (e : Element, characters: char[]) : bool =
//     if targetChars.Length = 0  then false
//     else 
//         targetChars <- Array.tail targetChars
//         true
        
// let evaluateChar (e : Element, characters: char[]) : bool =
//     if targetChars.Length = 0  then false
//     else 
//         if e.repeat then
//             while targetChars.[0].Equals e.value do 
//                 targetChars <- Array.tail targetChars
//             true 
//         else if targetChars.[0].Equals e.value then 
//             targetChars <- Array.tail targetChars
//             true
            
//         else 
//             false  

// let evaluateBrackets (e : Element, characters: char[]) : bool =
//     false

// let evaluateOther (e : Element, characters: char[]) : bool =
//     false

// let compareTarget (target : string) : string =
//     let mutable targetC = target.ToCharArray()
//     let comparing = Array.forall(fun (e : Element) ->
//         match e.Type with
//             | EType.dot -> evaluateDot(e,targetC)
//             | EType.character -> evaluateChar(e,targetC)
//             | EType.brackets -> evaluateOther(e,targetC)
//             | EType.other -> evaluateOther(e,targetC)
//             | _ -> false
//     )
//     if comparing baseE && Array.isEmpty targetChars then
//         "YES"
//     else
//         "NO"       


// let createElement (c : char, repeat: bool) : Option<Element> =
//     match c with 
//         | '.' -> Some {Type = EType.dot; repeat = repeat; isopen = false; elements = null; depth = depth; value = c}
//         | '|' ->
//             if isNested then 
//                 addOther(baseE.[baseE.Length])
//                 None
//             else
//                 let otherE: Element[][] = Array.empty
//                 let prev: Element array = Array.copy baseE: Element[]
//                 otherE.[0] <- prev
//                 otherE.[1] <- Array.empty
//                 baseE <- Array.empty
//                 Some {Type = EType.other; repeat = repeat; isopen = true; elements = otherE; depth = depth; value = c}
//         | '(' ->
//             if repeat then raise (SYNTAX_ERROR("SYNTAX ERROR"))
//             depth <- depth + 1
//             let mutable em = Array.zeroCreate<Element> 2
//             let r = em.[0]
//             let e = {Type = EType.dot; repeat = repeat; isopen = false; elements = null; depth = depth; value = c}
//             em <- Array.append em [|e|]
//             printfn "G"
//             let bracketsE: Element[][] = Array2D.zeroCreate<Element> 2 1
//             bracketsE.[0] <- Array.empty
//             printfn "H"
//             Some {Type = EType.brackets; repeat = repeat; isopen = true; elements = bracketsE; depth = depth; value = c}
//         | ')' ->
//             printfn "%A" baseE
//             if depth.Equals 0 then raise (SYNTAX_ERROR("SYNTAX ERROR"))
//             closeBrackets(baseE.[baseE.Length-1], depth, repeat)
//             depth <- depth - 1
//             None
//         |_ -> 
//             Some {Type = EType.character; repeat = repeat; isopen = false; elements = null; depth = depth; value = c}

// let readExpression (characters : char[]) =
//     let char = characters.[current]
//     let next = if current < characters.Length-1 then characters.[current+1] else ' '
//     current <- current + 1

//     if char.Equals '*' then raise (SYNTAX_ERROR("SYNTAX ERROR"))
//     let mutable repeat = false
//     if next.Equals '*' then
//         if char.Equals '|' then raise (SYNTAX_ERROR("SYNTAX ERROR"))
//         repeat <- true
//         current <- current + 1
//     let e = createElement(char, repeat)
//     if not e.IsNone then    
//         if isNested then    
//             addElement(baseE.[baseE.Length-1], e.Value)
//         else 
//             baseE <- Array.append baseE [|e.Value|]

type ExpressionTree =
    | CharValue of char * ExpressionTree
    | Line of char * ExpressionTree * ExpressionTree * ExpressionTree
    | Dot of char * ExpressionTree
    | Star of char * ExpressionTree * ExpressionTree
    | Brackets of ExpressionTree * ExpressionTree
    | Syn of char
    | End of string 


let rec parseExpression (expression : char[]) : ExpressionTree = 
    let conOr = Array.contains '|' expression
    if conOr then 
        let ind = Array.findIndex(fun x -> x = '|') expression
        let leftEx = Array.sub expression 0 ind 
        let counter = expression.Length - ind - 1
        let rightInd = ind + 1
        let rightEx = Array.sub expression rightInd counter
        let leftParsed = parseExpression(leftEx)
        let rightParsed = parseExpression(rightEx)
        let e = End("end")
        Line('|',leftParsed,rightParsed,e)
    else
        if expression.Length = 0 then 
            End("end")
        else 
            let c = expression.[0]
            if expression.Length = 1 then 
                match c with
                    | '.' ->
                        let t = Array.tail expression
                        let n = parseExpression(t)
                        Dot(c,n)
                    | ')' -> 
                        End("end")
                    |_ ->
                        let t = Array.tail expression
                        let n = parseExpression(t)
                        CharValue(c,n)
            else
                let sec = expression.[1]
                match sec with
                    | '|' ->
                        let s = c.ToString()
                        let ca = s.ToCharArray()
                        let l = parseExpression(ca)
                        let half = Array.tail expression
                        let t = Array.tail half
                        let r = parseExpression(t)
                        let e = End("end")
                        Line(c,l,r,e)
                    | '*' -> 
                        let s = c.ToString()
                        let ca = s.ToCharArray()
                        let l = parseExpression(ca)
                        let half = Array.tail expression
                        let t = Array.tail half
                        let n = parseExpression(t)
                        //let e = End("end")
                        Star(c,l,n)
                    |_ -> 
                        match c with
                            | '.' ->
                                let t = Array.tail expression
                                let n = parseExpression(t)
                                Dot(c,n)
                            | '|'->
                                let t = Array.tail expression
                                let n = parseExpression(t)
                                let e = End("end")
                                Line(c,e,n,e)
                            | '(' ->
                                let t = Array.tail expression
                                let n = parseExpression(t)
                                let e = End("end")
                                Brackets(n,e)
                            | ')' -> 
                                let t = Array.tail expression
                                let n = parseExpression(t)
                                n
                            |_ ->
                                let t = Array.tail expression
                                let n = parseExpression(t)
                                CharValue(c,n)

let rec matchExpression (expression: ExpressionTree, target : char[]) : bool =
    match expression with
    | End(v) -> 
        // printfn "End"
        //if target.Length = 0 then printfn "true" else printfn "false" 
        target.Length = 0
    |_ ->
        match expression with 
            | CharValue(v, nex) ->
                if target.Length = 0 then false 
                else
                    let f = target.[0]
                    // printfn "%c" v
                    // printfn "%c" f
                    let mat = v.Equals f
                    let t = Array.tail target
                    let r = matchExpression(nex,t)
                    // if r then printfn "true" else printfn "false" 
                    mat && r
            | Dot(v, nex) -> 
                if target.Length = 0 then false 
                else
                    let t = Array.tail target
                    let r = matchExpression(nex,t)
                    r
            | Star(v,ex, nex) -> 
                if target.Length = 0 then true 
                else 
                    
                    let mutable t = target
                    match ex with 
                    | CharValue(cv,cnext) -> 
                        while t.Length > 0 && t.[0]. Equals cv do 
                            t <- Array.tail t  
                        let r = matchExpression(nex,t)  
                        r
                    | Dot(cv,cnext) -> 
                        // while t.Length > 0 && t.[0]. Equals cv do 
                        //     t <- Array.tail t  
                        // let r = matchExpression(nex,t)  
                        // r
                        true
                    |_ -> false               
            | Brackets(v, nex) -> 
                // if target.Length = 0 then false
                // else
                //     //printfn "Brace"
                let r = matchExpression(v,target)
                r
            | Line(v,left,right, nex) ->
                let l = matchExpression(left,target)
                let r = matchExpression(right,target)
                // "Line"
                l || r
            | Syn(v) -> false
            | End(v) -> true
 
let countBrackets(expression : char[]) : bool =
    let mutable numBrack = 0 
    for i = 0 to expression.Length - 1 do
        let c = expression.[i]
        if c.Equals '(' then
            numBrack <- numBrack + 1
        else if c.Equals ')' then 
            numBrack <- numBrack - 1
        else 
            numBrack <- numBrack
    numBrack <> 0

let checkStars (expression : char[]) : bool =
    let mutable stars = false
    for i = 1 to expression.Length - 1 do
        let c = expression.[i]
        let p = expression.[i - 1]
        if c.Equals '*' then
            if p.Equals '|' || p.Equals '(' then
                stars <- true
            else 
                stars <- stars
        else 
                stars <- stars
    stars

 
let findSyntaxErrors(expression : char[]) : bool = 
    let numbrack = countBrackets(expression) 
    let f = expression.[0]
    let firstStar = f.Equals '*'
    let stars = checkStars(expression)
    numbrack || firstStar || stars

let runExpression (expression : string,target : string ) : string = 
    let mutable characters = expression.ToCharArray()
    let mutable targetchars = target.ToCharArray()
    let syntaxErrors = findSyntaxErrors(characters)
    if syntaxErrors then "SYNTAX ERROR" 
    else
        let exTree = parseExpression(characters)
        let res = matchExpression(exTree,targetchars)
        if res then
            "YES"
        else 
            "NO"

printfn "Testing"
let args = fsi.CommandLineArgs
if args.Length <> 3 then failwith "Incorrect arguments"
// printfn "%A" args
// printfn "%s" args.[1]
try 
    let exName = args.[1]
    let tarName = args.[2]
    let expressionLines = File.ReadAllLines(exName)
    let targetLines = File.ReadAllLines(tarName)
    // printfn "%s" expressionLines.[1]
    // printfn "%i" targetLines.Length
    for i = 0 to expressionLines.Length - 1 do
        //printfn "Expression: %s Target: %s" expressionLines.[i] targetLines.[i]
        // current <- 0
        // depth <- 0
        // baseE <- Array.empty
        let mutable expression = expressionLines.[i]
        let mutable target = targetLines.[i]
        // targetChars <- target.ToCharArray()
        let r = runExpression(expression, target)
        printfn "%s" r
with ex ->
    printfn "Error: Please make sure the arguments are valid files"