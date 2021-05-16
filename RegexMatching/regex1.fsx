open System.IO


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
        target.Length = 0
    |_ ->
        match expression with 
            | CharValue(v, nex) ->
                if target.Length = 0 then false 
                else
                    let f = target.[0]
                    let mat = v.Equals f
                    let t = Array.tail target
                    let r = matchExpression(nex,t)
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
                        true
                    |_ -> false               
            | Brackets(v, nex) -> 
                let r = matchExpression(v,target)
                r
            | Line(v,left,right, nex) ->
                let l = matchExpression(left,target)
                let r = matchExpression(right,target)
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

let args = fsi.CommandLineArgs
if args.Length <> 3 then failwith "Incorrect arguments"
try 
    let exName = args.[1]
    let tarName = args.[2]
    let expressionLines = File.ReadAllLines(exName)
    let targetLines = File.ReadAllLines(tarName)
    for i = 0 to expressionLines.Length - 1 do
        let mutable expression = expressionLines.[i]
        let mutable target = targetLines.[i]
        let r = runExpression(expression, target)
        printfn "%s" r
with ex ->
    printfn "Error: Please make sure the arguments are valid files"