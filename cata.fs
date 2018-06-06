// type AstroPath<'a> =
//     | Item of Map<string,'a>
//     | PathPart of Map<string,Patate<'a>>

// let buildPath (path:string) value = 
//     let rec buildPath' = function
//     | (None,x::xs) -> ([(x,value)] |> Map.ofList |> Item |> Some,xs) |> buildPath'
//     | (Some pp,x::xs) -> ([(x,pp)] |> Map.ofList |> PathPart |> Some,xs) |> buildPath'
//     | (Some pp,[]) -> pp

//     (None, path.Split '.' |> Array.rev |> List.ofArray) |> buildPath'
type AstroPath<'a> =
    | Empty
    | Item of string*'a
    | PathPart of string*AstroPath<'a>

let buildPath2 (path:string) value = 
    let foldPath pp ap = 
        match ap with
        | Empty -> Item (pp,value)
        | ap -> PathPart (pp,ap)
    
    (path.Split '.') |> Array.foldBack foldPath <| Empty

let rec json = function
| Empty -> "{}"
| Item (k,v) -> sprintf "{ '%s': '%A' }" k v
| PathPart (k,pp) -> sprintf "{ '%s': %s }" k (json pp)

let rec xml = function
| Empty -> ""
| Item (k,v) -> sprintf "<value name=\"%s\">%A</value>" k v
| PathPart (k,pp) -> sprintf "<path name=\"%s\">%s</path>" k (xml pp)

let rec cataAstroPath fEmpty fItem fPathPart = function
| Empty -> 
    fEmpty()
| Item (pp,value) -> 
    fItem pp value
| PathPart (pp,ap) ->
    let innerAstroPathResult = cataAstroPath fEmpty fItem fPathPart ap
    fPathPart pp innerAstroPathResult

let jsonUsingCata pp =
    let emptyJson() = "{}"
    let itemJson pp v = sprintf "{ '%s': '%A' }" pp v
    let pathPartJson pp innerJson = sprintf "{ '%s': %s }" pp innerJson

    cataAstroPath emptyJson itemJson pathPartJson pp

let rec foldAstroPath fEmpty fItem fPathPart acc = function
| Empty -> 
    fEmpty acc
| Item (pp,value) -> 
    fItem acc pp value
| PathPart (pp,ap) -> 
    let newAcc = fPathPart acc pp
    foldAstroPath fEmpty fItem fPathPart newAcc ap

(* https://fsharpforfunandprofit.com/posts/recursive-types-and-folds-2/#rules-for-creating-a-fold *)

let jsonUsingFold ap =
    let emptyJson _ = "{}"
    let itemJson acc pp v = acc + (sprintf "{ '%s': '%A' }" pp v)
    let pathPartJson acc pp = sprintf "{ '%s': %s }" pp acc

    foldAstroPath emptyJson itemJson pathPartJson "" ap

let jsonUsingFold2 ap =
    let emptyJson generator = generator "{}"
    let itemJson generator pp v = generator (sprintf "{ '%s': '%A' }" pp v)
    let pathPartJson generator pp = 
        let newGenerator innerText =
            let newInnerText = sprintf "{ '%s': %s }" pp innerText 
            generator newInnerText 
        newGenerator 

    foldAstroPath emptyJson itemJson pathPartJson (fun s -> "("+s+")") ap

let rec foldBackAstroPath fEmpty fItem fPathPart ap generator =
    match ap with
    | Empty -> 
        generator (fEmpty ())
    | Item (pp,value) -> 
        generator (fItem pp value)
    | PathPart (pp,ap) -> 
        let newGenerator innerVal =
            let newInnerVal = fPathPart pp innerVal 
            generator newInnerVal
        foldBackAstroPath fEmpty fItem fPathPart ap newGenerator

let jsonUsingFoldBack pp =
    let emptyJson() = "{}"
    let itemJson pp v = sprintf "{ '%s': '%A' }" pp v
    let pathPartJson pp innerJson = sprintf "{ '%s': %s }" pp innerJson

    foldBackAstroPath emptyJson itemJson pathPartJson pp id

(* https://fsharpforfunandprofit.com/posts/recursive-types-and-folds-2b/#iteration-vs-recursion *)




