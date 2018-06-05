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


