// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System

exception StackIsEmpty
exception NotProperConstruct
/// A Map of all the normal types where types that begin with 
/// + -> type modifiers
/// # -> complex types
let normalTypes = 
  [
    'A' , "+Reference";
    'B' , "+Volatile Reference";
    'C' , "signed char";
    'D' , "char";
    'E' , "unsigned char";
    'F' , "short";
    'G' , "unsigned short";
    'H' , "int";
    'I' , "unsigned int";
    'J' , "long";
    'K' , "unsigned long";
    'L' , "---------";
    'M' , "float";
    'N' , "double";
    'O' , "long double";
    'P' , "+Pointer";
    'Q' , "+const Pointer";
    'R' , "+volatile Pointer";
    'S' , "+const volatile pointer";
    'T' , "#union";
    'U' , "#struct";
    'V' , "#class";
    'W' , "#enum";
    'X' , "void";
    'Y' , "#cointerface";
    'Z' , "..."
  ] |> Map.ofList

let underscoredTypes = 
  [
    'D' , "__int8";
    'E' , "unsigned__int8";
    'F' , "__int16";
    'G' , "unsigned __int16";
    'H' , "__int32";
    'I' , "unsigned__int32";
    'J' , "__int64";
    'K' , "unsigned__int64";
    'L' , "__int128";
    'M' , "unsigned__int128";
    'N' , "bool";
    'O' , "Array";

    'S' , "char16_t";
    'U' , "char32_t";
    'W' , "wchar_t";
    'X' , "#coclass";
    'Z' , "#cointerface";


  ] |> Map.ofList


let scopes = 
  [
     'A' , "private:"
     'B' , "private:"
     'C' , "private: static"
     'D' , "private: static"
     'E' , "private: virtual"
     'F' , "private: virtual"
     'G' , "private: thunk"
     'H' , "private: thunk"
     'I' , "protected:"
     'J' , "protected:"
     'K' , "protected: static"
     'L' , "protected: static"
     'M' , "protected: virtual"
     'N' , "protected: virtual"
     'O' , "protected: thunk"
     'P' , "protected: thunk"
     'Q' , "public:"
     'R' , "public:"
     'S' , "public: static"
     'T' , "public: static"
     'U' , "public: virtual"
     'V' , "public: virtual"
     'W' , "public: thunk"
     'X' , "public: thunk"
     'Y' , ""
     'Z' , ""


  ] |> Map.ofList



let callConv = 
  [
     'A' , "cdecl"
     'B' , "private:"
     'C' , "pascal"
     'D' , "private: static"
     'E' , "thiscall"
     'F' , "thiscall"
     'G' , "stdcall"
     'H' , "stdcall"
     'I' , "fastcall"
     'J' , "fastcall"
     'K' , ""
     'L' , ""
     'M' , "clrcall"
  ] |> Map.ofList
/// Check if a type needs further dealing with
/// Check if it is Not implemented yet
let isComplexType (typeString: string) = 
  if List.contains typeString.[0] ['#' ; '+' ] then true
  else false

/// Check if a type is a normal type and needs no further decoding
let isNormalType (typeString: string) =
  if isComplexType typeString then false
  else true

/// Stack implementation is used to handle nested function demangling
type Stack = { stack : string list } with 
  member this.isEmpty = List.isEmpty this.stack
  member this.print = printfn "%A" this.stack
  member this.asList = this.stack
  member this.add s = { stack = s::this.stack }
  member this.pop () =  
    match this.stack with
    | [] -> raise StackIsEmpty
    | fst :: rest -> (fst, { stack = rest })

/// Check if a string is a visual c++ mangled string
let isMangledString (s: string) = 
  if s.[0] <> '?' then false
  else true

/// Check if the literal has is a special name
let isSpecialName (s: string) = 
  if s.[0] <> '?' then true
  else false

/// Check if a mangled string is a name
let isNameLiteral (s: string) =
  if  s.Length > 0 &&  Char.IsLetter s.[0] then true
  else false

/// Check if a literal is a type identifier
let isTypeLiteral (s: string) =
  if s.Length > 0 && s.[0] = '@' then true
  else false

/// Get corresponding types from a list of letters
/// called in inferTypes function
let rec getTypes (lst: char list)  (result: string list) = 
  match lst with 
  | first :: rest -> 
    if Char.IsLetterOrDigit first then 
      getTypes rest ((normalTypes.Item first)::result)
    elif first = '_' then 
      getTypes rest.Tail ((underscoredTypes.Item rest.Head)::result)
    else []
  
  | [] -> List.rev result



/// Infer the types from the type identifier string
/// Returns a list of type strings
/// Called in the demangle function
let inferTypes (s:string) =
  let charList = Seq.toList s.[1..]
  printfn "the characters are %A" charList
  getTypes charList []

/// Wrap the types in function parameter style string 
/// example (int,bool,...)
/// called in the assignTypes function
let makeParameters (types: string list) = 
  if types.Length = 0 then ""
  else 
    "(" + List.fold (fun s t -> s + "," + t) types.Head types.Tail + ")"

/// Form the function name with all return and parameter types
/// where the first type name is the return type and the rest parameter types
let assignTypes (func: string) (callConv: string) (types: string list) = 
  if types.Length = 0 then func
  else 
    types.Head + "  " + callConv + " " + func + makeParameters types.Tail

/// Form a nested representation using the :: symbol
let rec nestNames (lst: string list) =
  if List.length lst = 0 then ""
  else 
    List.foldBack (fun x acc -> acc + "::" + x) lst.Tail lst.Head


/// Display the output of the demangler on the screen
let getOutputString s result = 
  s + " : " + result
    

/// Update the mangled string to facilitate the split operation to follow
/// Used in the demangle function
let rec format (s: string) = 
  if s.Length < 2 then s
  elif s.[0] = '@' then 
    if s.[1] = '@' then 
      try " " + "@" + (format s.[ 2.. ]) with 
      | IndexOutOfRangeException -> raise NotProperConstruct
    else " " + format s.[ 1.. ]
  else  s.[0].ToString () + (format s.[ 1.. ])


/// Main demangler function
let demangle (s: string) =
  if not (isMangledString s) then getOutputString s "No Result"
  else 
    let formatted = format s
    let literals = formatted.[1 ..].Split [|' '|]
  
    let nameStack = {stack = Seq.toList (Seq.filter isNameLiteral literals)}
    let typeStack = {stack = Seq.toList (Seq.filter isTypeLiteral literals)}

    nameStack.print
    typeStack.print
    
    let funcName = nestNames nameStack.stack
    let fst,rStack = typeStack.pop () 

    printfn "%A" fst
    let types1 = inferTypes fst
    printfn "The types are %A" types1

    //let result = assignTypes funcName (List.filter isNormalType  types1)
    //getOutputString s result
    ""





let rec getFullName (namePart: string list) (mangledPart: string list)  = 
  match mangledPart with 
  | [] -> nestNames namePart, []
  | s1 :: rest ->
    match s1.[0] with 
    | '@'-> nestNames namePart, mangledPart
    | '?' -> failwith "unimplemented"
    | _ -> getFullName (s1 :: namePart) rest


let decodeTypes (types: string list) = 
  if types.IsEmpty then "" , "" , []
  else
    let t1 = types.Head
    printfn "the type to be analysed is %s" t1
    if t1.[0] = '@' then 
      let scope = scopes.Item t1.[1]
      let caller = callConv.Item t1.[2]
      let normalTypes = Seq.toList (t1.ToCharArray(3, t1.Length-3))
      
      printfn "the normal types inferred are %A" normalTypes
      
      let realTypes = getTypes normalTypes []
      scope, caller, realTypes
    else "","",[]
    
  

let demangle2 (str: string) = 
  if str.Length = 0 then ""
  else 
    match str.[0] with
    | '?' -> ""
    | _ ->
      let formatted = format str
      let literals = Seq.toList (formatted.Split [|' '|])
     
      printfn "the literals are %A" literals

      let namePart,typePart = getFullName [] literals

      printfn "the name decoded and the typePart are %s and %A" namePart typePart
      let scope,callConv,types = decodeTypes typePart
      
      printfn "the scope is %s" scope
      printfn "the callConv is %A" callConv
      printfn "the types are %A" types
      
      scope + (assignTypes namePart callConv types)
      
let start (str: string) = 
  if str.[0] <> '?' then "not mangled name"
  else demangle2 (str.[1 ..])

/// Test runs
[<EntryPoint>]
let main argv = 
  let test = "?something@@YAXH_N@Z"
  let result = start test

  printfn "%s" result
  Console.ReadKey () |> ignore
  0 // return an integer exit code
