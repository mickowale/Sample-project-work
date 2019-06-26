// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module rec PrivateDemangler
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
     'A' , "private: "
     'B' , "private: "
     'C' , "private: static "
     'D' , "private: static "
     'E' , "private: virtual "
     'F' , "private: virtual "
     'G' , "private: thunk "
     'H' , "private: thunk "
     'I' , "protected: "
     'J' , "protected: "
     'K' , "protected: static "
     'L' , "protected: static "
     'M' , "protected: virtual "
     'N' , "protected: virtual "
     'O' , "protected: thunk "
     'P' , "protected: thunk "
     'Q' , "public: "
     'R' , "public: "
     'S' , "public: static "
     'T' , "public: static "
     'U' , "public: virtual "
     'V' , "public: virtual "
     'W' , "public: thunk "
     'X' , "public: thunk "
     'Y' , ""
     'Z' , ""


  ] |> Map.ofList



let callConv = 
  [
     'A' , "cdecl"
     'B' , "cdecl"
     'C' , "pascal"
     'D' , "pascal"
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


let modifiers = 
  [
     'A' , ""
     'B' , "const"
     'C' , "volatile"
     'D' , "const volatile"
  ] |> Map.ofList



/// get the part for class and the rest of the list to be demangled separetly
let rec classSplit (classPart: string list) (lst: string list) = 
  match lst with 
  | [] -> List.rev classPart, []
  | head :: tail -> 
    match head.[0] with 
    | '@' -> List.rev classPart, lst
    | _ -> classSplit (head :: classPart)  tail

/// get the part for class and the rest of the list to be demangled separatly
/// This function takes care of nested classes
let rec classSplit2 (classPart: string list) (lst: string list) count =
  match lst with 
  | [] -> List.rev classPart, lst
  | head :: tail ->
    if count = 0 && (not (head.Contains "?")) then List.rev classPart, lst
    else 
      match head.[0] with 
        | '@' ->  
          if count = 1 then List.rev classPart, lst else
          if (head.Contains "?") then 
            classSplit2 (head :: classPart) tail count
          else 
          classSplit2 (head :: classPart) tail (count - 1) 
        | _ ->  
          if (head.Contains "?") then 
            classSplit2 (head :: classPart) tail (count + 1)
          else 
            classSplit2 (head :: classPart)  tail count
/// Get corresponding types from a list of letters
/// called in decodeTypes function
let rec getTypes (lst: char list)  (result: string list) : string list= 
  match lst with 
  | first :: rest -> 
    printfn "looking for a type for %c" first
    match first with 
    | 'P' -> 
      let ref,remain = rest.Head, rest.Tail
      match  getTypes remain [] with 
      | next :: other ->
        if ref = 'A' then
          List.concat [List.rev result; (next + " *") :: other] 
        else 
          List.concat [List.rev result; ((next + " const *") :: other)]
      | [] -> failwith "this should never happen"
    | '_' -> getTypes rest.Tail ((underscoredTypes.Item rest.Head) :: result)
    |  _  -> getTypes rest ((normalTypes.Item first) :: result)
  
  | [] -> List.rev result



/// Wrap the types in function parameter style string 
/// example (int,bool,...)
/// called in the assignTypes function
let makeParameters (types: string list) = 
  if types.Length = 0 then ""
  else 
    "(" + List.fold (fun s t -> s + "," + t) types.Head types.Tail + ")"

let makeClassParams (types: string list) = 
  if types.Length =  0 then ""
  else 
    "<" + List.fold (fun s t -> s + "," + t) types.Head types.Tail + ">"

/// Form the function name with all return and parameter types
/// where the first type name is the return type and the rest parameter types
let assignTypes (func: string) (callConv: string) (types: string list) = 
  if types.Length = 0 then func
  else 
    types.Head + " " + callConv + " " + func + makeParameters types.Tail

/// Form a nested representation using the :: symbol
let rec nestNames (lst: string list) =
  if List.length lst = 0 then ""
  else 
    List.fold (fun acc x -> acc + "::" + x) lst.Head lst.Tail



    

/// Update the mangled string to facilitate the split operation to follow
/// Used in the demangle function
/// Changes the first @'s of the string to " "
let rec format (s: string) = 
  if s.Length < 2 then s
  elif s.[0] = '@' then 
    if s.[1] = '@' then 
      try " " + "@" + (format s.[ 2.. ]) with 
      | _ -> raise NotProperConstruct
    else " " + format s.[ 1.. ]
  else  s.[0].ToString () + (format s.[ 1.. ])


/// This function helps decode the type string by using the calling scope
/// Gets the calling convention and modifier... appended at the end
/// Called in the decode Types function
let restTypesAndMod (scope:string) (types: string) = 
  printfn "the scope chosen is %s" scope
  match scope with
  | "" -> callConv.Item types.[2] , Seq.toList types.[3 ..], ""
  | _  -> 
    let modifier = modifiers.Item(types.[2])
    callConv.Item types.[3] , Seq.toList types.[4 ..], modifier

/// This function checks and decodes the full nested name 
/// It returns the nested name and the remaining type string
let rec getFullName (namePart: string list) (mangledPart: string list)  = 
  match mangledPart with 
  | [] -> nestNames namePart, []
  | s1 :: rest ->
    match s1.[0] with 
    | '@'-> 
      nestNames namePart, mangledPart
    | '?' -> 
      let classPart, remain = classSplit2 [] mangledPart 0
      printfn "the class part and the remaining of the class are %A and %A" classPart remain
      let className = demangle classPart
      getFullName (className :: namePart) remain
      
    | _ -> getFullName (s1 :: namePart) rest

/// This function decodes the type literall string
/// Handles function types with calling convention 
let decodeTypes (types: string list) = 
  if types.IsEmpty then "" , "" , [],""
  else
    let t1 = types.Head
    printfn "the type to be analysed is %s" t1
    if t1.[0] = '@' then 
      let scope = scopes.Item t1.[1]
      let caller, normalTypes, modifier = restTypesAndMod scope t1
      
      printfn "the normal types inferred are %A" normalTypes
      
      let realTypes = getTypes normalTypes []
      scope, caller, realTypes, modifier
    else failwith "implement"
    
  
/// Main demangling function
/// Takes the formatted string list and gives the demangled string
let demangle (literals: string list) =  
  match literals with
  | [] -> ""
  | head :: tail ->
    match head.[0] with
    | '?' ->
      match head.[1] with
      | '$' -> 
        if literals.[literals.Length - 1].[0] = '@' 
          then demangle literals.[0 .. literals.Length - 2]
        else 
          printfn "complex class trying to demangle %A" literals
          let typeChars = Seq.toList tail.Head
          let types = getTypes typeChars []
          let fullName = nestNames (List.rev (head.[2..] :: tail.Tail))

          fullName + makeClassParams types
          
        
    | _ ->
      let namePart, typePart = getFullName [] literals
      printfn "the name decoded and the typePart are %s and %A" namePart typePart
      let scope,callConv,types, modifier = decodeTypes typePart
      
      printfn "the scope is %s" scope
      printfn "the callConv is %A" callConv
      printfn "the types are %A" types
      
      scope + (assignTypes namePart callConv types) + modifier

/// Initiates the demangle process
/// Calls the format function function first
let start (str: string) = 
  if str.[0] <> '?' then "not mangled name"
  else 
    let formatted = format str.[1 ..]
    let literals = Seq.toList (formatted.Split [|' '|])
    printfn "the literals passed are %A" literals
    demangle (literals)









/// Display the output of the demangler on the screen
let getOutputString s result = 
  s + " : " + result

/// Test runs
[<EntryPoint>]
let main argv = 
  let test = "??$def@H@in@this@@"
  let result = start test

  printfn "%s" result
  Console.ReadKey () |> ignore
  0 // return an integer exit code
