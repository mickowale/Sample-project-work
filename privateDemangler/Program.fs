// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System

exception StackIsEmpty


/// Stack implementation is used to handle nested function demangling
type Stack = { stack : string list } with 
  member this.isEmpty = List.isEmpty this.stack
  member this.add s = { stack = s::this.stack }
  member this.pop ()=  
    match this.stack with
    | [] -> raise StackIsEmpty
    | fst :: rest -> (fst, { stack = rest })

/// Check if a string is a properly mangled string
let isMangledName (s: string) = 
  if s.[0] <> '?' then false
  else true

/// Check if a mangled string is a name
let isNameLiteral (s: string) =
  if Char.IsLetter s.[0] then true
  else false

/// Form a nested representation using the :: symbol
let rec nestNames (s: Stack) = 
  let firstName, newStack = s.pop ()
  if newStack.isEmpty then firstName
  else 
    nestNames newStack + "::" + firstName


/// Display the output of the demangler on the screen
let getOutputString s result = 
  s + " : " + result
    

let demangle (s: string) =
  if not (isMangledName s) then getOutputString s "No Result"
  else 
    let literals = s.[1 ..].Split [|'@'|]
  
    let nameStack = {stack = Seq.toList (Seq.filter isNameLiteral literals)}
    
    let result = nestNames nameStack
    getOutputString s result




[<EntryPoint>]
let main argv = 
  let test = "?func1@func2@func3@func4"
  let result = demangle test

  printfn "%s" result
  Console.ReadKey () |> ignore
  0 // return an integer exit code
