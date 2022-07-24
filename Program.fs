module LC

open Result

type Tok =
  | LParen
  | RParen
  | Lambda
  | Dot
  | Var of char

let rec tokenize (text: char list) =
  let alphabet = List.ofSeq "abcdefghijklmnopqrstuvwxyz" in
  match text with
    | [] -> []
    | '('::rest -> LParen::tokenize rest
    | ')'::rest -> RParen::tokenize rest
    | '.'::rest -> Dot::tokenize rest
    | '\\'::rest -> Lambda::tokenize rest
    | c::rest ->
      (if List.contains c alphabet
      then [Var c]
      else []) @ tokenize rest

type Term =
  | TVar of char
  | TLambda of char*Term
  | TApp of Term*Term
  | TClosure of char*Term*Env

and Env = (char*Term) list

let rec pSingle (ts: Tok list) = 
  match ts with
  | Var c::rest -> Ok (TVar c, rest)
  | Lambda::Var arg::Dot::b ->
    Result.map (fun (body, rest) -> (TLambda (arg, body), rest)) (pSingle b) 
  | LParen::code ->
    pSingle code
    |> Result.bind (fun (fn, rest) ->
    pSingle rest
      |> Result.bind (fun (value, rest') ->
       match rest' with
        | RParen::rest'' -> Ok (TApp (fn, value), rest'')
        | _ -> Error "Expected a righ paren ')'"))
  | _ -> Error "Failed to parse."


let parse (tokens: Tok list) =
  Result.map fst <| pSingle tokens

let rec evalInEnv (env: Env) (term: Term) =
  match term with
  | TVar name -> 
    match List.tryFind (fun (name', term) -> name' = name) env with
    | Some (_, term) -> Ok term
    | None -> Error (sprintf "Could not find a term by name: %c" name)
  | TLambda (arg, body) ->
    Ok (TClosure (arg, body, env))
  | TApp (fn, value) ->
    match evalInEnv env fn with
    | Ok(TClosure (arg, body, env')) ->
      Result.bind (fun e -> evalInEnv ((arg, e)::env' @ env) body) (evalInEnv env value)
    | _ -> Error "expected a closure in function application."
  | TClosure (a, b, e) -> Ok (TClosure (a, b, e))

let eval (term: Term) =
  evalInEnv [] term
  
let rec pp (term: Term): char list =
  match term with
  | TVar x -> [x]
  | TLambda (arg, body) -> ['\\'; arg; '.'] @ pp body
  | TApp (fn, x) -> '('::pp fn @ [' '] @ pp x @ [')']
  | TClosure (arg, body, _env) -> ['\\'; arg; '.'] @ pp body

let interpret cs =
  parse (tokenize cs)
  |> Result.bind eval
  |> Result.map pp

let interpretString =
  Result.map (String.concat "" << List.map string) << interpret << List.ofSeq

let rec repl () =
  System.Console.Write "> "
  let line = System.Console.ReadLine ()
  match interpretString line with
  | Ok(t) -> System.Console.WriteLine t
  | Error(e) -> System.Console.WriteLine (sprintf "Error: %s" e)
  repl ()
  
repl ()
