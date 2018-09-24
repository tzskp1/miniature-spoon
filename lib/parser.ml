open Base
open Base.Fn   

type ty = String.elt List.t
type fail = Fail 
type eof = Eof
type 'a parser = Parser of (ty -> ('a, fail) Either.t * ty)
                         
let fail_equal f1 f2 =
  match f1,f2 with
  | Fail, Fail -> true
    
let section x = Parser (fun y -> Either.First x, y)

let map ~f =
  function Parser p ->
    Parser
      begin fun x ->
      let _1, _2 = p x in
      Either.map _1 ~first:f ~second:id, _2
      end

let join : 'a parser parser -> 'a parser = 
  function Parser pp ->
    Parser
      begin fun x ->
      let _1, _2 = pp x in
      match _1 with
      | First Parser p -> p _2
      | Second f -> Either.Second f, _2
      end
           
let bind ~f p = join (map ~f:f p)
             
let app (p : 'a parser) ~(f : ('a -> 'b) parser) : 'b parser =
  bind ~f:(fun f -> map ~f:f p) f
    
let orP =
  function Parser p1 ->
    fun p2L -> 
      Parser
        begin fun x ->
        match p1 x with
        | First _, _ as y -> y
        | Second _, _ ->
           let lazy (Parser p2) = p2L in p2 x
        end

let tryP = function Parser p ->
             Parser
               begin fun src ->
               match p src with
               | Second f, _ -> Second f, src
               | orig -> orig
               end
                    
let checkP = function Parser p ->
               Parser
                 begin fun src ->
                 match p src with
                 | Second f, _ -> Second f, src
                 | f, _ -> f, src
                 end

let failP = function Parser p ->
              Parser
                begin fun src ->
                match p src with
                | First _, _ -> Second Fail, src
                | Second _, src' -> First src', src
                end

let repeat =
  function Parser p ->
    let rec iter acc x =
      match p x with
      | Second _, _ -> Either.First acc, x
      | First res, src' -> iter (res :: acc) src'
    in Parser (iter [])

let chainL1 (p : 'a parser) (op : ('a -> 'a -> 'a) parser) =
  let op' = repeat (app p ~f:op)
            |> map ~f:(List.fold_right ~init:id ~f:compose) in
  app p ~f:op'

let satisfy p =
  Parser
    begin function
    | x :: xs when p x -> Either.First x, xs
    | xs -> Either.Second Fail, xs
    end
  
let eof =
  Parser
    begin function
    | _ :: _ as xs -> Either.Second Fail, xs
    | [] -> Either.First Eof, []
    end
  
let any = satisfy (fun _ -> true)
let charP c = satisfy (Char.equal c)
let spaces = charP ' ' |> repeat
let oneOf str = satisfy (String.contains str)
let digit = oneOf "0123456789"
let consP p ps =
  app ps ~f:(map ~f:(fun x y -> x :: y) p)
let repeat1 p = consP p (repeat p)
let digits = repeat1 digit |> map ~f:String.of_char_list
           
let sequence ps =
  let rec iter = 
    function 
    | [] -> section []
    | p :: ps -> 
       consP p (iter ps)
  in iter ps
   
let stringP str =
  String.to_list str
  |> List.map ~f:charP 
  |> sequence
  |> map ~f:String.of_char_list

let (->>) p1 p2 =                  
  app p2 ~f:(map ~f:(fun x _ -> x) p1)
  
let (<<-) p1 p2 =                  
  app p2 ~f:(map ~f:(fun _ y -> y) p1)
  
let (|-|) p1 p2 = orP p1 (lazy p2)

let run =
  function Parser p ->
    fun src ->
    let res, rest = String.to_list src |> p in
    res, String.of_char_list rest
    
let rec until p =
  let p' = checkP p <<- section [] in
  orP p' (lazy (consP any (until p)))
