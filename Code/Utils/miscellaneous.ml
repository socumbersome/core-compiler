let (>>) f g x = g(f x);;
let (<<) f g x = f(g x);;
let (|>) x f = f x;;
let (<|) f x = f x;;

let spaces n = if n <= 0 then "" else String.make n ' ';;