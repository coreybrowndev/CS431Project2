(*
    Corey Brown
    03/25/2023
    Purpose of this application is to check whether a string is a valid palidrome or not.
*)

(*
Function that reverses a list
@params 
    list of elements
*)
fun my_reverse [] = [] 
    | my_reverse (x::xs) = my_reverse xs @ [x];

(*
Function that removes any non alphabetical characters from a list 
of characters.
@params 
    list of characters
*)
fun removeNonAlphabetic [] = []
    | removeNonAlphabetic(x::xs) = 
    let
        val code = ord x
    in 
        if
        (code >= ord #"a" andalso code <= ord #"z") orelse 
        (code >= ord #"A" andalso code <= ord #"Z") 
        then x::removeNonAlphabetic(xs)
        else removeNonAlphabetic(xs)
    end

(*
This function takes in a list of characters and returns the characters to a lowercase
@params
    list of characters
*)
fun changeToLowercase [] = []
    | changeToLowercase(x::xs) =
    let 
        val code = ord x
    in 
        if code >= ord #"A" andalso code <= ord #"Z" then chr(code + 32)::changeToLowercase(xs)
        else x::changeToLowercase(xs)
    end


(*
This function takes in an list of characters and returns the last character in the list
@params
    list of characters
*)
fun getLast [] = raise Empty
    (*Base case check if there is only one character which will simply return that character*)
    | getLast [x] = x
    | getLast(x::xs) = getLast(xs);

(*
This function takes in a list of characters and removes the last character in the list
@params
    list of characters
*)
fun removeLast [] = []
    (*Base case check if there is only one character which will return an empty list*)
    | removeLast [x] = []
    | removeLast(x::xs) = x::removeLast(xs);




(*The steps in which the function performs*)
(*
    1. Remove all non-alphabetic characters from the string
    2. Change all characters to lowercase
    3.reverse the string
    4. Compare the reversed string to the original string
    5. If the strings are the same then the string is a palindrome
*)
fun is_palindrome s = 
    let
        val word = explode s
        val clean = removeNonAlphabetic(word)
        val lower = changeToLowercase(clean)
        val reversed = my_reverse(lower)
    in 
        lower = reversed
    end;

