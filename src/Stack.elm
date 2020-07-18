module Stack exposing (Stack, initStack, pop, push, top)


type Stack
    = Stack (List Int)


initStack : Stack
initStack =
    Stack []


push : Int -> Stack -> Stack
push item (Stack stack) =
    Stack (item :: stack)


top : Stack -> Maybe Int
top (Stack stack) =
    List.head stack


pop : Stack -> ( Int, Stack )
pop (Stack stack) =
    case stack of
        [] ->
            ( 0, Stack [] )

        head :: tail ->
            ( head, Stack tail )
