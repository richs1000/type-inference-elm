module Question exposing (..)


type QuestionFormat
    = FillInTheBlank
    | MultipleChoice


type alias Question =
    { question : List String
    , distractors : List ResponseAndFeedback
    , answer : ResponseAndFeedback
    , format : QuestionFormat
    }


type alias ResponseAndFeedback =
    ( String, String )


emptyQuestion : Question
emptyQuestion =
    { question = []
    , distractors = []
    , answer = ( "", "" )
    , format = FillInTheBlank
    }


pickOne : List Int -> List a -> a -> a
pickOne randomValues lst defVal =
    let
        -- get a random value
        rv =
            Maybe.withDefault 0 (List.head randomValues)

        -- use that to choose a random index within the list
        index =
            rv `rem` (List.length lst)
    in
        -- extract the list item at the index position
        lst
            |> List.drop index
            |> List.head
            |> Maybe.withDefault defVal


newQuestion : List Int -> Int -> Question
newQuestion randomValues index =
    let
        -- pick from [1, 2, 3] randomly for variable e1, e2 or e3
        myVar =
            pickOne randomValues [ "e1", "e2", "e3" ] "e1"
    in
        if index == 1 then
            let
                myOp =
                    pickOne (List.drop 1 randomValues) [ "+", "-", "*" ] "+"

                myOp' =
                    if myOp == "+" then
                        "addition"
                    else if myOp == "-" then
                        "subtraction"
                    else
                        "multiplication"

                question' =
                    [ "Given this ML expression:"
                    , "val e1 = e2 " ++ myOp ++ " e3"
                    , "What is the type of " ++ myVar ++ "?"
                    ]

                distractors' =
                    if myVar == "e1" then
                        [ ( "string"
                          , "Incorrect. " ++ myVar ++ " is the result of " ++ myOp' ++ ", so it must be an int"
                          )
                        , ( "bool"
                          , "Incorrect. " ++ myVar ++ " is the result of " ++ myOp' ++ ", so it must be an int"
                          )
                        , ( "It can have any type"
                          , "Incorrect. " ++ myVar ++ " is the result of " ++ myOp' ++ ", so it must be an int"
                          )
                        ]
                    else
                        [ ( "string"
                          , "Incorrect. " ++ myVar ++ " is part of a " ++ myOp' ++ " expression, so it must be an int"
                          )
                        , ( "bool"
                          , "Incorrect. " ++ myVar ++ " is part of a " ++ myOp' ++ " expression, so it must be an int"
                          )
                        , ( "It can have any type"
                          , "Incorrect. " ++ myVar ++ " is part of a " ++ myOp' ++ " expression, so it must be an int"
                          )
                        ]

                answer' =
                    if myVar == "e1" then
                        ( "int"
                        , "Correct. " ++ myVar ++ " is the result of " ++ myOp' ++ ", so it must be an int"
                        )
                    else
                        ( "int"
                        , "Correct. " ++ myVar ++ " is part of a " ++ myOp' ++ " expression, so it must be an int"
                        )
            in
                { question = question'
                , distractors = distractors'
                , answer = answer'
                , format = MultipleChoice
                }
        else if index == 2 then
            let
                myOp =
                    pickOne (List.drop 1 randomValues) [ ">", ">=", "<", "<=" ] ">"

                myOp' =
                    if myOp == ">" then
                        "greater than"
                    else if myOp == ">=" then
                        "greater than or equal to"
                    else if myOp == "<=" then
                        "less than or equal to"
                    else
                        "less than"

                question' =
                    [ "Given this ML expression:"
                    , "val e1 = e2 " ++ myOp ++ " e3"
                    , "What is the type of " ++ myVar ++ "?"
                    ]

                distractors' =
                    if myVar == "e1" then
                        [ ( "string"
                          , "Incorrect. " ++ myVar ++ " is the result of a " ++ myOp' ++ " expression, so it must be a bool"
                          )
                        , ( "int"
                          , "Incorrect. " ++ myVar ++ " is the result of a " ++ myOp' ++ " expression, so it must be a bool"
                          )
                        , ( "It can have any type"
                          , "Incorrect. " ++ myVar ++ " is the result of a " ++ myOp' ++ " expression, so it must be a bool"
                          )
                        ]
                    else
                        [ ( "string"
                          , "Incorrect. " ++ myVar ++ " is part of a " ++ myOp' ++ " expression, so it must be an int"
                          )
                        , ( "bool"
                          , "Incorrect. " ++ myVar ++ " is part of a " ++ myOp' ++ " expression, so it must be an int"
                          )
                        , ( "It can have any type"
                          , "Incorrect. " ++ myVar ++ " is part of a " ++ myOp' ++ " expression, so it must be an int"
                          )
                        ]

                answer' =
                    if myVar == "e1" then
                        ( "bool"
                        , "Correct. " ++ myVar ++ " is the result of a " ++ myOp' ++ " expression, so it must be a bool"
                        )
                    else
                        ( "int"
                        , "Correct. " ++ myVar ++ " is part of a " ++ myOp' ++ " expression, so it must be an int"
                        )
            in
                { question = question'
                , distractors = distractors'
                , answer = answer'
                , format = MultipleChoice
                }
        else if index == 3 && myVar == "e1" then
            let
                question' =
                    [ "Given this ML expression:"
                    , "if e1 then e2 else e3"
                    , "What is the type of " ++ myVar ++ "?"
                    ]

                distractors' =
                    [ ( "string"
                      , "Incorrect. e1 is the test condition within an if-then-else expression, so it must be a bool"
                      )
                    , ( "int"
                      , "Incorrect. e1 is the test condition within an if-then-else expression, so it must be a bool"
                      )
                    , ( "It can have any type"
                      , "Incorrect. e1 is the test condition within an if-then-else expression, so it must be a bool"
                      )
                    ]

                answer' =
                    ( "bool"
                    , "Correct. e1 is the test condition within an if-then-else expression, so it must be a bool"
                    )
            in
                { question = question'
                , distractors = distractors'
                , answer = answer'
                , format = MultipleChoice
                }
        else
            let
                -- if the first variable in the question is e2, the second is e3 (and vice versa)
                myVar2 =
                    if myVar == "e2" then
                        "e3"
                    else
                        "e2"

                -- pick a type randomly
                myAnswer =
                    pickOne (List.drop 1 randomValues) [ "int", "bool", "string" ] "int"

                -- get the remaining types, which are distractors
                ( _, myDistractors ) =
                    List.partition (\s -> s == myAnswer) [ "int", "bool", "string", "It can have any type" ]

                question' =
                    [ "Given this ML expression:"
                    , "if e1 then e2 else e3"
                    , "If " ++ myVar ++ " has type " ++ myAnswer ++ ", what is the type of " ++ myVar2 ++ "?"
                    ]

                answer' =
                    ( myAnswer
                    , "Correct. Both branches of an if-then-else statement must have the same type."
                    )

                distractors' =
                    List.foldr
                        (\d ds -> ( d, "Incorrect. Both branches of an if-then-else statement must have the same type." ) :: ds)
                        []
                        myDistractors
            in
                { question = question'
                , distractors = distractors'
                , answer = answer'
                , format = MultipleChoice
                }


findFeedback : String -> String -> List ResponseAndFeedback -> String
findFeedback answer response distractors =
    case distractors of
        [] ->
            "Incorrect. The answer is " ++ answer

        d :: ds ->
            if ((fst d) == response || ((fst d) == "")) then
                (snd d) ++ " The answer is " ++ answer
            else
                findFeedback answer response ds
