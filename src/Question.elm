module Question exposing (..)


type QuestionFormat
    = FillInTheBlank
    | MultipleChoice


type alias Question =
    { question : String
    , distractors : List ResponseAndFeedback
    , answer : ResponseAndFeedback
    , format : QuestionFormat
    }


type alias ResponseAndFeedback =
    ( String, String )


emptyQuestion : Question
emptyQuestion =
    { question = ""
    , distractors = []
    , answer = ( "", "" )
    , format = FillInTheBlank
    }


newQuestion : List Int -> Int -> Question
newQuestion randomValues index =
    if index == 1 then
        { question = "Given this ML expression:\n\nval e1 = e2 + e3\n\nWhat is the type of e1?"
        , distractors =
            [ ( "string"
              , "Incorrect. e1 is the result of an addition, so it must be an int"
              )
            , ( "bool"
              , "Incorrect. e1 is the result of an addition, so it must be an int"
              )
            ]
        , answer =
            ( "int"
            , "Correct. e1 is the result of an addition, so it must be an int"
            )
        , format = MultipleChoice
        }
    else if index == 2 then
        { question = "Given this ML expression:\n\nval e1 = e2 + e3\n\nWhat is the type of e2?"
        , distractors =
            [ ( "string"
              , "Incorrect. e2 is part of an addition expression, so it must be an int"
              )
            , ( "bool"
              , "Incorrect. e2 is part of an addition expression, so it must be an int"
              )
            ]
        , answer =
            ( "int"
            , "Correct. e2 is part of an addition expression, so it must be an int"
            )
        , format = MultipleChoice
        }
    else if index == 3 then
        { question = "Given this ML expression:\n\nval e1 = e2 + e3\n\nWhat is the type of e3?"
        , distractors =
            [ ( "string"
              , "Incorrect. e3 is part of an addition expression, so it must be an int"
              )
            , ( "bool"
              , "Incorrect. e3 is part of an addition expression, so it must be an int"
              )
            ]
        , answer =
            ( "int"
            , "Correct. e3 is part of an addition expression, so it must be an int"
            )
        , format = MultipleChoice
        }
    else if index == 4 then
        { question = "Given this ML expression:\n\nval e1 = e2 - e3\n\nWhat is the type of e1?"
        , distractors =
            [ ( "string"
              , "Incorrect. e1 is the result of a subtraction, so it must be an int"
              )
            , ( "bool"
              , "Incorrect. e1 is the result of a subtraction, so it must be an int"
              )
            ]
        , answer =
            ( "int"
            , "Correct. e1 is the result of a subtraction, so it must be an int"
            )
        , format = MultipleChoice
        }
    else if index == 5 then
        { question = "Given this ML expression:\n\nval e1 = e2 - e3\n\nWhat is the type of e2?"
        , distractors =
            [ ( "string"
              , "Incorrect. e2 is part of a subtraction expression, so it must be an int"
              )
            , ( "bool"
              , "Incorrect. e2 is part of a subtraction expression, so it must be an int"
              )
            ]
        , answer =
            ( "int"
            , "Correct. e2 is part of a subtraction expression, so it must be an int"
            )
        , format = MultipleChoice
        }
    else if index == 6 then
        { question = "Given this ML expression:\n\nval e1 = e2 - e3\n\nWhat is the type of e3?"
        , distractors =
            [ ( "string"
              , "Incorrect. e3 is part of a subtraction expression, so it must be an int"
              )
            , ( "bool"
              , "Incorrect. e3 is part of a subtraction expression, so it must be an int"
              )
            ]
        , answer =
            ( "int"
            , "Correct. e3 is part of a subtraction expression, so it must be an int"
            )
        , format = MultipleChoice
        }
    else if index == 7 then
        { question = "Given this ML expression:\n\nval e1 = e2 * e3\n\nWhat is the type of e1?"
        , distractors =
            [ ( "string"
              , "Incorrect. e1 is the result of a multiplication, so it must be an int"
              )
            , ( "bool"
              , "Incorrect. e1 is the result of a multiplication, so it must be an int"
              )
            ]
        , answer =
            ( "int"
            , "Correct. e1 is the result of a multiplication, so it must be an int"
            )
        , format = MultipleChoice
        }
    else if index == 8 then
        { question = "Given this ML expression:\n\nval e1 = e2 * e3\n\nWhat is the type of e2?"
        , distractors =
            [ ( "string"
              , "Incorrect. e2 is part of a multiplication expression, so it must be an int"
              )
            , ( "bool"
              , "Incorrect. e2 is part of a multiplication expression, so it must be an int"
              )
            ]
        , answer =
            ( "int"
            , "Correct. e2 is part of a multiplication expression, so it must be an int"
            )
        , format = MultipleChoice
        }
    else if index == 9 then
        { question = "Given this ML expression:\n\nval e1 = e2 * e3\n\nWhat is the type of e3?"
        , distractors =
            [ ( "string"
              , "Incorrect. e3 is part of a multiplication expression, so it must be an int"
              )
            , ( "bool"
              , "Incorrect. e3 is part of a multiplication expression, so it must be an int"
              )
            ]
        , answer =
            ( "int"
            , "Correct. e3 is part of a multiplication expression, so it must be an int"
            )
        , format = MultipleChoice
        }
    else if index == 10 then
        { question = "Given this ML expression:\n\nval e1 = e2 > e3\n\nWhat is the type of e1?"
        , distractors =
            [ ( "string"
              , "Incorrect. e1 is the result of a greater-than comparison, so it must be a bool"
              )
            , ( "int"
              , "Incorrect. e1 is the result of a greater-than comparison, so it must be a bool"
              )
            ]
        , answer =
            ( "bool"
            , "Correct. e1 is the result of a greater-than comparison, so it must be a bool"
            )
        , format = MultipleChoice
        }
    else if index == 11 then
        { question = "Given this ML expression:\n\nval e1 = e2 > e3\n\nWhat is the type of e2?"
        , distractors =
            [ ( "string"
              , "Incorrect. e2 is part of a greater-than comparison, so it must be an int"
              )
            , ( "bool"
              , "Incorrect. e2 is part of a greater-than comparison, so it must be an int"
              )
            ]
        , answer =
            ( "int"
            , "Correct. e2 is part of a greater-than comparison, so it must be an int"
            )
        , format = MultipleChoice
        }
    else if index == 12 then
        { question = "Given this ML expression:\n\nval e1 = e2 > e3\n\nWhat is the type of e3?"
        , distractors =
            [ ( "string"
              , "Incorrect. e3 is part of a greater-than comparison, so it must be an int"
              )
            , ( "bool"
              , "Incorrect. e3 is part of a greater-than comparison, so it must be an int"
              )
            ]
        , answer =
            ( "int"
            , "Correct. e3 is part of a greater-than comparison, so it must be an int"
            )
        , format = MultipleChoice
        }
    else if index == 13 then
        { question = "Given this ML expression:\n\nval e1 = e2 < e3\n\nWhat is the type of e1?"
        , distractors =
            [ ( "string"
              , "Incorrect. e1 is the result of a less-than comparison, so it must be a bool"
              )
            , ( "int"
              , "Incorrect. e1 is the result of a less-than comparison, so it must be a bool"
              )
            ]
        , answer =
            ( "bool"
            , "Correct. e1 is the result of a less-than comparison, so it must be a bool"
            )
        , format = MultipleChoice
        }
    else if index == 14 then
        { question = "Given this ML expression:\n\nval e1 = e2 < e3\n\nWhat is the type of e2?"
        , distractors =
            [ ( "string"
              , "Incorrect. e2 is part of a less-than comparison, so it must be an int"
              )
            , ( "bool"
              , "Incorrect. e2 is part of a less-than comparison, so it must be an int"
              )
            ]
        , answer =
            ( "int"
            , "Correct. e2 is part of a less-than comparison, so it must be an int"
            )
        , format = MultipleChoice
        }
    else
        { question = "Given this ML expression:\n\nval e1 = e2 < e3\n\nWhat is the type of e3?"
        , distractors =
            [ ( "string"
              , "Incorrect. e3 is part of a less-than comparison, so it must be an int"
              )
            , ( "bool"
              , "Incorrect. e3 is part of a less-than comparison, so it must be an int"
              )
            ]
        , answer =
            ( "int"
            , "Correct. e3 is part of a less-than comparison, so it must be an int"
            )
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
