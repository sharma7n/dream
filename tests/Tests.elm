module Tests exposing (..)

import Test exposing (..)
import Expect


-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


testDocumentParser : Test
testDocumentParser =
    let
        testCase =
            test "documentParser" (\_ ->
                Expect.equal 1 1
            )
    in
    describe "Tests the document parser"
        [ testCase
        ]