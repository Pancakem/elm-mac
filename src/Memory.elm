module Memory exposing (Instructions, boolToInt, initInstructions, instrCount, instructionToAssembly, instructionsFromList, readInstruction)

import Array
import Instruction exposing (..)
import List.Extra as ListExtra


type Instructions
    = Instructions (List Int)


initInstructions : Instructions
initInstructions =
    Instructions []


instructionsFromList : List Int -> Instructions
instructionsFromList lis =
    Instructions lis


readInstruction : Int -> Instructions -> Int
readInstruction instP (Instructions instrs) =
    ListExtra.getAt instP instrs
        |> Maybe.withDefault 0


instrCount : Instructions -> Int
instrCount (Instructions instr) =
    List.length instr


boolToInt : Bool -> Int
boolToInt bool =
    case bool of
        True ->
            1

        False ->
            0


instructionToAssembly : Int -> Instructions -> String -> String
instructionToAssembly instrPointer instr asm =
    let
        instruct x =
            intToInstruction (readInstruction x instr)

        intInst x =
            readInstruction x instr
    in
    case instruct instrPointer of
        HLT ->
            asm ++ instructionToString (instruct instrPointer)

        PSH ->
            let
                newasm =
                    asm ++ instructionToString (instruct instrPointer) ++ " " ++ String.fromInt (intInst (instrPointer + 1)) ++ ","
            in
            instructionToAssembly (instrPointer + 2) instr newasm

        POP ->
            let
                newasm =
                    asm ++ instructionToString (instruct instrPointer) ++ ","
            in
            instructionToAssembly (instrPointer + 1) instr newasm

        ADD ->
            let
                newasm =
                    asm ++ instructionToString (instruct instrPointer) ++ ","
            in
            instructionToAssembly (instrPointer + 1) instr newasm

        MUL ->
            let
                newasm =
                    asm ++ instructionToString (instruct instrPointer) ++ ","
            in
            instructionToAssembly (instrPointer + 1) instr newasm

        DIV ->
            let
                newasm =
                    asm ++ instructionToString (instruct instrPointer) ++ ","
            in
            instructionToAssembly (instrPointer + 1) instr newasm

        SUB ->
            let
                newasm =
                    asm ++ instructionToString (instruct instrPointer) ++ ","
            in
            instructionToAssembly (instrPointer + 1) instr newasm

        SLT ->
            let
                newasm =
                    asm ++ instructionToString (instruct instrPointer) ++ " " ++ String.fromInt (intInst (instrPointer + 1)) ++ " " ++ String.fromInt (intInst (instrPointer + 2)) ++ ","
            in
            instructionToAssembly (instrPointer + 3) instr newasm

        MOV ->
            let
                newasm =
                    asm ++ instructionToString (instruct instrPointer) ++ " " ++ String.fromInt (intInst (instrPointer + 1)) ++ " " ++ String.fromInt (intInst (instrPointer + 2)) ++ ","
            in
            instructionToAssembly (instrPointer + 3) instr newasm

        SET ->
            let
                newasm =
                    asm ++ instructionToString (instruct instrPointer) ++ " " ++ String.fromInt (intInst (instrPointer + 1)) ++ " " ++ String.fromInt (intInst (instrPointer + 2)) ++ ","
            in
            instructionToAssembly (instrPointer + 3) instr newasm

        LOG ->
            let
                newasm =
                    asm ++ instructionToString (instruct instrPointer) ++ " " ++ String.fromInt (intInst (instrPointer + 1)) ++ ","
            in
            instructionToAssembly (instrPointer + 2) instr newasm

        IF ->
            let
                newasm =
                    asm ++ instructionToString (instruct instrPointer) ++  " " ++ String.fromInt (intInst (instrPointer + 1)) ++  " " ++ String.fromInt (intInst (instrPointer + 2)) ++  " " ++ String.fromInt (intInst (instrPointer + 3)) ++ ","
            in
            instructionToAssembly (instrPointer + 4) instr newasm

        IFN ->
            let
                newasm =
                    asm ++ instructionToString (instruct instrPointer) ++  " " ++String.fromInt (intInst (instrPointer + 1)) ++  " " ++String.fromInt (intInst (instrPointer + 2)) ++  " " ++String.fromInt (intInst (instrPointer + 3)) ++ ","
            in
            instructionToAssembly (instrPointer + 4) instr newasm

        GLD ->
            let
                newasm =
                    asm ++ instructionToString (instruct instrPointer) ++  " " ++ String.fromInt (intInst (instrPointer + 1)) ++ ","
            in
            instructionToAssembly (instrPointer + 2) instr newasm

        GPT ->
            let
                newasm =
                    asm ++ instructionToString (instruct instrPointer) ++  " " ++ String.fromInt (intInst (instrPointer + 1)) ++ ","
            in
            instructionToAssembly (instrPointer + 2) instr newasm

        NOP ->
            instructionToString (instruct instrPointer) ++ ","
