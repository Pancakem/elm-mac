module Instruction exposing (..)

import List.Extra as ListExtra



-- INSTRUCTIONS


type Instruction
    = HLT -- hlt              :: halts program
    | PSH -- psh val          :: pushes <val> to stack
    | POP -- pop              :: pops value from stack
    | ADD -- add              :: adds top two vals on stack
    | MUL -- mul              :: multiplies top two vals on stack
    | DIV -- div              :: divides top two vals on stack
    | SUB -- sub              :: subtracts top two vals on stack
    | SLT -- slt reg_a, reg_b :: pushes (reg_a < reg_b) to stack
    | MOV -- mov reg_a, reg_b :: movs the value in reg_a to reg_b
    | SET -- set reg, val     :: sets the reg to value
    | LOG -- log a            :: prints out a
    | IF -- if reg val ip     :: if the register == val branch to the ip
    | IFN -- ifn reg val ip   :: if the register != val branch to the ip
    | GLD -- gld reg          :: loads a register to the stack
    | GPT -- gpt reg          :: pushes top of stack to the given register
    | NOP -- nop              :: nothing


instructionToInt : Instruction -> Int
instructionToInt instr =
    case instr of
        HLT ->
            0

        PSH ->
            1

        POP ->
            2

        ADD ->
            3

        MUL ->
            4

        DIV ->
            5

        SUB ->
            6

        SLT ->
            7

        MOV ->
            8

        SET ->
            9

        LOG ->
            10

        IF ->
            11

        IFN ->
            12

        GLD ->
            13

        GPT ->
            14

        NOP ->
            15


intToInstruction : Int -> Instruction
intToInstruction instr =
    case instr of
        0 ->
            HLT

        1 ->
            PSH

        2 ->
            POP

        3 ->
            ADD

        4 ->
            MUL

        5 ->
            DIV

        6 ->
            SUB

        7 ->
            SLT

        8 ->
            MOV

        9 ->
            SET

        10 ->
            LOG

        11 ->
            IF

        12 ->
            IFN

        13 ->
            GLD

        14 ->
            GPT

        15 ->
            NOP

        _ ->
            HLT

instructionToString : Instruction -> String
instructionToString instr =
    case instr of
        HLT ->
            "HLT"

        PSH ->
            "PSH"

        POP ->
            "POP"

        ADD ->
            "ADD"

        MUL ->
            "MUL"

        DIV ->
            "DIV"

        SUB ->
            "SUB"

        SLT ->
            "SLT"

        MOV ->
            "MOV"

        SET ->
            "SET"

        LOG ->
            "LOG"

        IF ->
            "IF"

        IFN ->
            "IFN"

        GLD ->
            "GLD"

        GPT ->
            "GPT"

        NOP ->
            "NOP"
