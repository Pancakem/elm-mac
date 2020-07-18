module Register exposing (..)

import Array



-- REGISTERS


type Register
    = -- A, B, C, D, E, F, I, J are general purpose registers
      A
    | B
    | C
    | D
    | E
    | F
    | I
    | J
    | EX -- Excess
    | EXA -- More excess
    | IP -- Instruction pointer
    | SP -- Stack pointer
    | RegisterSize


type Registers
    = Registers (Array.Array Int)


initRegisters : Registers
initRegisters =
    Array.initialize (registerToInt RegisterSize) (always 0)
        |> Registers


registerToInt : Register -> Int
registerToInt reg =
    case reg of
        A ->
            1

        B ->
            2

        C ->
            3

        D ->
            4

        E ->
            5

        F ->
            6

        I ->
            7

        J ->
            8

        EX ->
            9

        EXA ->
            10

        IP ->
            11

        SP ->
            12

        RegisterSize ->
            13


intToRegister : Int -> Register
intToRegister reg =
    case reg - 1 of
        0 ->
            A

        1 ->
            B

        2 ->
            C

        3 ->
            D

        4 ->
            E

        5 ->
            F

        6 ->
            I

        7 ->
            J

        8 ->
            EX

        9 ->
            EXA

        10 ->
            IP

        11 ->
            SP

        _ ->
            RegisterSize


getRegisters : Registers -> Array.Array Int
getRegisters (Registers regs) =
    regs


readRegister : Register -> Registers -> Int
readRegister reg (Registers registers) =
    Array.get ((registerToInt reg) - 1) registers
        |> Maybe.withDefault 0


writeRegister : Register -> Int -> Registers -> Registers
writeRegister reg val (Registers registers) =
    let
        newRegisters =
            Array.set ((registerToInt reg) - 1) val registers
                |> Registers
    in
    newRegisters
