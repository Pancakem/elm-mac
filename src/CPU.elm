module CPU exposing (..)

import Instruction exposing (..)
import Memory exposing (..)
import Register exposing (..)
import Stack exposing (..)



-- CPU state


type alias Frame =
    { isJump : Bool
    , stack : Stack
    , registers : Registers
    , running : Bool
    , instructionCount : Int
    , instructionSpace : Int
    , stackPointer : Int
    , instructionPointer : Int
    , instructions : Instructions
    , logger : ( Bool, Int )
    }


initFrame : Frame
initFrame =
    { isJump = False
    , stack = initStack
    , registers = initRegisters
    , running = False
    , instructionCount = 0
    , instructionSpace = 0
    , stackPointer = -1
    , instructionPointer = 0
    , instructions = initInstructions
    , logger = ( False, 0 )
    }


readyToRun : Frame -> Frame
readyToRun frame =
    { frame | running = True }


loadInstructions : Instructions -> Frame -> Frame
loadInstructions instr frame =
    { frame | instructions = instr, instructionCount = instrCount instr }



-- fetches the instruction at the current pointer and executes it


eval : Frame -> Frame
eval frameState =
    let
        frame =
            { frameState | isJump = False }
    in
    case intToInstruction <| readInstruction frame.instructionPointer frame.instructions of
        HLT ->
            { frame | running = False }

        PSH ->
            let
                updatedSP =
                    { frame
                        | stackPointer = frame.stackPointer + 1
                        , instructionPointer = frame.instructionPointer + 1
                    }

                updatedStack =
                    { updatedSP | stack = push (readInstruction updatedSP.instructionPointer updatedSP.instructions) updatedSP.stack }
            in
            updatedStack

        POP ->
            let
                ( _, newStack ) =
                    pop frame.stack
            in
            { frame | stackPointer = frame.stackPointer - 1, stack = newStack }

        ADD ->
            let
                ( valueA, newStack ) =
                    pop frame.stack

                updatedRegA =
                    { frame
                        | registers = writeRegister A valueA frame.registers
                        , stackPointer = frame.stackPointer - 1
                    }

                ( valueB, newerStack ) =
                    pop newStack

                updatedRegB =
                    { updatedRegA
                        | registers = writeRegister B valueB updatedRegA.registers
                        , stackPointer = updatedRegA.stackPointer - 1
                        , stack = newerStack
                    }

                updatedRegC =
                    { updatedRegB
                        | registers = writeRegister C (readRegister B updatedRegB.registers + readRegister A updatedRegB.registers) updatedRegB.registers
                    }

                updatedStack =
                    { updatedRegC
                        | stack = push (readRegister C updatedRegC.registers) updatedRegC.stack
                        , stackPointer = updatedRegC.stackPointer + 1
                    }
            in
            updatedStack

        MUL ->
            let
                ( valueA, newStack ) =
                    pop frame.stack

                updatedRegA =
                    { frame
                        | registers = writeRegister A valueA frame.registers
                        , stackPointer = frame.stackPointer - 1
                    }

                ( valueB, newerStack ) =
                    pop newStack

                updatedRegB =
                    { updatedRegA
                        | registers = writeRegister B valueB updatedRegA.registers
                        , stackPointer = updatedRegA.stackPointer - 1
                    }

                updatedRegC =
                    { updatedRegB
                        | registers = writeRegister C (readRegister B updatedRegB.registers * readRegister A updatedRegB.registers) updatedRegB.registers
                    }

                updatedStack =
                    { updatedRegC
                        | stack = push (readRegister C updatedRegC.registers) updatedRegC.stack
                        , stackPointer = updatedRegC.stackPointer + 1
                    }
            in
            updatedStack

        DIV ->
            let
                ( valueA, newStack ) =
                    pop frame.stack

                updatedRegA =
                    { frame
                        | registers = writeRegister A valueA frame.registers
                        , stackPointer = frame.stackPointer - 1
                    }

                ( valueB, newerStack ) =
                    pop newStack

                updatedRegB =
                    { updatedRegA
                        | registers = writeRegister B valueB updatedRegA.registers
                        , stackPointer = updatedRegA.stackPointer - 1
                    }

                updatedRegC =
                    { updatedRegB
                        | registers = writeRegister C (readRegister B updatedRegB.registers // readRegister A updatedRegB.registers) updatedRegB.registers
                    }

                updatedStack =
                    { updatedRegC
                        | stack = push (readRegister C updatedRegC.registers) updatedRegC.stack
                        , stackPointer = updatedRegC.stackPointer + 1
                    }
            in
            updatedStack

        SUB ->
            let
                ( valueA, newStack ) =
                    pop frame.stack

                updatedRegA =
                    { frame
                        | registers = writeRegister A valueA frame.registers
                        , stackPointer = frame.stackPointer - 1
                    }

                ( valueB, newerStack ) =
                    pop newStack

                updatedRegB =
                    { updatedRegA
                        | registers = writeRegister B valueB updatedRegA.registers
                        , stackPointer = updatedRegA.stackPointer - 1
                    }

                updatedRegC =
                    { updatedRegB
                        | registers = writeRegister C (readRegister B updatedRegB.registers - readRegister A updatedRegB.registers) updatedRegB.registers
                    }

                updatedStack =
                    { updatedRegC
                        | stack = push (readRegister C updatedRegC.registers) updatedRegC.stack
                        , stackPointer = updatedRegC.stackPointer + 1
                    }
            in
            updatedStack

        SLT ->
            let
                newFrame =
                    { frame | stackPointer = frame.stackPointer - 1 }

                ( pop1, stack1 ) =
                    pop newFrame.stack

                ( pop2, stack2 ) =
                    pop stack1

                newStack =
                    push
                        (boolToInt
                            (pop1 < pop2)
                        )
                        stack2
            in
            { newFrame
                | stack = newStack
                , instructionPointer = newFrame.instructionPointer + 3
            }

        MOV ->
            let
                newRegister =
                    writeRegister (intToRegister <| readInstruction (frame.instructionPointer + 2) frame.instructions) (readInstruction (frame.instructionPointer + 1) frame.instructions) frame.registers

                newFrame =
                    { frame
                        | instructionPointer = frame.instructionPointer + 2
                        , registers = newRegister
                    }
            in
            newFrame

        SET ->
            let
                newRegister =
                    writeRegister (intToRegister <| readInstruction (frame.instructionPointer + 1) frame.instructions) (readInstruction (frame.instructionPointer + 2) frame.instructions) frame.registers

                newFrame =
                    { frame
                        | instructionPointer = frame.instructionPointer + 2
                        , registers = newRegister
                    }
            in
            newFrame

        LOG ->
            -- prints the contents of registers[instructions[IP+1]]
            { frame
                | instructionPointer = frame.instructionPointer + 1
                , logger = ( True, readRegister (intToRegister (frame.instructionPointer + 1)) frame.registers )
            }

        IF ->
            let
                newFrame =
                    if readRegister (intToRegister (frame.instructionPointer + 1)) frame.registers == readInstruction (frame.instructionPointer + 2) frame.instructions then
                        { frame
                            | instructionPointer = readInstruction (frame.instructionPointer + 3) frame.instructions
                            , isJump = True
                        }

                    else
                        { frame | instructionPointer = frame.instructionPointer + 3 }
            in
            newFrame

        IFN ->
            let
                newFrame =
                    if readRegister (intToRegister (frame.instructionPointer + 1)) frame.registers /= readInstruction (frame.instructionPointer + 2) frame.instructions then
                        { frame
                            | instructionPointer = readInstruction (frame.instructionPointer + 3) frame.instructions
                            , isJump = True
                        }

                    else
                        { frame | instructionPointer = frame.instructionPointer + 3 }
            in
            newFrame

        GLD ->
            let
                newFrame =
                    { frame | stackPointer = frame.stackPointer + 1, instructionPointer = frame.instructionPointer + 1 }

                newStack =
                    push (readRegister (intToRegister (readInstruction (newFrame.instructionPointer + 1) newFrame.instructions)) newFrame.registers) newFrame.stack
            in
            { newFrame
                | stack = newStack
            }

        GPT ->
            let
                ( val, stack ) =
                    pop frame.stack

                newRegisters =
                    writeRegister (intToRegister (readInstruction (frame.instructionPointer + 1) frame.instructions)) val frame.registers
            in
            { frame
                | registers = newRegisters
                , instructionPointer = frame.instructionPointer + 1
                , stack = stack
                , stackPointer = frame.stackPointer + 1
            }

        NOP ->
            frame
