import utils, unitInfo
from math import pow 


template systemInnerOps(System, a, b, step, S) =
    # Operations preserving units.

    proc `==`*[S: System](a, b: S): bool {.inline.} = (a.float == b.float)
    proc `<`*[S: System](a, b: S): bool {.inline.} = (a.float < b.float)
    proc `<=`*[S: System](a, b: S): bool {.inline.} = (a.float <= b.float)

    proc `-`*[S: System](a: S): S {.inline.} = S(-a.float)
    proc abs*[S: System](a: S): S {.inline.} = S(a.float.abs)

    proc `+`*[S: System](a, b: S): S {.inline.} = (a.float + b.float).S
    proc `-`*[S: System](a, b: S): S {.inline.} = (a.float - b.float).S

    proc `+=`*[S: System](a: var S, b: S) {.inline.} = a = a + b
    proc `-=`*[S: System](a: var S, b: S) {.inline.} = a = a - b

    proc `*`*[S: System](a: float, b: S): S {.inline.} = (a * b.float).S
    proc `*`*[S: System](a: S, b: float): S {.inline.} = (a.float * b).S
    proc `/`*[S: System](a: S, b: float): S {.inline.} = (a.float / b).S

    proc `*=`*[S: System](a: var S, b: float) {.inline.} = a = a * b
    proc `/=`*[S: System](a: var S, b: float) {.inline.} = a = a / b

    proc `div`*[S: System](a,b: S): int {.inline.} = (a.float / b.float).toInt
    proc `mod`*[S: System](a,b: S): int {.inline.} = (a / b) - (a div b)
  
    iterator countup*[S: System](a, b, step: S): S {.inline.} =
        ## Floating-point countup iterator.
        var acc = a
        while acc < b:
            acc += step
            yield acc

proc innerOpsDefinition*(info: SystemInfo): NimNode {.inline.} =
    getAst(systemInnerOps(info.name, ident"a", ident"b", ident"step", ident"S1"))
        
        
# template systemInnerOps(System, a, b, step, S) =
template systemOuterOps(System, a, b, S, S1, S2) =
    template `*`*[S1, S2: System](a: typedesc[S1], b: typedesc[S2]): auto =
        type(System[`+`])

    template `/`*[S1, S2: System](a: typedesc[S1], b: typedesc[S2]): auto =
        type(System[`-`])

    template `^`*[S: System](a: typedesc[S], b: static[int]): auto =
        type(System[`*` b])
        
    template `^-`*[S: System](a: typedesc[S], b: static[int]): auto =
        type(System[`*` (-b)])


    proc `*`*[S1, S2: System](a: S1, b: S2): auto {.inline.} =
        System[`+`](a.float * b.float)

    proc `/`*[S1, S2: System](a: S1, b: S2): auto {.inline.} =
        when S1 is S2: (a.float / b.float)
        else: System[`-`](a.float / b.float)


    proc `/`*[S: System](a: float, b: S): auto {.inline.} =
        System[`*`(-1)](a.float / b.float)

    proc `^`*[S: System](a: S, b: static[int]): auto {.inline.} =
        System[`*` b](pow(a.float, b))

    proc `^-`*[S: System](a: S, b: static[int]): auto {.inline.} =
        System[`*` (-b)](pow(a.float, -b))
    

proc injectManipulatedTypes*(info: SystemInfo, toUpdate, op: NimNode) =
    ## Injects manipulated types, e.x. summing or multiplying powers.

    template exactDiv(a, b: int, name, where: typed): int =
        # Exact division or compile-time error. For use in compile-time.
        when a mod b != 0:
            static:
                error fmt"power of {$name}({a}) is not exactly divisible by {b}, in type {$where}"
        a div b
 
    var 
        op: NimNode = op
        gen = newNimNode(nnkBracketExpr).add(info.name)

    # normal application
    if op.kind in {nnkIdent, nnkSym, nnkClosedSymChoice, nnkOpenSymChoice}:
        # for each system's base quantity, the operation is performed on both params' types' quantity power
        for unitInfo in info.units:
            #TODO: add some elasticity over param info.names
            let 
                operand1 = newDotExpr(ident"S1", unitInfo.quantity)
                operand2 = newDotExpr(ident"S2", unitInfo.quantity)
            gen.add newCall(op, operand1, operand2)
    
    # partial application
    else:
        let operand2 = op[1]
        op = op[0]
        # special treatment of `div`: use `exactDiv` instead
        # (maybe add a transformation table in the future)
        if op.eqIdent("div"):
            for unitInfo in info.units:
                let 
                    qname = unitInfo.quantity
                    operand1 = newDotExpr(ident"S1", unitInfo.quantity)
                # exactDiv is injected directly, not called
                gen.add getAst(exactDiv(operand1, operand2, qname, newCall("type", ident"a")))
        
        # other operations treated normally
        else:
            for unitInfo in info.units:
                let operand1 = newDotExpr(ident"S1", unitInfo.quantity)
                gen.add newCall(op, operand1, operand2)

    if toUpdate.kind == nnkCall and not toUpdate[0][1].eqIdent($info.name):
        gen = newNimNode(nnkBracketExpr).add(toUpdate[0][1]).add(gen)
    toUpdate[0] = gen


proc outerOpsDefinition*(info: SystemInfo): NimNode =
    ## Generate operations not preversing units.
    result = getAstCompact(systemOuterOps(info.name, ident"a",  ident"b", ident"S1", ident"S1", ident"S2"))

    for routine in result.children:
        if routine.kind notin RoutineNodes: continue

        # get final call (or last branch of final when statement) then transform it according to System[OP] rule.
        let lastCall = routine.body[^1]

        var 
            toUpdate = newSeq[NimNode]() # parent first child 
            ops = newSeq[NimNode]() # operation to be applied

        # if it returns type...
        if lastCall.kind == nnkTypeOfExpr or lastCall.kind == nnkCall:
            # ...call inside, with '[]' and 'System' as first and second param and OP as the third
            toUpdate.add lastCall
            ops.add lastCall[0][2]
                
        # if it returns different variant for S1 == S2...
        elif lastCall.kind == nnkWhenStmt:
            # two possiblities: either `S1 is S2` or anything else
            if lastCall[0].kind == nnkElifBranch and
                lastCall[0][0].kind == nnkInfix and
                lastCall[0][0][0].eqIdent("is") and
                lastCall[0][0][1].eqIdent("S1") and
                lastCall[0][0][2].eqIdent("S2"):
                # ...a when contains a whenElif, the second (last) one contains type(call)
                let up = lastCall[^1][0][0]
                toUpdate.add up
                ops.add up[0][2]

            else:
                # do the same for all branches
                var le = lastCall.len
                if lastCall[^1].kind == nnkElse:
                    dec le
                    let up = lastCall[^1][0][0]
                    toUpdate.add up
                    ops.add up[0][2]
                for i in 0..<le:
                    let up = lastCall[i][1][0]
                    toUpdate.add up
                    ops.add up[0][2]

        for i in 0..<toUpdate.len: injectManipulatedTypes(info, toUpdate[i], ops[i])