import macros
from strformat import fmt
from sequtils import foldl, mapIt
from math import pow
from strutils import `%`


proc callToPar(node: NimNode): NimNode =
    ## Converts call to its arguments in par.
    result = newNimNode(nnkPar)
    node.copyChildrenTo(result)
    result.del(0)

proc prettyRepr(s: string): string {.inline.} = s

proc prettyRepr(node: NimNode): string {.inline.} =
    if node.kind == nnkCall and node.len == 2: fmt"{node[0].repr}: {node[1].repr}" else: node.repr

proc errorTrace(src: NimNode, fmt: string, args: varargs[string, prettyRepr]) =
    ## Nice and handy error shouter.
    var s = @[src.prettyRepr]
    for arg in args: s.add arg
    error(fmt % s, src)

const
    notValidAs = "invalid $2: '$1'"
    identExpectedAs = "identifier expected as $2 but '$1' found"
    expectedAsIn = "$2 in form '$3' expected, but '$1' found"
    expectedAsMaybe = "$2 in form '$4' expected, but '$1' found (maybe try '$3'?)"


type UnitInfo = object  ## Contains information on a quantity and its unit.
    quantity: NimNode
    name: NimNode
    abbr: NimNode

proc newUnitInfo(quantity, name, abbr: NimNode): UnitInfo {.inline.} = 
    UnitInfo(quantity: quantity, name: name, abbr: abbr)

proc quantity*(info: UnitInfo): NimNode {.inline.} = info.quantity
proc name*(info: UnitInfo): NimNode {.inline.} = info.name
proc abbr*(info: UnitInfo): NimNode {.inline.} = info.abbr


proc getUnitInfo(node: NimNode): UnitInfo =
    ## Checks a node for being a valid quantity declaration and converts it to a UnitInfo object.
    if not (node.kind == nnkCall and node.len == 2 and node[0].kind == nnkIdent):
        error(fmt"expected quantity declaration of form 'Quantity: unit(abbr)', but found '{node.repr}'", node)

    let quantity = node[0]
    if quantity.kind != nnkIdent:
        error(fmt"expected quantity name in 'Quantity: unit(abbr)', but found '{quantity.repr}'", quantity)
        
    let unit = node[1][0]
    if unit.kind != nnkCall:
        if unit.kind == nnkIdent:
            error(fmt"no abbreviated unit name found for '{quantity.repr}: {unit.repr}', expected in form '{quantity.repr}: {unit.repr}(abbr)'", unit)
        error(fmt"unit names expected in form '{quantity.repr}: unit(abbr)', but found {unit.repr}", unit)

    if unit.len != 2:
        error(fmt"unit's single abbreviated name expected in form '{quantity.repr}: {unit[0].repr}(abbr)', but found '{callToPar(unit).repr}'", unit)

    let (unitName, unitAbbr) = (unit[0], unit[1])
    if unitName.kind != nnkIdent:
        error(fmt"identifier expected as unit full name in form '{quantity.repr}: unit(abbr)', but found '{unitName.repr}'", unitName)

    if unitAbbr.kind != nnkIdent:
        error(fmt"identifier expected as a unit abbreviated name in form '{unitName.repr}(abbr)', but found '{unitAbbr.repr}'", unitAbbr)

    newUnitInfo(quantity, unitName, unitAbbr)


type SystemInfo = object  ## Contains informations on unit system.
    name:  NimNode
    units: seq[UnitInfo]

proc newSystemInfo(name: NimNode, units: seq[UnitInfo]): SystemInfo {.inline.} =
    SystemInfo(name: name, units: units)

proc name*(info: SystemInfo): NimNode {.inline.} = info.name
proc units*(info: SystemInfo): seq[UnitInfo] {.inline.} = info.units


proc typeDefinition*(info: SystemInfo): NimNode =
    ## Generate unit system's type definition.

    # multiple-usage predefined nodes: `static[int]` and `distinct float`
    let 
        statNode = newTree(nnkStaticTy, bindSym"int")
        distNode = newTree(nnkDistinctTy, bindSym"float")

    # Parameters node containing all quantities (of type static[int]).
    let genNode = newNimNode(nnkGenericParams).add:
                    info.units
                        .foldl(a.add b.quantity, newNimNode(nnkIdentDefs))
                        .add(statNode)
                        .add(newEmptyNode())

    result = newStmtList()
    result.add newNimNode(nnkTypeSection).add(newTree(nnkTypeDef, info.name, genNode, distNode))
    
    template unit(Unit, System) =
        type Unit[S: System] = object
    
    result.add getAst(unit(ident($info.name & "Unit"), info.name))


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
                error fmt"power of {$name}({a}) is not exactly divisible by {b} in type {$where}"
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
    result = getAst(systemOuterOps(info.name, ident"a",  ident"b", ident"S1", ident"S1", ident"S2"))

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


proc printOpsDefinition*(info: SystemInfo): NimNode =
    ## Generate print operations for floats with units.

    # first create initializations, then add whenned appendings
    template ops(System, s, S, result) =
        proc `$`*[S: System](s: typedesc[S]): string =
            result = ""
        proc `$`*[S: System](s: S): string =
            result = $(s.float) & " "
    
    result = getAst(ops(info.name, ident"s", ident"S", ident"result"))

    for i, unitInfo in info.units:
        let quantity = unitInfo.quantity
        let abbr = unitInfo.abbr

        # add a single prop to the output
        #TODO: string optimization using yet another macro?
        proc printProp(name, uname: NimNode): NimNode =
            template appProp(prop, unit, result) =
                when S.prop != 0:
                    when S.prop == 1:
                        result &= unit & " "
                    else:
                        result &= unit & "^" & $(S.prop) & " "
            let cond = getAst(appProp(name, $uname, ident"result"))
            result = cond

        result[0].body.add printProp(quantity, quantity)
        result[1].body.add printProp(quantity, abbr)

    # del the trailing space
    template delLastChar(result) =
        if result.len != 0: result.setLen(result.len-1)

    let delLastResultChar = getAst(delLastChar(ident"result"))
    result[0].body.add delLastResultChar
    result[1].body.add delLastResultChar


template defQuantityType(qname, definition) =
    type qname* = definition
        
template declUnitFn(qname, rname, x) =
    proc rname*[T](x: T): qname {.inline.} = x.float.qname

proc quantityDefinition*(info: SystemInfo, idx: int): NimNode = ## i-th quantity type definition.
    let 
        qname = info.units[idx].quantity
        uname = info.units[idx].name
        definition = info.units.foldl(a.add newLit 0, newTree(nnkBracketExpr, info.name)) # unitlessType

    result = newStmtList()
    definition[idx + 1] = newLit 1

    result.add getAst(defQuantityType(qname, definition))
    result.add getAst(declUnitFn(qname, uname, ident"x"))


macro unitSystem*(name, impl: untyped) =
    if name.kind != nnkIdent: error("expected system name", name)

    let info = newSystemInfo(name, impl.mapIt(it.getUnitInfo))

    result = newStmtList()
    result.add info.typeDefinition
    result.add info.innerOpsDefinition
    result.add info.outerOpsDefinition
    result.add info.printOpsDefinition
    
    for i, _ in info.units:
        result.add info.quantityDefinition(i)


template genQuantity(qname, def) =
    type qname* = def

template genUnit(qname, uname, x) =
    proc uname*(x: float | int): qname {.inline.} = x.qname

template genAbbr(qname, aname, x) =
    proc aname*(x: float | int): qname = x * 1.0.qname

macro unitQuantity*(code: untyped) =
    result = newStmtList()

    for quantity in code:
        var qname, uname, aname, definition: NimNode

        if not (quantity.len == 2 and quantity[0].kind == nnkIdent):
            quantity.errorTrace(notValidAs, "quantity declaration")

        # no unit:
        if quantity.kind == nnkAsgn:
            qname = quantity[0]
            definition = quantity[1]

        # with unit:
        elif quantity.kind == nnkCall:
            qname = quantity[0]
            let afterName = quantity[1][0]
            if not (afterName.kind == nnkAsgn and afterName.len == 2):
                afterName.errorTrace(expectedAsIn, "quantity implementation", fmt"{qname} = impl", fmt"{qname}: unit(abbr) = impl")

            let unames = afterName[0]
            definition = afterName[1]

            if not (unames.kind == nnkCall and 
                    unames.len == 2 and
                    unames[0].kind == nnkIdent and 
                    unames[1].kind == nnkIdent): unames.errorTrace(expectedAsIn, "quantity unit name and abbreviated name", "unit(abbr)")

            uname = unames[0]                
            aname = unames[1]

        else: quantity.errorTrace(notValidAs, "quantity declaration")

        result.add getAst(genQuantity(qname, definition))
        
        if uname != nil:
            result.add getAst(genUnit(qname, uname, ident"x"))
            result.add getAst(genAbbr(qname, aname, ident"x"))    


template genPrefix(name, value, x) =
    proc name*(x: float | int): auto = x * value

macro unitPrefix*(code: untyped) =
    result = newStmtList()

    for prefix in code:
        if not (prefix.kind == nnkCall and prefix.len == 2):
            prefix.errorTrace(expectedAsIn, "prefix declaration", "prefix: number")

        let (name, value) = (prefix[0], prefix[1])
        if name.kind != nnkIdent: name.errorTrace(identExpectedAs, "prefix name")

        result.add getAst(genPrefix(name, value, ident"x"))


template genAbbrFun(abbr, prefix, unit, x) =
    proc abbr*(x: float | int): auto {.inline.} = x.prefix.unit

macro unitAbbr*(code: untyped) =
    result = newStmtList()

    for abbr in code:
        if not (abbr.kind == nnkAsgn and abbr.len == 2):
            abbr.errorTrace(expectedAsIn, "prefixed unit abbreviation declaration", "abbr = prefix.unit")

        let (name, what) = (abbr[0], abbr[1])
        if name.kind != nnkIdent: 
            name.errorTrace(identExpectedAs, "prefix unit abbreviation")
        if not (what.kind == nnkDotExpr and 
                what.len == 2 and 
                what[0].kind == nnkIdent and 
                what[1].kind == nnkIdent): what.errorTrace(expectedAsIn, "prefixed unit", fmt"{name} = prefixed.unit")
        
        let (prefix, unit) = (what[0], what[1])
        result.add getAst(genAbbrFun(name, prefix, unit, ident"x"))


template genAlias(alias, x, expr, unit) =
    proc alias*(x: float | int): auto {.inline.} =
        let x = x.float
        expr.unit

macro unitAlias*(code: untyped) =
    result = newStmtList()

    for aliasDef in code:
        # Any alias parses as assignment with alias on the left side and definition on the right side.
        if not (aliasDef.kind == nnkAsgn and aliasDef.len == 2):
            aliasDef.errorTrace(expectedAsIn, "alias definition", "x.alias = expr(x).unit", "x.alias(abbr) = expr(x).unit")

        var (aliasExpr, def) = (aliasDef[0], aliasDef[1])
        var abbr: NimNode

        if aliasExpr.kind == nnkCall and aliasExpr.len == 2:
            abbr      = aliasExpr[1]
            aliasExpr = aliasExpr[0]

        if not (aliasExpr.kind == nnkDotExpr and 
                aliasExpr.len == 2 and 
                aliasExpr[0].kind == nnkIdent and 
                aliasExpr[1].kind == nnkIdent): aliasExpr.errorTrace(expectedAsIn, "alias definition", "x.alias", "x.alias(abbr)")
            
        let (x, alias) = (aliasExpr[0], aliasExpr[1])

        if not (def.kind == nnkDotExpr and def.len == 2):
            def.errorTrace(expectedAsMaybe, "unit alias implementation", fmt"({def.repr}).unit", fmt"expr({$x}).unit")

        let (expr, unit) = (def[0], def[1])

        result.add getAst(genAlias(alias, x, expr, unit))
        if abbr != nil: result.add getAst(genAlias(abbr, x, expr, unit)) 