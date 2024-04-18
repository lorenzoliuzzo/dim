import utils
from strformat import fmt

type UnitInfo* = object  ## Contains information on a quantity and its unit.
    quantity: NimNode
    name: NimNode
    abbr: NimNode

proc newUnitInfo*(quantity, name, abbr: NimNode): UnitInfo {.inline.} = 
    UnitInfo(quantity: quantity, name: name, abbr: abbr)

proc destructurize*(info: UnitInfo): (NimNode, NimNode, NimNode) {.inline.} =
    (info.quantity, info.name, info.abbr)

proc quantity*(info: UnitInfo): NimNode {.inline.} = info.quantity
proc name*(info: UnitInfo): NimNode {.inline.} = info.name
proc abbr*(info: UnitInfo): NimNode {.inline.} = info.abbr


proc getUnitInfo*(node: NimNode): UnitInfo =
    ## Checks a node for being a valid quantity declaration and converts it to a UnitInfo object.
    if not node.isIdentCallOne:
        error(fmt"expected quantity declaration, of form 'Quantity: unit(abbr)', but found '{node.repr}'", node)

    let quantity = node[0]
    if quantity.kind != nnkIdent:
        error(fmt"expected quantity name, in 'Quantity: unit(abbr)', but found '{quantity.repr}'", quantity)
        
    let unit = getCompact(node[1])
    if unit.kind != nnkCall:
        if unit.kind == nnkIdent:
            error(fmt"no abbreviated unit name found for '{quantity.repr}: {unit.repr}', '{quantity.repr}: {unit.repr}(abbr)' form expected", unit)
        error(fmt"unit names expected in form '{quantity.repr}: unit(abbr)', but found {unit.repr}", unit)

    if unit.len != 2:
        error(fmt"unit's single abbreviated name expected in form '{quantity.repr}: {unit[0].repr}(abbr)', but found '{callToPar(unit).repr}'", unit)

    let (unitName, unitAbbr) = (unit[0], unit[1])
    if unitName.kind != nnkIdent:
        error(fmt"identifier expected as unit full name in '{quantity.repr}: unit(abbr)', but found '{unitName.repr}'", unitName)

    if unitAbbr.kind != nnkIdent:
        error(fmt"identifier expected as a unit abbreviated name in form '{unitName.repr}(abbr)', but found '{unitAbbr.repr}'", unitAbbr)

    newUnitInfo(quantity, unitName, unitAbbr)


type SystemInfo* = object  ## Contains informations on unit system.
    name:  NimNode
    units: seq[UnitInfo]

proc newSystemInfo*(name: NimNode, units: seq[UnitInfo]): SystemInfo {.inline.} =
    SystemInfo(name: name, units: units)

proc name*(info: SystemInfo): NimNode {.inline.} = info.name
proc units*(info: SystemInfo): seq[UnitInfo] {.inline.} = info.units