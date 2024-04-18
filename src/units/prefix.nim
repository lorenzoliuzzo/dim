import utils

{.push hint[XDeclaredButNotUsed]:off.}  # 'T is declared but not used'

type UnitPrefixed* = concept type T
    ## Float with a unit prefix (but no unit yet).
    T.hasUnitPrefix

{.pop.}

type floatMaybePrefixed* = float or int or UnitPrefixed

proc hasUnitPrefix*(a: typedesc[float]): bool =
    ## Informs that bare float has no prefix.
    false

proc hasUnitPrefix*[T: not typedesc](a: T): bool =
    ## Alias for type-based version of itself.
    typedesc[T].hasUnitPrefix