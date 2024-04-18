import macros
import units

unitSystem si: 
    Length:          meters(m)
    Mass:            kilograms(kg)
    Time:            seconds(s)
    ElectricCurrent: amperes(A)
    Temperature:     kelvins(K)
    Amount:          moles(mol)

let a = Length(10)
let b = 10.meters
assert b is Length