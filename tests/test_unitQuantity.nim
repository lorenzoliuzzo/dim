import units
import test_unitSystem

unitQuantity: 
    Velocity = Length / Time
    Frequency = Time^-1

let 
    a = 1.0.meters
    b = 2.0.seconds

assert a / b is Velocity
assert 1 / b is Frequency
assert b^-1 is Frequency

let c = 3.0 * a
let d = a + c
assert d is Length