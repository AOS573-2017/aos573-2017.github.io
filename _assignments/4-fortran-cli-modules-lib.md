---
layout: assignment
published: false
title: Fortran CLI, Modules, and Libraries
---

Today we are going to write a Fortran module that will perform a few thermodynamic conversions. Included in the `fortran-week-three` directory you cloned in the assignment is a file `homework.f90` that will be using the module. Go ahead and open the file and see the expected subroutine calls.

Thus, we need to build out our `thermo` module to include functions for the following conversions:

* `POTTEMP`, which calculates the potential temperature at a given pressure and temperature. `theta = Tk * ( p0 / p ) ** kappa`, where `kappa` is `Rd / cp` (`Rd = 287.0`, `cp = 1004.0`) and `p0 = 1000000` Pa. All pressures are assumed to be Pascals and all temperatures Kelvin.
* `EVAP`, which calculates the vapor pressure. `e = rhov * Rv * Tk`, where `rhov` is the density of water in kg per cubic meter, `Rv = 461.5` J per kg per K, and `Tk` is the given temperature in Kelvin. `e` will be in units of Pascals.
* `EVAPSAT`, which calculates the saturation vapor pressure. `es = a * LOG( (b * Tc) / (Tc + c) )`, where `a = 611.2` Pa, `b = 17.67`, and `Tc` is a given temperature in Celsius. `es` will be in units of Pascals.
* `STDATMO`, which calculates the pressure of a given tropospheric temperature level for the U.S. Standard Atmosphere. `pstd = p0 * ( Tklayer / Tk0 ) ** ( g / (Rd * Lapse) )`, where `p0 = 1013200` Pa, `Tklevel` is the temperature in Kelvin of the level to calculate the standard atmosphere at, `Tk0 = 288` K, `g = 9.81`, `Rd = 287.0`, and `Lapse = 6.5E-3` K/m. `pstd` will be in Pascals.
* `RH`, which calculates the relative humidity. `rh = (e / es) * 100.0`, where `e` is the actual vapor pressure in Pascals and `es` is the saturation vapor pressure in pascals. `rh` will be a percentage.

Using `./compile_hw.f90`, you can compile your subroutines and the homework and it will create an executable called `./hw`. This command will call the program with your module. 

Please submit `thermo.f90` to the Learn@UW dropbox for Fortran Week 3.
