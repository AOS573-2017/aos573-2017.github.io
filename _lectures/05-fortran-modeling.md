---
layout: lecture
published: true
title: Numerical Modeling
---

# Prelude

Today we will do some actual modeling with Fortran. You may have known that Fortran is not the easiest language to learn and code in, nor is it the most versatile or most elegant. The fact that a majority of our current climate and weather models are written in Fortran is mostly due to historical reasons. However, two of Fortran's biggest strengths, computational efficiency and array capability, are ideal for the coding of large and complex models, at least at the time when these models are built initially.

But first, we need to clone the repository for this week.

```bash
$ git clone https://github.com/aos573/fortran-week-four
$ cd fortran-week-four
```

# Numerical Modeling

By definition from the American Meteorological Society (AMS), numerical modeling is the "numerical construction of approximate solutions to the governing equations", or in more details, the process when "solutions are obtained by assigning discrete values to temporal and spatial derivatives in order to convert the governing differential equations into algebraic equations that can be solved by using computational methods." This basically sums up what climate and weather models do. Today we will try to numerically model a simple dynamic system with only temporal derivatives, or in other words, ordinary differential equations (ODEs).

A common dynamic model consisting of ODEs looks like this:

<img src="https://raw.githubusercontent.com/AOS573/aos573.github.io/master/_lectures/week5/Picture1.png" height="400">

Some ODEs can be solved analytically, namely their solutions can be described by some exact formula. A simple example is `dx/dt=2x`, the solution to which is `x=x^2+C`, where `C` is a given constant. However, most ODEs we encounter will be impossibly hard to solve analytically. With the powerful computers today, we non-mathematicians usually rely on the numerical methods.

## The Lorenz-63 model

The dynamic system we will model today is widely known as the Lorenz-63 model. It came from the groundbreaking 1963 paper ["Deterministic Nonperiodic Flow"](http://dx.doi.org/10.1175/1520-0469(1963)020%3C0130:DNF%3E2.0.CO;2) by [Edward Norton Lorenz](https://en.wikipedia.org/wiki/Edward_Norton_Lorenz). The system consists of 3 equations and 3 variables:

![alt text](https://wikimedia.org/api/rest_v1/media/math/render/svg/5f993e17e16f1c3ea4ad7031353c61164a226bb8 "Lorenz-63 Equations")

The default parameter values are σ=10, β=8/3 and ρ=28. This paper and the Lorenz-63 model marked the origin of the chaos theory. It's also the origin of the "butterfly effect" and the 2-week upper limit for possible weather forecast.

<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/7/71/Lorenz_system_r28_s10_b2-6666.png/1920px-Lorenz_system_r28_s10_b2-6666.png" width="500">

## Integration Schemes

In order to solve ODEs, we need to solve how the model state (x, y, z) moves with time using time integration schemes. These schemes are methods to assign discrete values to temporal derivatives so that we can approximate the movement of the model state in time. Some popular schemes include the Forward Euler scheme, the leapfrog scheme and the Runge-Kutta schemes.

Forward Euler scheme:

<img src="https://raw.githubusercontent.com/AOS573/aos573.github.io/master/_lectures/week5/Picture2.png" height="100">

Fortran expression:
```f90
FUNCTION forward(x, dt)
    IMPLICIT NONE
    REAL, INTENT(in) :: x, dt
    REAL :: forward
    forward = x + dt * F(x)
END FUNCTION
```

Leapfrog scheme:

<img src="https://raw.githubusercontent.com/AOS573/aos573.github.io/master/_lectures/week5/Picture3.png" height="100">

Runge-Kutta scheme (order 4):

<img src="https://raw.githubusercontent.com/AOS573/aos573.github.io/master/_lectures/week5/Picture4.png" height="500">

# Let's code!

## Model Equations

The coding of the model equations are fairly straightforward. There are two options to treat the three equations. You can either write all three in one subroutine or each one in a separate function. I prefer to use function for simple calculations because it's easier to integrate into the code and harder to mess up the input variables. You can certainly use either function or subroutine for the model equations. We will put all the functions and subroutines we need today in a module. Let's call it integrations.

```f90
MODULE integrations
    IMPLICIT NONE
    CONTAINS
END MODULE integrations
```

Let's put the 3 equations (`eqx`, `eqy`, `eqz`) in the form of Fortran code.

```f90
FUNCTION eqx(x,y,z)

    IMPLICIT NONE
    REAL, PARAMETER :: a = 10.0
    REAL, INTENT(in) :: x, y, z
    REAL :: eqx
    eqx = a * ( y - x )

END FUNCTION eqx

FUNCTION eqy(x,y,z)

    IMPLICIT NONE
    REAL, PARAMETER :: b = 28.0
    REAL, INTENT(in) :: x, y, z
    REAL :: eqy
    eqy = b * x - y - x * z

END FUNCTION eqy

FUNCTION eqz(x,y,z)

    IMPLICIT NONE
    REAL, PARAMETER :: c = 8.0 / 3.0
    REAL, INTENT(in) :: x, y, z
    REAL :: eqz
    eqz = x * y - c * z

END FUNCTION eqz
```

## Integrations

Subroutines are better suited to handle the integration schemes with multiple input and output variables. In the simple Euler forward scheme, all we need are the initial model state (`x0, y0, z0`) and a timestep length (`dt`). The output will be the new model state at the next timestep (`x1, y1, z1`).

```f90
SUBROUTINE forward(x0,y0,z0,x1,y1,z1,dt)

    IMPLICIT NONE
    REAL, INTENT(in) :: x0, y0, z0, dt
    REAL, INTENT(out) :: x1, y1, z1

    x1 = eqx(x0,y0,z0) * dt + x0
    y1 = eqy(x0,y0,z0) * dt + y0
    z1 = eqz(x0,y0,z0) * dt + z0

END SUBROUTINE forward
```

You can try other integration schemes later. Some of the schemes require model states at the two or more timesteps to generate the next model state.

## Main program

With all the subroutines and functions ready, let's finish the main program.

```f90
PROGRAM Lorezn63
    USE integrations
    IMPLICIT NONE

    ! Variables
    INTEGER :: i       !For loop
    INTEGER :: N       !Number of cycles
    REAL :: dt         !Integration step length
    REAL :: x0, y0, z0, x1, y1, z1  !current and next model state

    !Set the initial conditions and parameters
    x0 = 0
    y0 = 0
    z0 = 0
    dt = 0.01
    N = 10000

    !Open a file to write the output of the model
    OPEN ( Unit = 8, File = 'Lorenz63.txt', Action = 'WRITE')
    WRITE ( 8, * ) x0, y0, z0

    DO i = 1, N
      CALL forward(x0,y0,z0,x1,y1,z1,dt)
      x0 = x1; y0 = y1; z0 = z1
      WRITE ( 8, * ) x0, y0, z0
    END DO

    CLOSE ( 8 )

END PROGRAM Lorezn63
```

For now, the output of the model is only in text format. We definitely want to see the results and admire the genius of the chaos theory. In the repository we cloned for this week, there is a piece of matlab code called `plot_lorenz.m` that can be used to plot the output. We will not get into the details of the code today, as you will learn about it in the next few weeks.

Copy `plot_lorenz.m` into the folder where the Lorenz-63 model and output are. Open Matlab and navigate to the that directory. Then you can either run `plot_lorenz.m` from the Matlab command line, or open the file and run it from the editor. The code will generate two figures, one with the timeseries of the three variables and the other with a 3D visualization.

Sometimes we want to change the initial conditions, the timestep length and the number of integrations. If these parameters are "hardcoded" into the program, I have to recompile the program every time I want to change the parameters. There are two ways to avoid recompilation. The first is to input the parameters via the command line as we learned last week. The second and better way is to write the desired parameters in a file and read them in the program.

# Lab Assignment

[Follow this link to our assignment](/assignments/05-fortran-modeling.html).
