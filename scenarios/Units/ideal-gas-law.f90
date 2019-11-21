module ideal_gas_law
  !! author: Damian Rouson and Magne Haveraaen
  !!
  !! 1. Encapsulate the 3D scalar field representations of the quantities involved
  !!    one form of the ideal gas law: pressure, density, gas constant, and temperature.
  !! 2. Gather large, contiguous data arrays into derived types, each with a
  !!    component whose type declares the units of the corresponding to data array
  implicit none

  type kg_per_mXX3 !! kilogram/meter^3
  end type

  type density_field
    real, allocatable :: values(:,:,:)
    type(kg_per_mXX3) units
  contains
    procedure x_gas_constant !! post-multiply gas_constant
    generic :: operator(*) => x_gas_constant
  end type

  type Pa_per_K !! Pascal/Kelvin
  end type

  type density_x_gas_constant
    real, allocatable :: values(:,:,:)
    type(Pa_per_K) units
  contains
    procedure x_temperature !! post-multiply temperature_field
    generic :: operator(*)=>x_temperature
  end type

  type Pa_mXX3_per_kg_K
  end type

  type gas_constant
    real value_
    type(Pa_mXX3_per_kg_K) units
  end type

  type K !! Kelvin
  end type

  type temperature_field
    real, allocatable :: values(:,:,:)
    type(K) units
  end type

  type Pa !! Pascal
  end type

  type pressure_field
    real, allocatable :: values(:,:,:)
    type(Pa) units
  end type

contains

   pure module function x_gas_constant(this,R) result(this_x_R)
     class(density_field), intent(in) :: this
     type(gas_constant), intent(in) :: R
     type(density_x_gas_constant)  this_x_R
     this_x_R%values = this%values * R%value_
   end function

   pure module function x_temperature(this,T) result(this_x_T)
     class(density_x_gas_constant), intent(in) :: this
     type(temperature_field), intent(in) :: T
     type(pressure_field) this_x_T
     this_x_T%values = this%values * T%values
   end function

end module ideal_gas_law

program main
  !! author: Damian Rouson and Magne Haveraaen
  !!
  !! Demonstrate ideal gas-law example and discuss design alternatives
  use ideal_gas_law, only : &
     pressure_field, density_field, gas_constant, temperature_field, &
     kg_per_mXX3, Pa_mXX3_per_kg_K, K
  implicit none

  real, parameter :: R_air = 8.314 / 27.

  type(pressure_field) :: p
  type(density_field) :: rho = density_field( reshape([real :: 0.], [1,1,1]), kg_per_mXX3())
  type(gas_constant) :: R = gas_constant( R_air, Pa_mXX3_per_kg_K())
  type(temperature_field) :: T = temperature_field( reshape([real :: 0.], [1,1,1]), K())

   p = rho*R*T  !! dimensional consistency: Pa = (kg/m^3) * (Pa*m^3/(kg*K)) * (*K) = (Pa/K)*K = Pa
     !! Alternatives:
     !! 1. Unit types are components checked at runtime by user-written code.
     !! 2. Unit types could be parent types to automate type-checking buti currently this polymorphism imposes overhead.
     !! 3. Generic approach: all types are instantiated to real

end program
