cmake_minimum_required(VERSION 3.15)
project(molecules)

set(CMAKE_CXX_STANDARD 17)

add_executable(molecules main.cpp vector2d.cpp vector2d.h particle.cpp particle.h Calculator.cpp Calculator.h ParticlesCells.cpp ParticlesCells.h)