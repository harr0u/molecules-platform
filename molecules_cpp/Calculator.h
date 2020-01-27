//
// Created by marka on 25.10.2019.
//

#ifndef MOLECULES_CALCULATOR_H
#define MOLECULES_CALCULATOR_H


#include <vector>
#include "particle.h"
#include "ParticlesCells.h"

class Calculator {
public:
    Calculator(int number_of_particles, long double density, long double delta_time);
    long double box_width;
    void iteration(std::vector<Particle> &particles);
    void iteration(ParticlesCells &particles);
    static int signum(long double x);

private:
    int number_of_particles;
    long double delta_time;

    void recompute_acceleration_and_potential(Particle &p1, Particle &p2);
    void limit_condition_periodic(Particle& particle);

    static long double calculate_box_width(int number_of_particles, long double density);
    static void drop_potential_and_acceleration(Particle& particle);
};


#endif //MOLECULES_CALCULATOR_H
