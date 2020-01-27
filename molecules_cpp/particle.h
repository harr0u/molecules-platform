//
// Created by marka on 25.10.2019.
//

#ifndef MOLECULES_PARTICLE_H
#define MOLECULES_PARTICLE_H

#include "iostream"
#include "sstream"
#include "vector2d.h"

class Particle {
public:
    static int last_particle_id;
    Vector2D center, velocity, acceleration;
    float potential, mass;
    int id;
    Particle(Vector2D center, Vector2D velocity);

    void translate(Vector2D delta_r);

    std::string particle2string();
};

#endif //MOLECULES_PARTICLE_H