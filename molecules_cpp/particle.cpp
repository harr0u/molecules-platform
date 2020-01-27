//
// Created by marka on 25.10.2019.
//

#include "particle.h"

Particle::Particle(Vector2D center, Vector2D velocity) {
    this->id = last_particle_id;
    last_particle_id++;

    this->center = center;
    this->velocity = velocity;
    this->acceleration = Vector2D();
    this->mass = 1;
}

void Particle::translate(Vector2D delta_r) {
    this->center = center + delta_r;
}

std::string Particle::particle2string() {
    std::stringstream ss;
    ss << "(" << center.x << ", " << center.y << ", " << velocity.x << ", " << velocity.y << ")";
    return ss.str();
}
int Particle::last_particle_id = 0;