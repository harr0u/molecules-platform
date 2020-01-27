//
// Created by marka on 25.10.2019.
//

#include "Calculator.h"

Calculator::Calculator(int number_of_particles, long double density, long double delta_time) {
    this->number_of_particles = number_of_particles;
    this->delta_time = delta_time;
    this->box_width = Calculator::calculate_box_width(number_of_particles, density);
}

int Calculator::signum(long double x) {
    if (x == 0.0) {
        return 0;
    } else if (x < 0) {
        return -1;
    } else {
        return 1;
    }
}

void Calculator::recompute_acceleration_and_potential(Particle &p1, Particle &p2) {
    // std::cout << p1.id << " " << p2.id << std::endl;

    Vector2D radius_vector = p2.center - p1.center;
    if (abs(radius_vector.x) > this->box_width / 2) {
        radius_vector.x = -(this->box_width - abs(radius_vector.x)) * Calculator::signum(radius_vector.x);
    }
    if (abs(radius_vector.y) > this->box_width / 2) {
        radius_vector.y = -(this->box_width- abs(radius_vector.y)) * Calculator::signum(radius_vector.y);
    }
    long double radius_vector_scalar = radius_vector.get_norm();

    long double radius_vector_inverted = 1 / radius_vector_scalar;
    long double rr = radius_vector_inverted * radius_vector_inverted;
    long double rrr = radius_vector_inverted * rr;
    long double r6 = rrr * rrr;
    long double r12 = r6 * r6;
    Vector2D acceleration = radius_vector * (-48 * (r12 * rr - 0.5 * r6 * rr));
    long double potential = 2 * (r12 - r6);
    p1.acceleration = p1.acceleration + acceleration;
    p2.acceleration = p2.acceleration - acceleration;

    p1.potential += potential;
    p2.potential += potential;
}

void Calculator::limit_condition_periodic(Particle &particle) {
    if (particle.center.x >= box_width) {
        particle.center.x -= box_width;
    } else if (particle.center.x < 0) {
        particle.center.x += box_width;
    }
    if (particle.center.y >= box_width) {
        particle.center.y -= box_width;
    } else if (particle.center.y < 0) {
        particle.center.y += box_width;
    }
}

void Calculator::iteration(std::vector<Particle> &particles) {
    for (auto & particle : particles) {
        particle.center = particle.center + particle.velocity * this->delta_time + particle.acceleration * (this->delta_time * this->delta_time / 2);
        particle.velocity = particle.velocity + particle.acceleration * (this->delta_time / 2);
        limit_condition_periodic(particle);
        Calculator::drop_potential_and_acceleration(particle);
    }

    for (int i = 0; i < this->number_of_particles - 1; i++) {
        for (int j = i+1; j < this->number_of_particles; j++) {
            if (particles[i].id != particles[j].id)
                recompute_acceleration_and_potential(particles[i], particles[j]);
        }
    }
    for (auto & particle : particles) {
        particle.velocity = particle.velocity + particle.acceleration * (this->delta_time / 2);
    }
}

void Calculator::iteration(ParticlesCells &cells) {
    for (int i = 0; i < cells.cells_side_count; i++) {
        for (int j = 0; j < cells.cells_side_count; j++) {
            auto & cell_particles = cells.cells[i][j];
            for (auto & particle : cell_particles) {
                particle.center = particle.center + particle.velocity * this->delta_time + particle.acceleration * (this->delta_time * this->delta_time / 2);
                particle.velocity = particle.velocity + particle.acceleration * (this->delta_time / 2);
                limit_condition_periodic(particle);
                Calculator::drop_potential_and_acceleration(particle);
            }
        }
    }

    cells.update_cells();

    for (int i = 0; i < cells.cells_side_count; i++) {
        for (int j = 0; j < cells.cells_side_count; j++) {
            auto & cell1_particles = cells.cells[i][j];

            for (int particle1_index = 0; particle1_index + 1 < cell1_particles.size(); particle1_index++) {
                for (int particle2_index = particle1_index + 1; particle2_index < cell1_particles.size(); particle2_index++) {
                    auto & p1 = cell1_particles[particle1_index];
                    auto & p2 = cell1_particles[particle2_index];

                    recompute_acceleration_and_potential(p1, p2);
                }
            }

            for (auto & particle1 : cell1_particles) {
                for (int cell_di = -1; cell_di <= 1; cell_di++) {
                    for (int cell_dj = -1; cell_dj <= 1; cell_dj++) {
                        if (cell_di == 0 && cell_dj == 0) {
                            continue;
                        }

                        int adjacent_cell_i = i + cell_di;
                        int adjacent_cell_j = j + cell_dj;

                        if (adjacent_cell_i < 0) adjacent_cell_i += cells.cells_side_count;
                        if (adjacent_cell_i >= cells.cells_side_count) adjacent_cell_i -= cells.cells_side_count;
                        if (adjacent_cell_j < 0) adjacent_cell_j += cells.cells_side_count;
                        if (adjacent_cell_j >= cells.cells_side_count) adjacent_cell_j -= cells.cells_side_count;

                        if (adjacent_cell_i < i || (adjacent_cell_i == i && adjacent_cell_j < j)) {
                            continue;
                        }

                        auto & cell2_particles = cells.cells[adjacent_cell_i][adjacent_cell_j];
                        for (auto & particle2 : cell2_particles) {
                            recompute_acceleration_and_potential(particle1, particle2);
                        }
                    }
                }
            }
        }
    }

    for (int i = 0; i < cells.cells_side_count; i++) {
        for (int j = 0; j < cells.cells_side_count; j++) {
            auto & cell_particles = cells.cells[i][j];
            for (auto & particle : cell_particles) {
                particle.velocity = particle.velocity + particle.acceleration * (this->delta_time / 2);
            }
        }
    }
}

void Calculator::drop_potential_and_acceleration(Particle & particle) {
    particle.potential = 0.0;
    particle.acceleration = Vector2D(0.0, 0.0);
}

long double Calculator::calculate_box_width(int number_of_particles, long double density) {
    long double length = sqrtf(number_of_particles) / sqrtf(density);
    return length;
}

