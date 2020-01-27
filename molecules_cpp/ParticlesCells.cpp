//
// Created by marka on 25.10.2019.
//

#include "ParticlesCells.h"

ParticlesCells::ParticlesCells(std::vector<Particle> particles, long double box_width, long double radius_cut_off) {
    this->box_width = box_width;
    this->cells_side_count = fmax(box_width / radius_cut_off, 1);

    std::vector<std::vector<std::vector<Particle> > > empty_cells(cells_side_count);

    for (int i = 0; i < this->cells_side_count; i++) {
        for (int j = 0; j < this->cells_side_count; j++) {
            std::vector<Particle> empty_particles;
            empty_cells[i].push_back(empty_particles);
        }
    }

    this->empty_cells = empty_cells;
    std::vector<std::vector<std::vector<Particle> > > cells = this->empty_cells;

    for (auto & particle : particles) {
        int row_index = particle.center.y / this->box_width * this->cells_side_count;
        int col_index = particle.center.x / this->box_width * this->cells_side_count;
        cells[row_index][col_index].push_back(particle);
    }

    this->cells = cells;
}

void ParticlesCells::update_cells() {
    std::vector<std::vector<std::vector<Particle> > > new_cells = this->empty_cells;

    for (int i = 0; i < this->cells_side_count; i++) {
        for (int j = 0; j < this->cells_side_count; j++) {
            auto cell_particles = this->cells[i][j];

            for (auto & particle : cell_particles) {
                int row_index = fmin(particle.center.y / this->box_width * this->cells_side_count, this->cells_side_count - 1);
                int col_index = fmin(particle.center.x / this->box_width * this->cells_side_count, this->cells_side_count - 1);
                new_cells[row_index][col_index].push_back(particle);
            }
        }
    }

    this->cells = new_cells;
}

