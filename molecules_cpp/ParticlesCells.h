//
// Created by marka on 25.10.2019.
//

#ifndef MOLECULES_PARTICLESCELLS_H
#define MOLECULES_PARTICLESCELLS_H


#include <vector>
#include "particle.h"

class ParticlesCells {
public:
    int cells_side_count;
    std::vector<std::vector<std::vector<Particle> > > cells;

    ParticlesCells(std::vector<Particle> particles, long double box_width, long double radius_cut_off);

    void update_cells();

private:
    std::vector<std::vector<std::vector<Particle> > > empty_cells;
    long double box_width;
};


#endif //MOLECULES_PARTICLESCELLS_H
