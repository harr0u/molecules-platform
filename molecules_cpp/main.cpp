#include <iostream>
#include <math.h>
#include <cmath>
#include <vector>
#include <random>
#include <sstream>
#include <iostream>
#include <fstream>
#include <string>

#include "vector2d.h"
#include "particle.h"
#include "Calculator.h"
#include "ParticlesCells.h"
#include <chrono>


std::vector<Particle> make_particles(const long double box_width, const int particles_side_count, const long double velocity_mul) {
    std::vector<Particle> particles = std::vector<Particle>();
    long double delta_width = box_width / particles_side_count;

    std::random_device rd;  //Will be used to obtain a seed for the random number engine
    std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
    std::normal_distribution<> dis(0.0, 1.0);

    for (int i = 0; i < particles_side_count; i++) {
        for (int j = 0; j < particles_side_count; j++) {
            Vector2D center(delta_width * j + 0.5 * delta_width, delta_width * i + 0.5 * delta_width);
            Vector2D velocity = Vector2D(dis(gen), dis(gen)) * velocity_mul;
            Particle p(center, velocity);
            particles.push_back(p);
        }
    }

    Vector2D center_of_mass_velocity(0.0, 0.0);

    for (auto & particle : particles) {
        center_of_mass_velocity = center_of_mass_velocity + particle.velocity * (particle.mass / particles.size());
    }
    for (auto & particle : particles) {
        particle.velocity = particle.velocity - center_of_mass_velocity;
    }
    return particles;
}

const std::string DETAILED_LOG_PATH = "./molecules_cpp/data_detailed.csv";
const std::string LOG_PATH = "./molecules_cpp/data.csv";

void log_values(const std::vector<Particle>& particles, int id, bool detailed) {
    long double kinetic_energy = 0.0;
    long double potential = 0.0;

    std::ofstream detailedFile;
    std::ofstream myfile;

    if (detailed) detailedFile.open(DETAILED_LOG_PATH, std::fstream::app);
    myfile.open(LOG_PATH, std::fstream::app);

    for (auto p : particles) {
        long double velocity_scalar = p.velocity.get_norm();
        long double kinetic_energy_self = velocity_scalar * velocity_scalar / 2;
        kinetic_energy += kinetic_energy_self;
        potential += p.potential;

        if (detailed) {
            detailedFile << id << " " << p.id << " " << p.center.x << " " << p.center.y << " " << p.potential << " " << kinetic_energy_self << std::endl;
        }

    }
    if (detailed) detailedFile.close();

    int number_of_particles = particles.size();
    kinetic_energy /= number_of_particles;
    potential /= number_of_particles;
    long double total_energy = kinetic_energy + potential;


    myfile << kinetic_energy << " " << potential << " " << total_energy << " " << kinetic_energy * 120.0 << std::endl;
    myfile.close();

    std::cout << "Kinetic ~> " << kinetic_energy << "; Potential ~> " << potential
        << "; Total ~> " << total_energy
        << "; Temperature ~> " << kinetic_energy * 120.0 << std::endl;
}

void log_values(const ParticlesCells& cells, int id, bool detailed) {
    std::vector<Particle> particles;

    for (int i = 0; i < cells.cells_side_count; i++) {
        for (int j = 0; j < cells.cells_side_count; j++) {
            for (auto & particle : cells.cells[i][j]) {
                particles.push_back(particle);
            }
        }
    }

    log_values(particles, id, detailed);
}

const int PARTICLES_SIDE_COUNT = 100;
const int PARTICLES_COUNT = PARTICLES_SIDE_COUNT * PARTICLES_SIDE_COUNT;
const long double DENSITY = 0.7;
const long double RADIUS_CUT_OFF = 5;
const long double VELOCITY_MUL = 0.8;
const long double DELTA_TIME = 0.001;
const bool DETAILED_LOG = true;

int main1() {
    Calculator calculator(PARTICLES_COUNT, DENSITY, DELTA_TIME);
    std::cout << "width ~> " << calculator.box_width << std::endl;
    std::vector<Particle> particles = make_particles(calculator.box_width, PARTICLES_SIDE_COUNT, VELOCITY_MUL);

    std::ofstream myfile;
    if (DETAILED_LOG) {
        myfile.open(DETAILED_LOG_PATH);
        myfile.clear();
        myfile << PARTICLES_SIDE_COUNT << " " << DENSITY << " " << DELTA_TIME << " " << calculator.box_width << " " << std::endl;
        myfile.close();
    }

    myfile.open(LOG_PATH);
    myfile.clear();
    myfile.close();

    for (int index = 0; index < 100000; index++) {
        auto start = std::chrono::high_resolution_clock::now();
        calculator.iteration(particles);
        auto stop = std::chrono::high_resolution_clock::now();
        std::cout << index << " " << std::chrono::duration_cast<std::chrono::milliseconds>(stop - start).count() << " ";
        log_values(particles, index, DETAILED_LOG);
    }
    return 0;
}

int main() {
    Calculator calculator(PARTICLES_COUNT, DENSITY, DELTA_TIME);
    std::cout << "width ~> " << calculator.box_width << std::endl;
    std::vector<Particle> particles = make_particles(calculator.box_width, PARTICLES_SIDE_COUNT, VELOCITY_MUL);

    ParticlesCells cells(particles, calculator.box_width, RADIUS_CUT_OFF);

    std::ofstream myfile;
    if (DETAILED_LOG) {
        myfile.open(DETAILED_LOG_PATH);
        myfile.clear();
        myfile << PARTICLES_SIDE_COUNT << " " << DENSITY << " " << DELTA_TIME << " " << calculator.box_width << " " << std::endl;
        myfile.close();
    }

    myfile.open(LOG_PATH);
    myfile.clear();
    myfile.close();

    for (int index = 0; index < 100000; index++) {
        auto start = std::chrono::high_resolution_clock::now();
        calculator.iteration(cells);
        auto stop = std::chrono::high_resolution_clock::now();
        std::cout << index << " " << std::chrono::duration_cast<std::chrono::milliseconds>(stop - start).count() << " ";
        log_values(cells, index, DETAILED_LOG);
    }
    return 0;
}