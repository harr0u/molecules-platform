from typing import List
from particle import Particle
from vector import Vector2D
from time import time
from particles_factory import ParticlesCells
import math
import numpy as np
class Calculator:
    def __init__(self, width, height, initial_particles, **kwargs):
        self.width = width
        self.height = height
        self.delta_time = kwargs.get('delta_time', 0.001)
        self.cut_off_distance = kwargs.get('cut_off_distance', 2.5)

    def limit_condition_periodic(self, particle):
        particle.center.x %= self.width
        particle.center.y %= self.height

    def iteration(self, particles: ParticlesCells, log_time: bool):
        t1 = time()
        for particle in particles.iterate_throw_particles():
            particle.velocity += particle.acceleration * (self.delta_time / 2)
            particle.translate(particle.velocity * self.delta_time)
            self.limit_condition_periodic(particle)
        particles.update_cells()
        t_before = time()
        self.drop_acceleration_and_potential(particles)
        for particle1, particle2 in particles.iterate_throw_particle_pairs():
            self.recompute_acceleration_and_potential(particle1, particle2)
        t_after = time()

        for particle in particles.iterate_throw_particles():
            particle.velocity += particle.acceleration * (self.delta_time / 2)
        t2 = time()
        if log_time:
            print(f'Total={t2 - t1}. Acceleration = {t_after - t_before}, F1 = {t_before - t1}, F2 = {t2 - t_after}')

    def iteration_2(self, particles: ParticlesCells, log_time: bool):
        t1 = time()
        for particle in particles.iterate_throw_particles():
            particle.translate(particle.velocity * self.delta_time + particle.acceleration * (self.delta_time ** 2 / 2))
            particle.velocity += particle.acceleration * (self.delta_time / 2)
            self.limit_condition_periodic(particle)
        t11 = time()
        particles.update_cells()
        t_before = time()
        self.drop_acceleration_and_potential(particles)
        for particle1, particle2 in particles.iterate_throw_particle_pairs():
            self.recompute_acceleration_and_potential(particle1, particle2)
        t_after = time()

        for particle in particles.iterate_throw_particles():
            particle.velocity += particle.acceleration * (self.delta_time / 2)
        t2 = time()
        if log_time:
            print(f'Total={t2 - t1}. Acceleration = {t_after - t_before}, F1 = {t11 - t1}, F2 = {t_before - t11}')

    def recompute_acceleration_and_potential(self, particle_i: Particle, particle_j: Particle):
        radius_i_j = particle_j.center - particle_i.center

        if abs(radius_i_j.x) > (self.width / 2):
            radius_i_j.x = -(self.width - abs(radius_i_j.x)) * np.sign(radius_i_j.x)
        if abs(radius_i_j.y) > (self.height / 2):
            radius_i_j.y = -(self.height - abs(radius_i_j.y)) * np.sign(radius_i_j.y)
        radius_i_j_scalar = abs(radius_i_j)

        if radius_i_j_scalar < self.cut_off_distance:
            radius_i_j_inverted = 1 / radius_i_j_scalar
            acceleration_i_j = radius_i_j * (-48 * ((radius_i_j_inverted ** 14) - 0.5 * (radius_i_j_inverted ** 8)))
            potential_i_j = 4 * (radius_i_j_inverted**12 - radius_i_j_inverted**6)

            particle_i.acceleration += acceleration_i_j
            particle_j.acceleration -= acceleration_i_j

            particle_i.potential += potential_i_j
            particle_j.potential += potential_i_j

    def drop_acceleration_and_potential(self, particles: ParticlesCells):
        for p in particles.iterate_throw_particles():
            p.acceleration = Vector2D()
            p.potential = 0.0


def get_width_height(particles_count, density):
    length = (particles_count / density) ** 0.5
    return (length, length)


def calculate_center_of_mass_velocity(particles: ParticlesCells):
    # P = M*Vc
    total_momentum = sum([np.multiply(p.velocity.coordinates, p.mass) for p in particles.iterate_throw_particles()])
    total_mass = sum([p.mass for p in particles.iterate_throw_particles()])

    return total_momentum * (1 / total_mass)