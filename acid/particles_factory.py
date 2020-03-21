from particle import Particle
from vector import Vector2D
from random import random
import numpy as np

class ParticlesCells:
    def __init__(self, width, height, cut_off_distance):
        self.width = width
        self.height = height
        self.rows_count = max(int(height / cut_off_distance), 1)
        self.cols_count = max(int(width / cut_off_distance), 1)
        self.cut_off_distance = cut_off_distance
        self.cells = []

    def update_cells(self, particles=None):
        if particles is None:
            particles = self.iterate_throw_particles()

        cells = [[[] for j in range(self.cols_count)] for i in range(self.rows_count)]
        for particle in particles:
            row_index = int(particle.center.y / self.height * self.rows_count)
            col_index = int(particle.center.x / self.width * self.cols_count)
            cells[row_index][col_index].append(particle)

        self.cells = cells

    def iterate_throw_particle_pairs(self):
        for i in range(self.rows_count):
            for j in range(self.cols_count):
                for particle1 in self.cells[i][j]:
                    for cell_di in (-1, 0, 1):
                        for cell_dj in (-1, 0, 1):
                            adjacent_cell_i = (i + cell_di) % self.rows_count
                            adjacent_cell_j = (j + cell_dj) % self.cols_count
                            if adjacent_cell_i < i or (adjacent_cell_i == i and adjacent_cell_j < j):
                                continue

                            for particle2 in self.cells[adjacent_cell_i][adjacent_cell_j]:
                                if particle1.id != particle2.id:
                                    yield particle1, particle2

    def iterate_throw_particles(self):
        for i in range(self.rows_count):
            for j in range(self.cols_count):
                cell = self.cells[i][j]
                for particle in cell:
                    yield particle


def make_particles(width, height, particles_count, cut_off_distance, velocity_mul=0):
    cols_count = int((particles_count * width / height)**0.5)
    rows_count = int(particles_count // cols_count)

    delta_w = width / cols_count
    delta_h = height / rows_count

    particles = []
    for i in range(rows_count):
        for j in range(cols_count):
            particle = Particle(
                center=Vector2D(
                    delta_w * j + 0.5 * delta_w,
                    delta_h * i + 0.5 * delta_h
                ),
                radius=0.5,
                velocity=Vector2D(random() - 0.5, random() - 0.5) * velocity_mul
            )
            particles.append(particle)

    cells = ParticlesCells(width, height, cut_off_distance)
    cells.update_cells(particles)

    return cells


def load_particles_from_file(width: float, height: float, cut_off_distance, file_name):
    with open(file_name) as file:
        particles = [Particle.string2particle(line.strip()) for line in file.readlines()]

    cells = ParticlesCells(width, height, cut_off_distance)
    cells.update_cells(particles)

    return cells
