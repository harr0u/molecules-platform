import pygame
import numpy as np
from particles_factory import ParticlesCells

# make abstract if additional visualization
class PygamePainter:
    def __init__(self, screen, scale, **kwargs):
        self._scale = scale
        self._screen = screen
        self._width = screen.get_width()
        self._height = screen.get_height()

        self._background_color = kwargs.get("background_color", (50, 50, 50))
        self._particles_default_color = kwargs.get("particle_color", (255, 255, 255))

    def draw_background(self):
        self._screen.fill(self._background_color)

    def draw_cells(self, particles: ParticlesCells):
        if particles.rows_count <= 1 or particles.cols_count <= 1:
            return

        delta_y = self._width / particles.rows_count
        delta_x = self._height / particles.cols_count

        for i in range(1, particles.rows_count):
            pygame.draw.line(self._screen, (255, 0, 0), (0, delta_y * i), (self._width, delta_y * i), 1)
        for i in range(1, particles.cols_count):
            pygame.draw.line(self._screen, (255, 0, 0), (delta_x * i, 0), (delta_x * i, self._height), 1)


    def draw_particles(self, particles: ParticlesCells):
        self.draw_particles_(particles.iterate_throw_particles())

    def draw_particles_(self, particles):
        self.draw_background()

        for particle in particles:
            scaled_center = np.multiply(particle.center.coordinates, self._scale).astype(int)
            radius = int(particle.radius * self._scale)
            color = particle.color if particle.color else self._particles_default_color
            # pygame.draw.circle(self._screen, color, scaled_center, radius, 2)
            for delta_x in [-self._width, 0, self._width]:
                for delta_y in [-self._height, 0, self._height]:
                    center = np.add(scaled_center, [delta_x, delta_y])
                    pygame.draw.circle(self._screen, color, center, radius, 1)
