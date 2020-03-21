import sys
import pygame
import matplotlib.pyplot as plt

from time import time
from typing import Dict, List
from plot_logger import PlotLogger
from pygame_painter import PygamePainter
from calculator import Calculator, calculate_center_of_mass_velocity
from particles_factory import ParticlesCells
from file_logger import FileLogger


class Board:
    def __init__(self, **kwargs):
        self.width = kwargs.get('width', 100)
        self.height = kwargs.get('height', 100)

        self.window_width = kwargs.get('window_width', 600)
        self.window_height = kwargs.get('window_height', 600)

        self.ticks_per_second = kwargs.get('ticks_per_second', 60)
        self.delta_time = kwargs.get('delta_time', 0.01)
        self.cut_off_distance = kwargs.get('cut_off_distance', 2.5)

        if (not pygame.get_init()):
            pygame.init()

    def start(self, particles: ParticlesCells):
        clock = pygame.time.Clock()
        screen = pygame.display.set_mode((self.window_width, self.window_height))

        scale = self.calculate_and_check_scale()
        painter = PygamePainter(screen, scale)
        logger = PlotLogger()
        fileLogger = FileLogger('last_particles_state', 'csv')
        calculator = Calculator(self.width, self.height, particles, delta_time=self.delta_time, cut_off_distance=self.cut_off_distance)

        center_of_mass_velocity = calculate_center_of_mass_velocity(particles)
        for p in particles.iterate_throw_particles():
            p.velocity.coordinates -= center_of_mass_velocity

        condition = True
        iteration_index = 1
        while condition:
            clock.tick(self.ticks_per_second)

            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    condition = False

            calculator.iteration_2(particles, log_time=(iteration_index % 50 == 0))
            logger.log_particles(particles)

            painter.draw_particles(particles)

            pygame.display.update()

            if (iteration_index % 1000 == 0):
                print(f'Iteration {iteration_index} - DONE')
            iteration_index += 1

        fileLogger.log_particles(particles)
        logger.dispose()

    def calculate_and_check_scale(self):
        x_scale = self.window_width / self.width
        y_scale = self.window_height / self.height

        if (x_scale != y_scale):
            raise Exception()
        return x_scale
