import os
import datetime
from particles_factory import ParticlesCells
from particle import Particle

class FileLogger:
    def __init__(self, file_name, file_extension='csv'):
        log_folder = '.log'

        if (not os.path.isdir(log_folder)):
            os.mkdir(log_folder)

        # now = str(datetime.datetime.now()).replace(' ', '-')
        self.file_path = f"{file_name}.{file_extension}"
        self.file = open(self.file_path, 'w+')

    def log_particles(self, particles: ParticlesCells):
        data = [str(p) + '\n' for p in particles.iterate_throw_particles()]
        self.file.writelines(data)

    def dispose(self):
        self.file.close()
