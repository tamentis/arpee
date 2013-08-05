# Copyright (c) 2013 Bertrand Janin <b@janin.com>
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

"""
Magnavox Odyssey2 / Philips Videopac / G7000
"""

from cpu.i8048 import CPU_i8048
from video.i8244 import GPU_i8244


class Odyssey2(object):

    def __init__(self):
        # Prepare the CPU.
        self.cpu = CPU_i8048()
        self.cpu.external_ram = [0] * 128

        # Prepare the VDC/GPU.
        self.vdc = GPU_i8244()

    def load_bios(self, filepath):
        self.cpu.load_bios(filepath)

    def load_data(self, filepath):
        self.cpu.load_data(filepath)

    def run(self):
        self.cpu.run()
