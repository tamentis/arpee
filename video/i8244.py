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
The Intel 8244 was a custom IC built by Intel for Magnavox. It was used in the
Odyssey2 a.k.a. Philips Videopac.

Here are a few facts accumulated from various sources:

    - sister PAL IC is i8245.
    - 160x200 resolution (NTSC)
    - 16 color fixed palette
    - 8x8 sprites (8 colors max)
    - mono sound output

It's not quite a GPU in today's terms but fsck it, close enough.
"""


class GPU_i8244(object):

    def __init__(self):
        pass
