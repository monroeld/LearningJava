# Download Thonny
# Tools -> Manage Packages -> PyUserInput
import pymouse
import pykeyboard
from pymouse import PyMouse
from pykeyboard import PyKeyboard

m = PyMouse()
k = PyKeyboard()

x_dim, y_dim = m.screen_size() # Set x and y screen sizes
m.move(x_dim/2, y_dim/2) # Move mouse to location

# print(m.position())
# print(x_dim, y_dim)

# Basic use instructions:
# https://github.com/SavinaRoja/PyUserInput/wiki/PyMouse