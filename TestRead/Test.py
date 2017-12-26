# Download Thonny
# Tools -> Manage Packages -> PyUserInput
import pymouse
import pykeyboard
import time
from pymouse import PyMouse
from pykeyboard import PyKeyboard

m = PyMouse()
k = PyKeyboard()

# x_dim, y_dim = m.screen_size() # Set x and y screen sizes
# m.move(x_dim/2, y_dim/2) # Move mouse to location

# Give the user 10 seconds to get the window enlarged and centered
#time.sleep(10)
mouseX, mouseY = m.position()
time.sleep(1)
m.click(int(mouseX), int(mouseY), 1)
#print(m.position())

# Basic use instructions:
# https://github.com/SavinaRoja/PyUserInput/wiki/PyMouse

# Take screenshot
# k.press_keys(['Command','shift','3'])

#import cv2
# OpenCV-Python
#img = cv2.imread("Test.png")
#print(type(img))