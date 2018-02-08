# Download Thonny
# Tools -> Manage Packages -> PyUserInput
import pymouse
import pykeyboard
import time
import pyscreeze
import random
from pymouse import PyMouse
from pykeyboard import PyKeyboard

m = PyMouse()
k = PyKeyboard()

# x_dim, y_dim = m.screen_size() # Set x and y screen sizes
# m.move(x_dim/2, y_dim/2) # Move mouse to location

# Give the user 3 seconds to get the window positioned for the attack button
# time.sleep(3)
#mouseX, mouseY = m.position()

attackX = int(m.position()[0])
attackY = int(m.position()[1])

# Take a screenshot to define RGB of attack location
im = pyscreeze.screenshot()
attackR = pyscreeze.pixel(attackX*2, attackY*2)[0]
attackG = pyscreeze.pixel(attackX*2, attackY*2)[1]
attackB = pyscreeze.pixel(attackX*2, attackY*2)[2]
#m.click(int(mouseX), int(mouseY), 1)
#pyscreeze.pixel(m.position()[0], m.position()[1])

print("Starting")

reps = 0

while reps in range(0, 3):
    
# Take a fresh screenshot for analysis
    im = pyscreeze.screenshot()

# If there's white text at the attack site, click 'attack' and continue
    if pyscreeze.pixelMatchesColor(attackX*2, attackY*2 (attackR, attackG, attackB), tolerance = 3):
        print("Click Attack")
        m.click(attackX, attackY, 1)
        time.sleep(15)
        
# If it's the scroll background color, the battle is over
# The scroll doesn't actually contribute to the color...
    elif pyscreeze.pixelMatchesColor(attackX*2, (attackY+150)*2, (39, 36, 53), tolerance = 3):
# Click 'Next' twice in case of z-token
        m.click(attackX, attackY+230, 1)
        time.sleep(0.5)
        m.click(attackX, attackY+230, 1)
        time.sleep(1)
# Move to the battle button relative to (attackX,attackY)
        m.click(attackX-200, attackY + 130, 1)
        reps = reps+1
        print(reps)
        
# If that didn't work, let it sleep a little longer
    else:
        print("Waiting...")
        time.sleep(3)
        
# Battle should start over again

print("That was 3!")
    
    
#print(im.getpixel(m.position()))

# Mix things up a bit
#print(random.randint(1, 100))


# Basic use instructions:
# https://github.com/SavinaRoja/PyUserInput/wiki/PyMouse

# Take screenshot
# k.press_keys(['Command','shift','3'])

import cv2
# OpenCV-Python
img = cv2.imread("Test.png")
print(type(img))