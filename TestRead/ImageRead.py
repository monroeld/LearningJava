import numpy
from PIL import Image

import pymouse
from pymouse import PyMouse

m = PyMouse()

img = Image.open("Test.png")

imgarray = numpy.array(img)


# Attack = (460, 300) (Comes from mouse position call)
# healthStart = (560, 625) (Calculate based on attack position)
# healthEnd = (685, 625) (Calculate based on attack position)

# Everything needs to be doubled for some reason...

red = imgarray[1250, 1120:1370, 1]
print(red)
green = imgarray[1250, 1120:1370, 2]
print(green)


#for y in range(1120, 1380):
#    print(imgarray[1250, y, 1])
    #for x in range(0, imgarray.shape[0]):
        #redArray[y, x] = imgarray[y, x, 1]
        #greenArray[y, x] = imgarray[y, x, 2]
        #blueArray[y, x] = imgarray[y, x, 3]

#print(redArray[3, 54])
#mousePos = m.position()
#mouseX, mouseY = mousePos
#print(mouseX, mouseY)
#print(redArray[int(mouseX), int(mouseY)])

#print(redArray[1120:1370, 1250])
#print(blueArray[1120:1370, 1250])
#print(greenArray[1120:1370, 1250])