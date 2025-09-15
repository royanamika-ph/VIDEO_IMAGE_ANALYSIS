import matplotlib.pyplot as plt
import matplotlib.image as mpimg
import numpy as np
from numpy import exp
#from cluster_labeling import cluster_labeling2d

img_name = '17.35'
img = mpimg.imread(img_name+'.png')
#plt.imshow(img[:,:,2],cmap='gray')
#plt.show()
print(img.shape)
print(img.max(),img.min())
#img_gray = np.zeros((img.shape[:2]))
img_gray1 = np.zeros((img.shape[:2]))
img_gray2 = np.zeros((img.shape[:2]))

img_gray1 = (img[:,:,0]+img[:,:,1]+img[:,:,2])/3
print(img_gray1.shape)
plt.imshow(img_gray1,cmap='gray')
plt.savefig(img_name+'_gray.png')
plt.show()


def gaussian(x, miu, sigma):
  return exp(-(x-miu)**2/(2*sigma**2))

img_shape = img_gray1.shape
for i in range(img_shape[0]):
    for j in range(img_shape[1]):
        dist = ((img_shape[0]/2-i)**2 + (img_shape[1]/2-j)**2)**0.5/((img_shape[0]/2)**2 + (img_shape[1]/2)**2)**0.5
        threshold = 0.19
        #threshold = 0.25*gaussian(dist, miu=0, sigma=0.45) + 0.08*gaussian(dist,0.61, 0.15) -0.2*gaussian(i,miu=900,sigma=200)*gaussian(j,900,200) # 
        if img_gray1[i,j] > threshold:
            img_gray2[i,j] = 1
        

img_gray2  = img_gray2[00:930,00:930]
print(img_gray2.shape)
plt.imshow(img_gray2, cmap='gray')

plt.show()




plt.imshow(img_gray2, cmap='gray')

plt.savefig(img_name+'_binary.png')
plt.show()
file_name = img_name+'.dat'
np.savetxt(file_name, img_gray2, fmt='%i')

