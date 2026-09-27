"""Optional reconstruction of published Figure 2 footprints.
Requires numpy, scipy, Pillow, OpenCV, GDAL CLI, Poppler and the cached
reference rasters described in README.md. Not needed to run the R plots.
"""
from pathlib import Path
import cv2, numpy as np, json, subprocess
from PIL import Image
repo = Path(__file__).resolve().parents[2]
root = repo / 'data/hansen_forest_change_2025/figure2_registration'
subprocess.run(['pdfimages', '-f', '2', '-l', '2', '-png', str(root/'hansen_2013.pdf'), str(root/'paper')], check=True)
fig=np.array(Image.open(root/'paper-001.png'))
boxes={'paraguay':(0,227,1251,975),'indonesia':(1276,227,2526,975),'usa':(0,994,1251,1739),'russia':(1276,994,2526,1739)}
sift=cv2.SIFT_create(nfeatures=16000,contrastThreshold=.015)
for slug,box in boxes.items():
 path=root/(f'{slug}_reference_north.tif' if slug=='indonesia' else f'{slug}_reference.tif')
 if not path.exists(): continue
 try: ref=np.array(Image.open(path))
 except:continue
 if ref.max()==0:continue
 x0,y0,x1,y1=box
 patch=fig[y0:y1,x0:x1]
 gray=patch[:,:,1].copy()
 # Ignore coloured change overlays and scale bars when selecting paper features.
 mask=((patch[:,:,1].astype(float)>patch[:,:,0]*1.15)&(patch[:,:,1].astype(float)>patch[:,:,2]*1.15) | (patch.max(axis=2)<35)).astype('uint8')*255
 mask[-90:,:]=0
 refgray=np.uint8(np.clip(ref.astype(float)*255/80,0,255))
 k1,d1=sift.detectAndCompute(gray,mask);k2,d2=sift.detectAndCompute(refgray,None)
 matches=cv2.BFMatcher().knnMatch(d1,d2,k=2)
 good=[m for m,n in matches if m.distance<.7*n.distance]
 p1=np.float32([k1[m.queryIdx].pt for m in good]); p2=np.float32([k2[m.trainIdx].pt for m in good])
 M,inliers=cv2.estimateAffine2D(p1,p2,method=cv2.RANSAC,ransacReprojThreshold=3,maxIters=10000)
 if M is None:print(slug,'failed');continue
 gt=json.loads(subprocess.check_output(['gdalinfo','-json',str(path)]))['geoTransform']
 corners=np.float64([[0,0,1],[x1-x0,0,1],[x1-x0,y1-y0,1],[0,y1-y0,1]])@M.T
 ll=[[gt[0]+(x+.5)*gt[1],gt[3]+(y+.5)*gt[5]] for x,y in corners]
 residual=np.linalg.norm(np.c_[p1,np.ones(len(p1))]@M.T-p2,axis=1)
 print(slug,'matches',len(good),'inliers',int(np.sum(inliers[:,0])),'median error',np.median(residual[inliers[:,0]>0]),'matrix',M.tolist(),'corners',ll,flush=True)
 np.savez(root/f'{slug}_matches.npz',paper=p1,reference=p2,inliers=inliers,affine=M,geotransform=gt,box=box)

# Fit north-up Mercator axes to robustly matched features.
import csv
from scipy.optimize import least_squares
rows=[]
for slug, box in boxes.items():
 z=np.load(root/f'{slug}_matches.npz')
 keep=z['inliers'].ravel()>0
 p=z['paper'][keep]; q=z['reference'][keep]; gt=z['geotransform']
 lon=gt[0]+(q[:,0]+.5)*gt[1]; lat=gt[3]+(q[:,1]+.5)*gt[5]
 R=6378137.0
 xy=np.c_[R*np.deg2rad(lon), R*np.log(np.tan(np.pi/4+np.deg2rad(lat)/2))]
 fits=[]
 for axis in range(2):
  A=np.c_[p[:,axis],np.ones(len(p))]
  initial=np.linalg.lstsq(A,xy[:,axis],rcond=None)[0]
  fits.append(least_squares(lambda c:A@c-xy[:,axis], initial, loss='soft_l1', f_scale=150).x)
 x=np.array([0,box[2]-box[0]])*fits[0][0]+fits[0][1]
 y=np.array([0,box[3]-box[1]])*fits[1][0]+fits[1][1]
 longitude=np.rad2deg(x/R); latitude=np.rad2deg(2*np.arctan(np.exp(y/R))-np.pi/2)
 predicted=np.c_[p[:,0]*fits[0][0]+fits[0][1],p[:,1]*fits[1][0]+fits[1][1]]
 error=np.linalg.norm(predicted-xy,axis=1)*np.cos(np.deg2rad(lat))
 rows.append(dict(slug=slug,xmin=longitude.min(),xmax=longitude.max(),ymin=latitude.min(),ymax=latitude.max(),matched_features=len(p),median_match_error_m=np.median(error)))
with (repo/'analysis/hansen_support/figure2_bounds.csv').open('w') as f:
 writer=csv.DictWriter(f,fieldnames=rows[0].keys());writer.writeheader();writer.writerows(rows)
print(rows)
