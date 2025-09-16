# video_image_analysis

This repository contains code that was partly used for image analysis, cluster labelling, and estimation of Euler number of droplet evaporation images in the following article:

📄 Roy, A. (2023). *Droplet evaporation study*. AIP Publishing. https://doi.org/10.1063/5.0158179

Please see the [CITATION.cff](CITATION.cff) and [LICENSE](LICENSE) files for details on citation and usage.

---

## 📂 Repository Structure

- `imageread1.py` → Reads an image, applies thresholding, and outputs a `.dat` file.  
- `euler9scale.f` → Takes the `.dat` file as input and generates a coarse-grained matrix at scale 1 in a hexagonal grid. Outputs the Euler characteristic.  
- `eul_r_up.f` → Takes the coarse-grained matrix as input and generates results at higher scales, including number of clusters, Euler number, and normalized Euler number.  

---

## ⚙️ Usage Instructions

### Step 1: Preprocessing
Run the Python script with your image file. Adjust the threshold as required.

```bash
python imageread1.py your_image.png
```

This will generate `your_image.dat`.

---

### Step 2: First Euler Analysis
Compile and run the Fortran program:

```bash
gfortran euler9scale.f -o euler9scale
./euler9scale
```

- Input: `your_image.dat` (labelled `1` in program).  
- Output:
  - File `11` → coarse-grained matrix at one scale higher.  
  - Terminal output → Euler characteristic value (save this manually).  

Parameters:
- `n = original grid size / 2`  
- `r = image size`  

---

### Step 3: Higher Scale Analysis
Use the output (`out1_imagename`) as input for the next Fortran program:

```bash
gfortran eul_r_up.f -o eul_r_up
./eul_r_up
```

- Input: `out1_imagename` (labelled `1`).  
- Outputs:  
  - File `11` → coarse-grained matrix at scale 2.  
  - File `3` → results: number of clusters, Euler number, normalized Euler number.  

Repeat until:
- The desired scale is reached, or  
- The Euler number becomes constant.  

---

##  Notes
- Adjust thresholds and parameters (`n`, `r`) before running.  
- Fortran programs are compiled with `gfortran` and executed as `./a.out` or using the `-o` option to name the executable.  

---

##  Citation
If you use this code in your research, please cite:

Roy, A. (2023). *Droplet evaporation study*. AIP Publishing. https://doi.org/10.1063/5.0158179  

Also see the [CITATION.cff](CITATION.cff) file for machine-readable citation.  

---

##  License
This project is released under the [MIT License](LICENSE).
