# ShoreTrans (Fortran Version)

This repository contains a Fortran translation of the original [ShoreTrans model](https://github.com/jakmccarroll/ShoreTrans), developed by [McCarroll et al. (2021)](https://doi.org/10.1016/j.margeo.2021.106466). The original MATLAB model simulates long-term (10-100 years) coastal change in response to sea level rise and sediment supply, accounting for sediment budgeting, shoreface translation, and multiple real-world coastal constraints. This version includes all main features but does not include some developments found in the original version (e.g storm demand, dune growth). 

The work was done as part of my Master/PhD work within a collaboration between BRGM and EPOC.

---

## What is ShoreTrans?

ShoreTrans is a rules-based, 1D profile evolution model designed for:

- Dune encroachment and accretion
- Barrier rollback
- Non-erodible layers and hard-rock coasts
- Seawalls and armouring
- Lower shoreface translation
- Alongshore rotation
- Sediment source/sink handling

It applies directly to surveyed coastal profiles, avoiding the need for simplified parameterizations. The model supports sand, gravel, rock, and engineered coastlines.

---

## About This Fortran Version

This version is a direct translation of the MATLAB code into modern Fortran 2008, with the following enhancements:

- Improved root-solving routine (bracketing + Brent's method) for better optimization and numerical stability
- Continuous (sub-grid) translation distance: the volume budget is closed to machine precision independently of the grid resolution
- Support for sub-meter transect resolution (`dx < 1m`) -- enabling high-resolution modeling of complex morphologies
- Modular code structure suitable for batch runs, sensitivity analyses, or integration into larger modeling frameworks


Special thanks to [Jak McCarroll](mailto:jak.mccarroll@unimelb.edu.au) for the original model and approval to release this version.

---

## Repository Structure

    +-- src/ # Fortran source files 
    +-- bin/ # Final compiled executable (created by Makefile) 
    +-- build/ # Object and module files 
    +-- tutorials/ # tutorial files
    +-- Makefile # Build instructions 
    +-- README.md # You are here
    +-- LICENSE


---

## Building the Model

Requirements:
- A Fortran compiler (`gfortran` recommended)
- `make`

To build the model:

```bash
make exe              # builds in release mode
make exe BUILD=debug  # builds in debug mode (extra checks and symbols)
make clean            # removes all build artifacts
```

---

## Running the Model

After building the model with `make`, the executable `shoretrans` will be located in the `bin/` directory.

You can run the model directly like this:

```bash
./bin/shoretrans /path/to/parameter_file.dat
```

Or, for convenience, add the bin/ directory to your system PATH:

```bash
export PATH=$PATH:$(pwd)/bin
```

Then you can simply run:
```bash
shoretrans /path/to/input_file.txt
```

If you omit the input file, running:

```bash
shoretrans
```

will execute the model in the current working directory, expecting the parameter file to be located there.

## License

This project is available under the [Creative Commons Attribution-NonCommercial 4.0 International License](LICENSE). Feel free to share and adapt the material with proper attribution for non-commercial purposes.

