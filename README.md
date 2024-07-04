# SpectraOptimizer

Used for preparing Mass Calibration Spectra readings for a machine learning model, the optimizer grid searches for the best scoring calibration(a slope and an offset), and then zooms in and repeats the gridsearch until we cannot find a better score than the one we already have.
