# data and stratum printouts and summaries

    Code
      synthetic_survey()
    Output
      su_images
      Data citation: No Data Citation
      Stratum: all (none selected)
      Images: 600
      Taxa: 40
      Individuals: 3220
      Seabed area: 4377.58 m²
      Other metadata: Substratum, Habitat, Latitude, Longitude
    Code
      sand
    Output
      su_images
      Data citation: No Data Citation
      Stratum: Substratum = sand
      Images: 160
      Taxa: 40
      Individuals: 233
      Seabed area: 1167.66 m²
      Other metadata: Substratum, Habitat, Latitude, Longitude
    Code
      summary(sand)
    Output
      Summary of the data
      Data: 160 images, stratum Substratum = sand
      Data citation: No Data Citation
      
      Units: 160 images
        No individuals: 50 (31%)
        Individuals per image: mean 1.46, 0 to 12
        Seabed area per image: mean 7.3 m², 5.65 to 8.83 m²
      Taxa: 28 seen, of 40 in the data
        Seen once (singletons): 8
        Seen twice (doubletons): 10
      
      Most common taxa
          taxon individuals share
       taxon_02          57 24.5%
       taxon_04          51 21.9%
       taxon_06          25 10.7%
       taxon_01          17  7.3%
       taxon_03          16  6.9%
      
      Diversity indices for units with no individuals are as vegan gives them
      (Shannon 0, Simpson 1, inverse Simpson Inf).

# metrics for single images

    Code
      m <- su_metrics(sand, metrics)
    Condition
      Warning:
      50 of 160 images have no individuals. Diversity indices for these are as vegan gives them (Shannon 0, Simpson 1, inverse Simpson Inf).
    Code
      head(m)
    Output
          FileName richness   density   shannon
      1 image_0441        2 0.2954381 0.6931472
      2 image_0442        0 0.0000000 0.0000000
      3 image_0443        2 0.2545213 0.6931472
      4 image_0444        0 0.0000000 0.0000000
      5 image_0445        2 0.3032130 0.6931472
      6 image_0446        1 0.1207686 0.0000000
    Code
      summary(m)
    Output
      Summary of metrics for survey design
      Data: 160 images, stratum Substratum = sand
      Data citation: No Data Citation
      
         metric n_units   mean     sd    cv zeros left_out
       richness     160 1.1560 1.0850  93.8    50        0
        density     160 0.2014 0.2295 113.9    50        0
        shannon     160 0.2667 0.4182 156.8   109        0
      
      Most variable: shannon (CV 156.8%). It needs the most SUs.

# number of SUs needed

    Code
      sample_size(cv = 25, delta = 40)
    Output
      [1] 12
    Code
      sample_size_table()
    Output
      Sample units required per stratum
      From Durden et al. (2016) Eqn 1 for set CVs, not from survey data
      2 strata compared, alpha 0.05, power 0.95
      Each cell is the number of SUs needed for that CV and delta.
      
           delta
      CV    10% 14% 20% 28% 40% 56% 79% 112% 158% 224%
        5%    8   5   4   3   3   3   3    3    3    3
        10%  28  15   8   5   4   3   3    3    3    3
        15%  60  31  16   9   5   4   3    3    3    3
        20% 106  55  28  15   8   5   4    3    3    3
        25% 164  84  42  22  12   7   4    3    3    3
        30% 235 121  60  31  16   9   6    4    3    3
        35% 320 164  81  42  22  12   7    4    3    3
        40% 417 214 106  55  28  15   8    5    4    3
        45% 528 270 133  69  34  18  10    6    4    3
        50% 651 333 164  84  42  22  12    7    4    3
    Code
      sample_size_data(sand, delta = 40, metrics = metrics)
    Condition
      Warning:
      50 of 160 images have no individuals. Diversity indices for these are as vegan gives them (Shannon 0, Simpson 1, inverse Simpson Inf).
    Output
      Sample units required per stratum, from survey data
      Data: 160 images, stratum Substratum = sand
      Data citation: No Data Citation
      7 strata compared (from the data), alpha 0.05, power 0.95
      
         metric n_units   mean     sd    cv n_delta_40
       richness     160 1.1560 1.0850  93.8        144
        density     160 0.2014 0.2295 113.9        212
        shannon     160 0.2667 0.4182 156.8        400
      
      Worst case: shannon (CV 156.8%). Design the survey for this metric.

# composite SUs with a fixed seed

    Code
      sus
    Output
      su_composite
      Data citation: No Data Citation
      Stratum: Substratum = sand
      Composite SUs: 7
      Built by: area, at least 150 m² each
      Images used: 147, 13 left over and dropped
      Images per SU: 21
      Taxa: 40
      Individuals: 211
      Seabed area: 1072.59 m²
    Code
      summary(sus)
    Output
      Summary of the data
      Data: 7 composite SUs, stratum Substratum = sand
      Data citation: No Data Citation
      
      Units: 7 composite SUs
        Images per SU: 21
        Images used: 147, 13 left over and dropped
        No individuals: 0 (0%)
        Individuals per SU: mean 30.1, 23 to 35
        Seabed area per SU: mean 153 m², 150 to 155 m²
      Taxa: 28 seen, of 40 in the data
        Seen once (singletons): 8
        Seen twice (doubletons): 10
      
      Most common taxa
          taxon individuals share
       taxon_02          52 24.6%
       taxon_04          45 21.3%
       taxon_06          22 10.4%
       taxon_01          16  7.6%
       taxon_03          13  6.2%
    Code
      sus$meta
    Output
          SU Area_seabed_m2 n_images
      1 SU_1       150.4364       21
      2 SU_2       155.1670       21
      3 SU_3       155.1992       21
      4 SU_4       150.9861       21
      5 SU_5       154.4354       21
      6 SU_6       155.3135       21
      7 SU_7       151.0549       21
    Code
      head(sus$members)
    Output
          SU   FileName
      1 SU_1 image_0508
      2 SU_1 image_0569
      3 SU_1 image_0483
      4 SU_1 image_0454
      5 SU_1 image_0491
      6 SU_1 image_0525
    Code
      summary(su_metrics(sus, metrics))
    Output
      Summary of metrics for survey design
      Data: 7 composite SUs, stratum Substratum = sand
      Data citation: No Data Citation
      
         metric n_units    mean      sd   cv zeros left_out
       richness       7 11.4300 2.07000 18.1     0        0
        density       7  0.1969 0.02877 14.6     0        0
        shannon       7  2.0930 0.25620 12.2     0        0
      
      Most variable: richness (CV 18.1%). It needs the most SUs.
    Code
      sample_size_data(sus, delta = 40, metrics = metrics)
    Output
      Sample units required per stratum, from survey data
      Data: 7 composite SUs, stratum Substratum = sand
      Data citation: No Data Citation
      7 strata compared (from the data), alpha 0.05, power 0.95
      
         metric n_units    mean      sd   cv n_delta_40
       richness       7 11.4300 2.07000 18.1          6
        density       7  0.1969 0.02877 14.6          4
        shannon       7  2.0930 0.25620 12.2          3
      
      Worst case: richness (CV 18.1%). Design the survey for this metric.

# SU size curves with a fixed seed

    Code
      su_curve(sand, metrics, times = 100, steps = c(1, 5, 10, 20, 50, 100), seed = 1)
    Condition
      Warning:
      In 38 of 600 steps the images drawn so far had no individuals. Diversity indices there are as vegan gives them (Shannon 0, Simpson 1, inverse Simpson Inf).
    Output
      Sample unit size curves
      Data: 160 images, stratum Substratum = sand
      Data citation: No Data Citation
      100 random orders of images, with replacement. Median and middle 95% shown.
      
      richness
       images    area individuals median lower upper
            1   7.209         1.0    1.0  0.00  4.00
            5  36.800         6.0    4.0  2.00  8.00
           10  73.450        13.5    7.0  4.00 11.00
           20 146.200        29.0   10.5  7.00 15.00
           50 365.300        73.0   16.0 12.00 20.52
          100 730.400       144.0   21.0 16.95 25.00
      
      density
       images    area individuals median   lower  upper
            1   7.209         1.0 0.1356 0.00000 0.8139
            5  36.800         6.0 0.1697 0.05713 0.5254
           10  73.450        13.5 0.1853 0.09308 0.4204
           20 146.200        29.0 0.1999 0.12200 0.3155
           50 365.300        73.0 0.2005 0.14300 0.2817
          100 730.400       144.0 0.1975 0.16170 0.2440
      
      shannon
       images    area individuals median  lower upper
            1   7.209         1.0  0.000 0.0000 1.361
            5  36.800         6.0  1.346 0.5651 2.043
           10  73.450        13.5  1.809 1.0880 2.274
           20 146.200        29.0  2.039 1.5110 2.454
           50 365.300        73.0  2.256 1.8950 2.524
          100 730.400       144.0  2.336 2.1050 2.553

