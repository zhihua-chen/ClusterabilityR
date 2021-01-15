# Examples file to replicate examples and plots from clusterability package

# Copyright (C) 2020  Zachariah Neville, Naomi Brownstein, Andreas Adolfsson, Margareta Ackerman

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

library(clusterability)
library(elasticnet)
# Normals1 - results
data(normals1)
normals1 <- normals1[,-3]
norm1_dippca <- clusterabilitytest(normals1, "dip")
norm1_dipdist <- clusterabilitytest(normals1, "dip",  distance_standardize = "NONE", reduction = "distance")
norm1_silvpca <- clusterabilitytest(normals1, "silverman", s_setseed = 123)
norm1_silvdist <- clusterabilitytest(normals1, "silverman", distance_standardize = "NONE", reduction = "distance", s_setseed = 123)
norm1_dipspca=clusterability::clusterabilitytest(normals1, "dip", reduction = "spca")
norm1_silvspca=clusterability::clusterabilitytest(normals1, "silverman", reduction = "spca")



print(norm1_dippca)
print(norm1_dipdist)
print(norm1_silvpca)
print(norm1_silvdist)
print(norm1_dipspca)
print(norm1_silvspca)


# Normals2 - results
data(normals2)
normals2 <- normals2[,-3]
norm2_dippca <- clusterabilitytest(normals2, "dip")
norm2_dipdist <- clusterabilitytest(normals2, "dip", reduction = "distance", distance_standardize = "NONE")
norm2_silvpca <- clusterabilitytest(normals2, "silverman", s_setseed = 123)
norm2_silvdist <- clusterabilitytest(normals2, "silverman", reduction = "distance", distance_standardize = "NONE", s_setseed = 123)
norm2_dipspca <- clusterabilitytest(normals2, "dip", reduction="spca")
norm2_silvspca <- clusterabilitytest(normals2, "silverman", reduction="spca",s_setseed = 123)


print(norm2_dippca)
print(norm2_dipdist)
print(norm2_silvpca)
print(norm2_silvdist)
print(norm2_dipspca)
print(norm2_silvspca)



# Normals3 - results
data(normals3)
normals3 <- normals3[,-3]
norm3_dippca <- clusterabilitytest(normals3, "dip")
norm3_dipdist <- clusterabilitytest(normals3, "dip", reduction = "distance", distance_standardize = "NONE")
norm3_silvpca <- clusterabilitytest(normals3, "silverman", s_setseed = 123)
norm3_silvdist <- clusterabilitytest(normals3, "silverman", reduction = "distance", distance_standardize = "NONE", s_setseed = 123)
norm3_dipspca <- clusterabilitytest(normals3, "dip", reduction="spca")
norm3_silvspca <- clusterabilitytest(normals3, "silverman", reduction="spca",s_setseed = 123)




print(norm3_dippca)
print(norm3_dipdist)
print(norm3_silvpca)
print(norm3_silvdist)
print(norm3_dipspca)
print(norm3_silvspca)


# Normals4 - results
data(normals4)
normals4 <- normals4[,-4]
norm4_dippca <- clusterabilitytest(normals4, "dip")
norm4_dipdist <- clusterabilitytest(normals4, "dip", reduction = "distance", distance_standardize = "NONE")
norm4_silvpca <- clusterabilitytest(normals4, "silverman", s_setseed = 123)
norm4_silvdist <- clusterabilitytest(normals4, "silverman", reduction = "distance", distance_standardize = "NONE", s_setseed = 123)
norm4_dipspca <- clusterabilitytest(normals4, "dip", reduction="spca")
norm4_silvspca <- clusterabilitytest(normals4, "silverman", reduction="spca", s_setseed = 123)

print(norm4_dippca)
print(norm4_dipdist)
print(norm4_silvpca)
print(norm4_silvdist)
print(norm4_dipspca)
print(norm4_silvspca)


# Normals5 - results
data(normals5)
normals5 <- normals5[,-4]
norm5_dippca <- clusterabilitytest(normals5, "dip")
norm5_dipdist <- clusterabilitytest(normals5, "dip", reduction = "distance", distance_standardize = "NONE")
norm5_silvpca <- clusterabilitytest(normals5, "silverman", s_setseed = 123, s_digits = 20)
norm5_silvdist <- clusterabilitytest(normals5, "silverman", reduction = "distance", distance_standardize = "NONE", s_setseed = 123)
norm5_dipspca <- clusterabilitytest(normals5, "dip", reduction="spca")
norm5_silvspca <- clusterabilitytest(normals5, "silverman", reduction="spca",s_setseed = 123)

print(norm5_dippca)
print(norm5_dipdist)
print(norm5_silvpca)
print(norm5_silvdist)
print(norm5_dipspca)
print(norm5_silvspca)


# cars - results
data(cars)

cars_dippca <- clusterabilitytest(cars, "dip")
cars_dipspca <- clusterabilitytest(cars, "dip",reduction="spca")
cars_dipdist <- clusterabilitytest(cars, "dip", reduction = "distance", distance_standardize = "NONE")
cars_silvpca <- clusterabilitytest(cars, "silverman", s_setseed = 123)
cars_silvdist <- clusterabilitytest(cars, "silverman", reduction = "distance", distance_standardize = "NONE", s_setseed = 123)
cars_silvspca <- clusterabilitytest(cars, "silverman", reduction="spca", s_setseed = 123)

print(cars_dippca)
print(cars_dipdist)
print(cars_dipspca)
print(cars_silvpca)
print(cars_silvdist)
print(cars_silvspca)



# iris - results
data(iris)
newiris <- iris[,c(1:4)]
iris_dippca <- clusterabilitytest(newiris, "dip")
iris_dipspca <- clusterabilitytest(newiris, "dip",reduction="spca")
iris_dipdist <- clusterabilitytest(newiris, "dip", reduction = "distance", distance_standardize = "NONE")
iris_silvpca <- clusterabilitytest(newiris, "silverman", s_setseed = 123)
iris_silvdist <- clusterabilitytest(newiris, "silverman", reduction = "distance", distance_standardize = "NONE", s_setseed = 123)
iris_silvspca <- clusterabilitytest(newiris, "silverman", reduction="spca", s_setseed = 123)

print(iris_dippca)
print(iris_dipdist)
print(iris_dipspca)
print(iris_silvpca)
print(iris_silvspca)
print(iris_silvdist)
