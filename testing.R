# Set parameters examples
p_area <- "E10000015"
inds <- c(90366, 108)
c_areas <- nearest_neighbours(p_area, 102, "CIPFA")
district <- c("E07000095","E07000096","E07000098","E07000099","E07000102","E07000103","E07000240",
              "E07000241","E07000242","E07000243")

# Fetch data
data1 <- fingertips_data(inds, AreaTypeID = 102)
data2 <- fingertips_data(inds, AreaCode = district, AreaTypeID = 101, ParentAreaTypeID = 102)