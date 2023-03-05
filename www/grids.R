rank <- c("Professor", "Associate Professor", "Assistant Professor", "Lecturer", 
           "Senior Instructor", "Instructor II", "Instructor I", "Librarian",
           "Associate Librarian", "Assistant Librarian", "General Librarian")

floor1 <- c(120684, 98285, 83734, 66835, 98285, 83734, 75000,
            110779, 888518, 76797, 66679)
increment1 <- c(4827, 3931, 3349, 2673, 3931, 3349, 3000, 4431, 3541,
                3072, 2667)
maximum1 <- c(168957, 137599, 117227, 93569, 137599, 117227, 10500,
              155090, 123925, 107516, 93350)

floor2 <- c(123398.95, 100496.41, 85617.58, 68338.79, 100496.41, 85617.58, 76687.50,
            113271.09, 90509.51, 78525.08, 68178.84)
increment2 <- c(4936, 4019, 3424, 2734, 4020, 3425, 3068, 4531, 3620, 3141, 2727)
maximum2 <- c(172758.53, 140694.98, 119864.61, 95674.30, 140694.98, 119864.61, 
              107362.50, 158579.53, 126713.31, 109935.11, 95459.38)

floor3 <-c(126175.43, 102757.58, 87543.97, 69876.41, 102757.58, 87543.97, 78412.97,
            115819.69, 92545.97, 80291.89, 69712.86)
increment3 <- c(5047, 4110, 3502, 2795, 4110, 3502, 3137, 4633, 3702, 3212, 2789)
maximum3 <- c(176645.60, 143860.61, 122561.56, 97826.97, 143860.61, 122561.56,
              109778.16, 162147.56, 129564.36, 112408.65, 97598.01)

grid2021 <- data.frame(rank, floor = floor1, increment = increment1, maximum = maximum1)
grid2022 <- data.frame(rank, floor = floor2, increment = increment2, maximum = maximum2)
grid2023 <- data.frame(rank, floor = floor3, increment = increment3, maximum = maximum3)