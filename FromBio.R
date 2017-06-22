# FromBio.R

# Note: run Mainscript first

currentTerm = 2600

bioCourses = c("Biology 1 credit", "Biology Honors 1 credit", "Biology")
bioBcourses = c("Biology Part B", "Biology B")
alg1Courses = c("Algebra B", "Algebra I", "Algebra", "Integrated Algebra ", "Integrated Algebra I- 1 credit", 
                "Integrated Algebra I Honors 1 credit", "Integrated Algebra I", "Integrated Algebra")
pastAlg1Courses = c("Algebra II /Trig Honors- 1 credit", "Algebra II/ Trig", "Algebra II/Trig- 1 credit",
                    "Algebra II/Trig", "APPLIED GEOMETRY R", "Geometry 1 credit", "Geometry A", "Geometry B",
                    "Geometry Honors", "Geometry Plato", "Geometry", "Hudson Valley Calculus", "Hudson Valley Community College Math 110",
                    "HVCC- Precalculus", "Integrated Geometry", "Intermediate Algebra 10", "Intermediate Geometry 10", 
                    "Intermediate Trig", "Intermediate Trigonometry", "INTRO GEOMETRY 2R", "Intro. Geometry 1R",
                    "Introduction to Algebra II", "Introduction to Calculus", "Pre-Calculus 1-credit",
                    "Pre-Calculus Honors 1 credit", "Topics in Math/ Finite Math", "Trig")

# Read in June Bio regents scores exported from ASAP
# Note: if only the June Bio regents scores are used, then students who scored higher on an earlier exam may be placed incorrectly
asapBio = read.xlsx(xlsxFile = "asapExports.xlsx", sheet = "Bio")
str(asapBio)


# Get all the bio enrollments and add bio regents scores
bioStudents = F2[F2$Course_Name %in% c(bioCourses, bioBcourses) & F2$TermID == currentTerm,]
length(unique(bioStudents$`[1]Student_Number`))
letterGrades = unique(bioStudents$Grade)
passingGrades = setdiff(letterGrades, "F")

bioStudents$PriorbioRegents = powerschool$Regents_Living_Environment_Score[match(x = bioStudents$`[1]Student_Number`, table = powerschool$student_number)]
bioStudents$NewbioRegents = asapBio$ScaledScore_1[match(x = bioStudents$`[1]Student_Number`, table = asapBio$StudentID_1)]
bioStudents$BestbioRegents = VbetterMax(bioStudents$PriorbioRegents, bioStudents$NewbioRegents)
summary(bioStudents$BestbioRegents)

bioStudents$passedBioRegents = FALSE
bioStudents$passedBioRegents[bioStudents$BestbioRegents > 64] = TRUE
bioStudents$passedBioRegents[is.na(bioStudents$passedBioRegents)] = FALSE

# Add info about math
bioStudents$passedAlgebra = FALSE
for(i in 1:nrow(bioStudents)){
  curStu = bioStudents$`[1]Student_Number`[i]
  mathpasses = F2[F2$`[1]Student_Number` == curStu & F2$Course_Name %in% c(pastAlg1Courses, alg1Courses) & F2$Grade %in% passingGrades,]
  if(nrow(mathpasses) > 0){
    bioStudents$passedAlgebra[i] = TRUE
  }
}

# Determine future courses
bioStudents$nextCourse = NA_character_


# Bio R and H students
for(i in 1:nrow(bioStudents)){
  if(bioStudents$Course_Name[i] %in% bioCourses){ # if took bio R or H
    if(bioStudents$Grade[i] %in% passingGrades){ # if passed bio
      if(bioStudents$passedBioRegents[i]){ # if passed regents
        if(bioStudents$passedAlgebra[i]){ # if passed algebra
          if(bioStudents$BestbioRegents[i] < 80){ # if bio regents less than 80
            bioStudents$nextCourse[i] = "Earth"
          } else { # if bio regents 80 or higher
            bioStudents$nextCourse[i] = "Chem"
          }
        } else { # hasn't passed algebra
          bioStudents$nextCourse[i] = "Earth"
        }
      } else {
        # hasn't passed bio regents
        bioStudents$nextCourse[i] = "Bio B"
      }
    } else {
      #failed bio
      bioStudents$nextCourse[i] = "Bio R"
    }
  } else {  # if took bio b
    if(bioStudents$passedAlgebra[i]){ # if passed algebra
      if(is.na(bioStudents$BestbioRegents[i])){ # if didn't take bio regents
        bioStudents$nextCourse[i] = "Earth"
      } else if(bioStudents$BestbioRegents[i] < 80){ # if bio regents less than 80
        bioStudents$nextCourse[i] = "Earth"
      } else { # if bio regents 80 or higher
        bioStudents$nextCourse[i] = "Chem"
      }
    } else { # hasn't passed algebra
      bioStudents$nextCourse[i] = "Earth"
    }
  }
}


summary(as.factor(bioStudents$nextCourse))

write.csv(x = bioStudents, file = "bioStudentCourseAssignments.csv")
