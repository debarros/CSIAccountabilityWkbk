# CreateCompareMatrix.R
CreateCompareMatrix = function(studentlist, Exams, matrix1, matrix2){
  n = length(studentlist)
  nExam = length(Exams)
  CompareMatrix = matrix(integer(0), n, nExam)       # set up a matrix that will hold the best scores
  rownames(CompareMatrix) = studentlist              # in the matrix, name the rows according to the student ID
  colnames(CompareMatrix) = Exams                    # in the matrix, name the columns according to the exam or category name
  CompArray = abind(matrix1, matrix2, along = 3)     # bind the two matrices of scores into a 3-d array
  CompareMatrix = MbetterMax(CompArray)              # for each student/exam intersection, pick the better of the two scores
  return(CompareMatrix)
}

