#문제1
lovehufs = seq(5, 30, by=5)
lovehufs

#문제2
a = c(1,2,3)
b = c(11, 12, 13)
c = a + b
d = a * b
#문제2_결과물
a
b
c
d

#문제3
a1=c(45, 67, 89)
a2=c(12, 34, 56)
a3=c(111, 222, 333)
a4 =rbind(a1, a2, a3)
#문제3_결과물
a4 

#문제4
name=list(c("사과", "바나나", "오렌지"),
          c("apple", "banana", "orange"))
a4=matrix(a4, nrow=3, dimnames = name )
#문제4_결과물
a4

#문제5
name=list(c('R1','R2','R3'),
          c('C10','C20','C30','C40'))
temp=c(101, 102, 103, 104, 105,106,107, 108, 109, 110, 111, 112)
ABC=array(temp, dim=c(3,4), dimnames = name)
#문제5_결과물
ABC

#문제6 & 결과물
ABC[1,]

#문제7
L1=list(c(12,145,200), c('김철수'), c('GBT','영통'), c(5))
#문제7_결과물
L1

#문제8
L1=list("class"=c(12,145,200), "name"=c('김철수'), "major"=c('GBT','영통'), "gpa"=c(5))
#문제8_결과물
L1

#문제9 & 결과물
L1$class
L1["class"]
L1[["class"]]

#문제10
work = read.csv("C:/Users/Jaeyun/Desktop/소셜콘텐츠/R_Project/assignment1/IRIS.csv")
#문제10_결과물
work


#문제11 & 결과물
head(work, n=12)

#문제12 & 결과물
work[121,]

#문제13
work[135,] #변경 전 135행 전체 값 확인
work[135, "petal.length"] #변경 전 petal.length열 값 확인용
work[135, "petal.length"] = 10
#문제13_결과물
work[135, "petal.length"]
