CREATE TABLE Student(
	SNO INT PRIMARY KEY,
	SNAME CHAR(8) UNIQUE,
	SEX CHAR(2),
	DEPTNO INT
);
CREATE TABLE Course(
	CNO INT,
	CNAME CHAR(20) NOT NULL,
	TNO INT,
	CREDIT	INT,
	PRIMARY KEY (CNO, TNO)
);
CREATE TABLE SC(
	SNO INT,
	CNO INT,
	GRADE INT,
	PRIMARY KEY (SNO, CNO)
);
CREATE TABLE Teacher(
	TNO INT PRIMARY KEY,
	TNAME CHAR(8) NOT NULL,
	DEPTNO INT
);
CREATE TABLE Dept(
	DEPTNO INT PRIMARY KEY,
	DNAME CHAR(20) NOT NULL
);
ALTER TABLE Student ADD AGE INT;
DROP TABLE Student;
DROP TABLE Course;
DROP TABLE SC;
DROP TABLE Teacher;
DROP TABLE Dept;

CREATE UNIQUE INDEX stusno ON Student(SNO);
CREATE UNIQUE INDEX coucno ON Course(CNO);
DROP INDEX stusno;
DROP INDEX coucno;

CREATE VIEW CS_STUDENT AS SELECT *
	FROM Student
	WHERE DEPTNO =
			(SELECT DEPTNO
				FROM Dept
				WHERE DNAME = '计算机科学与技术')
	WITH CHECK OPTION;

DROP VIEW CS_STUDENT;

INSERT INTO Student VALUES(1001, '张天', '男', 20, 10);
INSERT INTO Student VALUES(1002, '李兰', '女', 21, 10);
INSERT INTO Student VALUES(1003, '陈铭', '男', 21, 10);
INSERT INTO Student VALUES(1004, '李茜', '女', 21, 20);
INSERT INTO Student VALUES(1005, '马朝阳', '男', 22, 20);

INSERT INTO Course VALUES(1, '数据结构', 101, 4);
INSERT INTO Course VALUES(2, '数据库', 102, 4);
INSERT INTO Course VALUES(3, '离散数学', 103, 4);
INSERT INTO Course VALUES(4, 'C语言程序设计', 101, 2);
INSERT INTO Course VALUES (1,'数据结构', 101, 4);
INSERT INTO Course VALUES (2,'数据库', 102, 4);
INSERT INTO Course VALUES (3,'离散数学', 103, 4);
INSERT INTO Course VALUES (4,'C语言程序设计', 101, 2);

INSERT INTO SC VALUES (1001,1,80);
INSERT INTO SC VALUES (1001,2,85);
INSERT INTO SC VALUES (1001,3,78);
INSERT INTO SC VALUES (1002,1,78);
INSERT INTO SC VALUES (1002,2,82);
INSERT INTO SC VALUES (1002,3,86);
INSERT INTO SC VALUES (1003,1,82);
INSERT INTO SC VALUES (1003,3,90);
INSERT INTO SC VALUES (1004,1,87);
INSERT INTO SC VALUES (1004,4,90);
INSERT INTO SC VALUES (1005,1,85);
INSERT INTO SC VALUES (1005,4,92);

INSERT INTO Teacher VALUES (101,'张星', 10);
INSERT INTO Teacher VALUES (102,'李珊', 10);
INSERT INTO Teacher VALUES (103,'赵天应', 10);
INSERT INTO Teacher VALUES (104,'刘田', 20);

INSERT INTO Dept VALUES (10,'计算机科学与技术');
INSERT INTO Dept VALUES (20,'信息');

/*删除数据*/
DELETE FROM SC WHERE SNO IN 
	(SELECT SNO
	FROM Student
	WHERE SNAME = '马朝阳'
)；
