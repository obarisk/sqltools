require(sqltools)
require(tibble)
require(Lahman)
context("check insert query string generator")

Pitching <- Pitching

dT <- iris[1:2, ] %>% 
  cbind(Date=as.Date('2017/01/01')) %>%
  cbind(Posixct=as.POSIXct('2017/01/01 14:00:00'))

dU <- dT
dU[2, ] <- NA

test_that("MySQL insert query string", {
  # simpliest case
  expect_equal(
    my_qsg_fromdf(dT, 'iris'), 
    tibble(
      QueryString=gsub(" {1,}", " ", 
        gsub("\n", " ",
          "INSERT INTO `iris` 
          (`Sepal.Length`,`Sepal.Width`,`Petal.Length`,`Petal.Width`,`Species`,`Date`,`Posixct`) 
          VALUES 
          (5.1,3.5,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00'),(4.9,3.0,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00')"
          )
        )
      )
    )
  # with timezone
  expect_equal(
    my_qsg_fromdf(dT, 'iris', tz='US/Eastern'), 
    tibble(
      QueryString=gsub(" {1,}", " ", 
        gsub("\n", " ",
          "INSERT INTO `iris` 
          (`Sepal.Length`,`Sepal.Width`,`Petal.Length`,`Petal.Width`,`Species`,`Date`,`Posixct`) 
          VALUES 
          (5.1,3.5,1.4,0.2,'setosa','2017/01/01','2017/01/01 01:00:00'),(4.9,3.0,1.4,0.2,'setosa','2017/01/01','2017/01/01 01:00:00')"
          )
        )
      )
    )
  # with null
  expect_equal(
    my_qsg_fromdf(dU, 'iris'), 
    tibble(
      QueryString=gsub(" {1,}", " ", 
        gsub("\n", " ",
          "INSERT INTO `iris` 
          (`Sepal.Length`,`Sepal.Width`,`Petal.Length`,`Petal.Width`,`Species`,`Date`,`Posixct`) 
          VALUES 
          (5.1,3.5,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00'),(NULL,NULL,NULL,NULL,NULL,NULL,NULL)"
          )
        )
      )
    )
  # iLength = 1
  expect_equal(
    my_qsg_fromdf(dT, 'iris', iLength=1), 
    tibble(
      QueryString=gsub(" {1,}", " ", 
        gsub("\n", " ", c(
            "INSERT INTO `iris`
            (`Sepal.Length`,`Sepal.Width`,`Petal.Length`,`Petal.Width`,`Species`,`Date`,`Posixct`) VALUES
            (5.1,3.5,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00')",
            "INSERT INTO `iris`
            (`Sepal.Length`,`Sepal.Width`,`Petal.Length`,`Petal.Width`,`Species`,`Date`,`Posixct`) VALUES
            (4.9,3.0,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00')"
            )
          )
        )
      )
    )
  # tablename prefix with quote
  expect_equal(
    my_qsg_fromdf(dT, '`iris`.iris'), 
    tibble(
      QueryString=gsub(" {1,}", " ", 
        gsub("\n", " ",
          "INSERT INTO `iris`.iris 
          (`Sepal.Length`,`Sepal.Width`,`Petal.Length`,`Petal.Width`,`Species`,`Date`,`Posixct`) 
          VALUES 
          (5.1,3.5,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00'),(4.9,3.0,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00')"
          )
        )
      )
    )
  # tablename prefix without quote
  expect_equal(
    my_qsg_fromdf(dT, 'iris.iris'), 
    tibble(
      QueryString=gsub(" {1,}", " ", 
        gsub("\n", " ",
          "INSERT INTO `iris`.`iris` 
          (`Sepal.Length`,`Sepal.Width`,`Petal.Length`,`Petal.Width`,`Species`,`Date`,`Posixct`) 
          VALUES 
          (5.1,3.5,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00'),(4.9,3.0,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00')"
          )
        )
      )
    )
})

test_that("MSSQL insert query string without column header", {
  # simpliest case
  expect_equal(
    ms_qsg_fromdf(dT, 'iris'), 
    tibble(
      QueryString=gsub(" {1,}", " ", 
        gsub("\n", " ",
          "INSERT INTO [iris] 
          VALUES 
          (5.1,3.5,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00'),(4.9,3.0,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00')"
          )
        )
      )
    )
  # with timezone
  expect_equal(
    ms_qsg_fromdf(dT, 'iris', tz='US/Eastern'), 
    tibble(
      QueryString=gsub(" {1,}", " ", 
        gsub("\n", " ",
          "INSERT INTO [iris] 
          VALUES 
          (5.1,3.5,1.4,0.2,'setosa','2017/01/01','2017/01/01 01:00:00'),(4.9,3.0,1.4,0.2,'setosa','2017/01/01','2017/01/01 01:00:00')"
          )
        )
      )
    )
  # with null
  expect_equal(
    ms_qsg_fromdf(dU, 'iris'), 
    tibble(
      QueryString=gsub(" {1,}", " ", 
        gsub("\n", " ",
          "INSERT INTO [iris] 
          VALUES 
          (5.1,3.5,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00'),(NULL,NULL,NULL,NULL,NULL,NULL,NULL)"
          )
        )
      )
    )
  # iLength = 1
  expect_equal(
    ms_qsg_fromdf(dT, 'iris', iLength=1), 
    tibble(
      QueryString=gsub(" {1,}", " ", 
        gsub("\n", " ", c(
            "INSERT INTO [iris]
            VALUES
            (5.1,3.5,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00')",
            "INSERT INTO [iris]
            VALUES
            (4.9,3.0,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00')"
            )
          )
        )
      )
    )
  # tablename prefix with quote
  expect_equal(
    ms_qsg_fromdf(dT, 'testdb.dbo.[iris]'), 
    tibble(
      QueryString=gsub(" {1,}", " ", 
        gsub("\n", " ",
          "INSERT INTO testdb.dbo.[iris] 
          VALUES 
          (5.1,3.5,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00'),(4.9,3.0,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00')"
          )
        )
      )
    )
  # tablename prefix without quote
  expect_equal(
    ms_qsg_fromdf(dT, 'testdb..iris'), 
    tibble(
      QueryString=gsub(" {1,}", " ", 
        gsub("\n", " ",
          "INSERT INTO [testdb]..[iris] 
          VALUES 
          (5.1,3.5,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00'),(4.9,3.0,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00')"
          )
        )
      )
    )
  # with colnames
  expect_equal(
    ms_qsg_fromdf(dT, 'iris', Colnames=TRUE), 
    tibble(
      QueryString=gsub(" {1,}", " ", 
        gsub("\n", " ",
          "INSERT INTO [iris] 
          ([Sepal.Length],[Sepal.Width],[Petal.Length],[Petal.Width],[Species],[Date],[Posixct]) 
          VALUES 
          (5.1,3.5,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00'),(4.9,3.0,1.4,0.2,'setosa','2017/01/01','2017/01/01 14:00:00')"
          )
        )
      )
    )
})

test_that("MySQL compatitable support", {
  expect_equal(length(data2insertmysqlqs(Pitching, 'Pitching')), 
    nrow(my_qsg_fromdf(Pitching, 'Pitching')))
  expect_equal(data2insertmysqlqs(Pitching, 'Pitching')[1], 
    unlist(my_qsg_fromdf(Pitching,'Pitching')[1, 1], use.names=FALSE))
})

test_that("MSSQL compatitable support", {
  expect_equal(length(data2insertmssqlqs(Pitching, 'Pitching')), 
    nrow(ms_qsg_fromdf(Pitching, 'Pitching')))
  expect_equal(data2insertmssqlqs(Pitching, 'Pitching')[1], 
    unlist(ms_qsg_fromdf(Pitching,'Pitching')[1, 1], use.names=FALSE))
})

